// Copyright (c) 2009 Nick Gerakines <nick@gerakines.net>
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
// 
// This program was based on the example provided by Richard Jones, http://www.metabrew.com/
// http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-3/
#include <sys/types.h>
#include <sys/time.h>
#include <sys/queue.h>
#include <stdlib.h>
#include <err.h>
#include <string.h>
#include <event.h>
#include <evhttp.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <pthread.h>
#include "erl_interface.h"
#include "ei.h"

#include <signal.h>

extern const char *erl_thisnodename(void); 
extern short erl_thiscreation(void); 
#define SELF(fd) erl_mk_pid(erl_thisnodename(),fd,0,erl_thiscreation())

#define BUFSIZE 1024
#define MAXUSERS (65536) //!< Max number of connections to be handled concurrently

int fd;
struct evhttp_request * clients[MAXUSERS+1];
int cuid;
pthread_mutex_t clients_mutex;
pthread_mutex_t cuid_mutex;

void request_handler(struct evhttp_request *req, void *arg) {
    pthread_mutex_lock(&cuid_mutex);
    if (cuid == MAXUSERS-1) { cuid = 0; }
    int mycuid = cuid++;
    pthread_mutex_unlock(&cuid_mutex);

    u_char *data = EVBUFFER_DATA(req->input_buffer);
    size_t len = EVBUFFER_LENGTH(req->input_buffer);
    char *body;
    if ((body = malloc(len + 1)) == NULL) {
          evbuffer_drain(req->input_buffer, len);
    }

    memcpy(body, data, len);
    body[len] = '\0';
    evbuffer_drain(req->input_buffer, len + 1);

    ETERM *harray[50]; // Wasted space if there are less than 50 headers
    int hcount = 0;
    struct evkeyval *header; // XXX Not being released.
    TAILQ_FOREACH(header, req->input_headers, next) {
        if (hcount == 50) {
            ETERM *harr[2];
            harr[0] = erl_mk_string((const char *) header->key);
            harr[1] = erl_mk_string((const char *) header->value);
            harray[hcount] = erl_mk_tuple(harr, 2);
            hcount++;
        }
    }

    // {pid(), int(), int(), string(), list(tuple()), string()}
    ETERM *arr[6], *emsg2;
    arr[0] = SELF(fd);
    arr[1] = erl_mk_int(mycuid);
    arr[2] = erl_mk_int(req->type);
    arr[3] = erl_mk_binary(req->uri, sizeof(req->uri));
    arr[4] = erl_mk_list(harray, hcount);
    arr[5] = erl_mk_binary(body, len + 1);
    emsg2 = erl_mk_tuple(arr, 6);

    pthread_mutex_lock(&clients_mutex);
    clients[mycuid] = req;
    pthread_mutex_unlock(&clients_mutex);

    erl_reg_send(fd, "mochevent_handler", emsg2);
    erl_free_compound(emsg2);
    free(body);

    int timeout = 3 * CLOCKS_PER_SEC;
    long time_taken = clock();

    // NKG: There might be a better way to do this.
    while (clients[mycuid]) {
        if (clock() - time_taken > timeout) {
            struct evbuffer *buf;
            buf = evbuffer_new();
            evbuffer_add_printf(buf, "Took tooo long.");
            evhttp_send_reply(req, HTTP_SERVUNAVAIL, "", buf);
            evbuffer_free(buf);
            
            pthread_mutex_lock(&clients_mutex);
            clients[mycuid] = NULL;
            pthread_mutex_unlock(&clients_mutex);
            break;
        }
    }
}

void cnode_run() {
    int got;
    unsigned char *buf;
    buf = malloc(BUFSIZE + 1);
    ErlMessage emsg;
    ETERM *reqidr, *coder, *respheadersr, *respbodyr;

    erl_init(NULL, 0);
    if (erl_connect_init(1, "secretcookie", 0) == -1) {
        erl_err_quit("erl_connect_init");
    }
    if ((fd = erl_connect("httpdmaster@localhost")) < 0) {
        erl_err_quit("erl_connect");
    }

    struct evbuffer *evbuf = evbuffer_new();

    while (1) {
        got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
        if (got == ERL_TICK) {
            continue;
        } else if (got == ERL_ERROR) {
            break;
        } else {
            if (emsg.type == ERL_SEND) {
                reqidr = erl_element(1, emsg.msg);
                coder = erl_element(2, emsg.msg);
                respheadersr = erl_element(3, emsg.msg);
                respbodyr = erl_element(4, emsg.msg);

                int reqid = ERL_INT_VALUE(reqidr);
                int code = ERL_INT_VALUE(coder);
                char *body = (char *) ERL_BIN_PTR(respbodyr);
                int body_len = ERL_BIN_SIZE(respbodyr);
                if (clients[reqid]) {

                    if (ERL_IS_LIST(respheadersr)) {
                        ETERM *list;
                        for (list = respheadersr; ! ERL_IS_EMPTY_LIST(list); list = ERL_CONS_TAIL(list)) {
                            ETERM *item = ERL_CONS_HEAD(list);
                            ETERM *keyr = erl_element(1, item);
                            ETERM *valuer = erl_element(2, item);
                            int key_len = ERL_BIN_SIZE(keyr);
                            char *key;
                            key = malloc((key_len + 1) * sizeof(char));
                            memcpy(key, (char *) ERL_BIN_PTR(keyr), key_len);
                            key[key_len + 1] = '\0';

                            int value_len = ERL_BIN_SIZE(valuer);
                            char *value;
                            value = malloc((value_len + 1) * sizeof(char));
                            memcpy(key, (char *) ERL_BIN_PTR(valuer), value_len);
                            value[value_len + 1] = '\0';

                            evhttp_add_header(clients[reqid]->output_headers, key, value);
                            free(key);
                            free(value);
                            erl_free_term(item);
                            erl_free_term(keyr);
                            erl_free_term(valuer);
                        }
                        erl_free_term(list);
                    }

                    evbuffer_add(evbuf, (const void*) body, (size_t) body_len);
                    evhttp_send_reply(clients[reqid], code, "OK", evbuf);
                    pthread_mutex_lock(&clients_mutex);
                    clients[reqid] = NULL;
                    pthread_mutex_unlock(&clients_mutex);
                }
                erl_free_term(emsg.msg);
                erl_free_term(emsg.to);
                erl_free_term(reqidr);
                erl_free_term(coder);
                erl_free_term(respheadersr);
                erl_free_term(respbodyr);0
            }
        }
    }
    pthread_exit(0);
}

int main(int argc, char **argv) {
    cuid = 1;
    pthread_t helper;
    pthread_create(&helper, NULL, (void *) cnode_run, NULL);

    event_init();
    struct evhttp *httpd = evhttp_start("0.0.0.0", 8000);
    evhttp_set_gencb(httpd, request_handler, NULL);
    event_dispatch();
    evhttp_free(httpd);

    return 0;
}
