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
#define MAXUSERS (17*65536) //!< Max number of connections to be handled concurrently

struct evhttp_request * clients[MAXUSERS+1];
int fd;
int cuid;

//! The request handler set to the libevent http callback.
void request_handler(struct evhttp_request *req, void *arg) {
    struct evbuffer *evbuf = evbuffer_new();
    if (evbuf == NULL) {
        err(1, "failed to create response buffer");
    }
    char *line;
    char body[1024];
    body[0] = '\0';
    while ((line = evbuffer_readline(req->input_buffer)) != NULL) {
        strcat(body, line);
    }
    
    ETERM *harray[50];
    int hcount = 0;
    struct evkeyval *header;
    TAILQ_FOREACH(header, req->input_headers, next) {
        ETERM *harr[2];
        harr[0] = erl_mk_string((const char *) header->key);
        harr[1] = erl_mk_string((const char *) header->value);
        harray[hcount] = erl_mk_tuple(harr, 2);
        hcount++;
    }

    // {pid(), int(), int(), string(), list(tuple()), string()}
    ETERM *arr[6], *emsg2;
    arr[0] = SELF(fd);
    arr[1] = erl_mk_int(cuid);
    arr[2] = erl_mk_int(req->type);
    arr[3] = erl_mk_string((const char *) evhttp_request_uri(req));
    arr[4] = erl_mk_list(harray, hcount);
    arr[5] = erl_mk_string(body);
    emsg2 = erl_mk_tuple(arr, 6);
    clients[cuid] = req;
    erl_reg_send(fd, "mochevent_handler", emsg2);
    // Add some sort of time-out so requests are given up on after n seconds.
    while (clients[cuid]) { }

    // erl_free_array(harray, hcount);
    // erl_free_array(arr, 6);
    // erl_free_term(emsg2);
    evbuffer_free(evbuf);
}

/* A function executed in a pthread that will create a cnode connect to the
local httpdmaster node to dispatch requests. The request loop will assume that
all incoming messages are contain request bodies for existing requests stored
in the clients array. When that is the case the reply to that request is
crafted and sent.
*/
void cnode_run() {
    int got;
    unsigned char buf[BUFSIZE];
    ErlMessage emsg;
    ETERM *reqidr, *coder, *respheadersr, *respbodyr;

    erl_init(NULL, 0);
    if (erl_connect_init(1, "secretcookie", 0) == -1) {
        erl_err_quit("erl_connect_init");
    }
    if ((fd = erl_connect("httpdmaster@localhost")) < 0) {
        erl_err_quit("erl_connect");
    }

    while (1) {
        got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
        if (got == ERL_TICK) {
            continue;
        } else if (got == ERL_ERROR) {
            fprintf(stderr, "ERL_ERROR from erl_receive_msg.\n");
            break;
        } else {
            if (emsg.type == ERL_SEND) {
                // get uid and body data from eg: {123, <<"Hello">>}
                // {ReqID, Code, ResponseHeaders, ResponseBody}
                reqidr = erl_element(1, emsg.msg);
                coder = erl_element(2, emsg.msg);
                respheadersr = erl_element(3, emsg.msg);
                respbodyr = erl_element(4, emsg.msg);

                int reqid = ERL_INT_VALUE(reqidr);
                int code = ERL_INT_VALUE(coder);
                char *body = (char *) ERL_BIN_PTR(respbodyr);
                int body_len = ERL_BIN_SIZE(respbodyr);
                if (clients[reqid]) {
                    // fprintf(stderr, "... sending %d bytes to uid %d\n", body_len, reqid);
                    struct evbuffer *evbuf = evbuffer_new();
                    evbuffer_add(evbuf, (const void*) body, (size_t) body_len);
                    evhttp_send_reply(clients[reqid], code, "OK", evbuf);
                    clients[reqid] = NULL;
                } else {
                    fprintf(stderr, "... ignoring unknown msgid %d\n", reqid);
                }
                erl_free_term(emsg.msg);
                erl_free_term(reqidr);
                erl_free_term(coder);
                erl_free_term(respheadersr);
                erl_free_term(respbodyr);
                // clients[msgid] = NULL;
                cuid++;
            }
        }
    }
    pthread_exit(0);
}

/* The main loop. Spawn the cnode pthread, create an httpd server on
0.0.0.0:8000, set the generic callback function and start dispatching
requests.
*/
int main(int argc, char **argv) {
    pthread_t helper;
    cuid = 1;
    pthread_create(&helper, NULL, (void *) cnode_run, NULL);

    event_init();
    struct evhttp *httpd = evhttp_start("0.0.0.0", 8000);
    evhttp_set_gencb(httpd, request_handler, NULL);
    event_dispatch();
    evhttp_free(httpd);

    return 0;
}
