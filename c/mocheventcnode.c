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
#include <sys/stat.h>
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
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#define RUNNING_DIR	"/tmp"
#define LOCK_FILE	"mochevent.lock"
#define LOG_FILE	"mochevent.log"

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
char *secret;
char *remotenode;
char *regproc;
int timeout;

void log_message(filename, message) char *filename; char *message; {
    FILE *logfile;
    logfile = fopen(filename,"a");
    if (! logfile) { return; }
    fprintf(logfile, "%s\n", message);
    fclose(logfile);
}

void signal_handler(sig) int sig; {
    switch(sig) {
        case SIGHUP:
            log_message(LOG_FILE,"hangup signal catched");
            break;
        case SIGTERM:
            log_message(LOG_FILE,"terminate signal catched");
            exit(0);
            break;
    }
}

void request_handler(struct evhttp_request *req, void *arg) {
    pthread_mutex_lock(&cuid_mutex);
    if (cuid == MAXUSERS-1) { cuid = 0; }
    int mycuid = cuid++;
    pthread_mutex_unlock(&cuid_mutex);

    u_char *data = EVBUFFER_DATA(req->input_buffer);
    size_t len = EVBUFFER_LENGTH(req->input_buffer);
    char *body;
    if ((body = malloc(len)) == NULL) {
          evbuffer_drain(req->input_buffer, len);
    }

    memcpy(body, data, len);
    evbuffer_drain(req->input_buffer, len);

    ETERM **harray = NULL;
    int nalloc = 0;
    int hcount = 0;
    struct evkeyval *header;
    TAILQ_FOREACH(header, req->input_headers, next) {
        nalloc += 1;
        harray = (ETERM **)realloc((ETERM *)harray, nalloc * sizeof(ETERM *));
        if(harray == NULL){
           fprintf(stderr, "out of memory with %d elements\n", hcount);
           exit(1);
        }
        ETERM *harr[2];
        harr[0] = erl_mk_string((const char *) header->key);
        harr[1] = erl_mk_string((const char *) header->value);
        harray[hcount++] = erl_mk_tuple(harr, 2);
    }

    // {pid(), int(), int(), string(), list(tuple()), string()}
    ETERM *arr[6], *emsg2;
    arr[0] = SELF(fd);
    arr[1] = erl_mk_int(mycuid);
    arr[2] = erl_mk_int(req->type);
    arr[3] = erl_mk_binary(req->uri, strlen(req->uri));
    arr[4] = erl_mk_list(harray, hcount);
    arr[5] = erl_mk_binary(body, len);
    emsg2 = erl_mk_tuple(arr, 6);

    pthread_mutex_lock(&clients_mutex);
    clients[mycuid] = req;
    pthread_mutex_unlock(&clients_mutex);

    erl_reg_send(fd, regproc, emsg2);
    erl_free_compound(emsg2);
    free(body);
    free(harray);

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
    if (erl_connect_init(1, secret, 0) == -1) {
        erl_err_quit("erl_connect_init");
    }
    if ((fd = erl_connect(remotenode)) < 0) {
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
                erl_free_term(respbodyr);
            }
        }
    }
    pthread_exit(0);
}

void daemonize() {
    int i,lfp;
    char str[10];
    /* already a daemon */
    if (getppid() == 1) {
        return;
    }
    i = fork();
    /* fork error */
    if (i < 0) {
        exit(1);
    }
    /* parent exits */
    if (i > 0) {
        exit(0);
    }

    setsid();
    /* close all descriptors */
    for (i = getdtablesize(); i >= 0; --i) {
        close(i);
    }
    i = open("/dev/null", O_RDWR);
    dup(i);
    dup(i);
    umask(027);
    chdir(RUNNING_DIR);

    lfp = open(LOCK_FILE,O_RDWR|O_CREAT,0640);
    if (lfp < 0) {
        exit(1);
    }
    if (lockf(lfp, F_TLOCK, 0) < 0) {
        exit(0);
    }

    sprintf(str, "%d\n", getpid());
    write(lfp, str, strlen(str));

    signal(SIGCHLD, SIG_IGN);
    signal(SIGTSTP, SIG_IGN);
    signal(SIGTTOU, SIG_IGN);
    signal(SIGTTIN, SIG_IGN);
    signal(SIGHUP, signal_handler); /* catch hangup signal */
    signal(SIGTERM, signal_handler); /* catch kill signal */
}

int main(int argc, char **argv) {
    cuid = 1;

    char *ipaddress;
    int port = 8000;
    timeout = 10 * CLOCKS_PER_SEC;
    static int daemon_mode = 0;

    int c;
    while (1) {
        static struct option long_options[] = {
            {"daemon",  no_argument,       &daemon_mode, 1},
            {"ip",      required_argument, 0, 'i'},
            {"port",    required_argument, 0, 'p'},
            {"master",  required_argument, 0, 'm'},
            {"secret",  required_argument, 0, 's'},
            {"timeout", required_argument, 0, 't'},
            {"remote",  required_argument, 0, 'r'},
            {0, 0, 0, 0}
        };
        int option_index = 0;
        c = getopt_long(argc, argv, "i:p:m:s:t:r:", long_options, &option_index);
        if (c == -1) { break; }
        switch (c) {
            case 0:
                if (long_options[option_index].flag != 0) { break; }
                printf ("option %s", long_options[option_index].name);
                if (optarg) { printf(" with arg %s", optarg); }
                printf("\n");
                break;
            case 'i':
                ipaddress = optarg;
                break;
            case 'p':
                port = atoi(optarg);
                break;
            case 's':
                secret = optarg;
                break;
            case 'm':
                remotenode = optarg;
                break;
            case 'r':
                regproc = optarg;
                break;
            case 't':
                timeout = atoi(optarg) * CLOCKS_PER_SEC;
                break;
            case '?':
                /* getopt_long already printed an error message. */
                break;
            default:
                abort();
         }
    }
    if (ipaddress == NULL) {
        ipaddress = "0.0.0.0";
    }
    if (remotenode == NULL) {
        remotenode = "httpdmaster@localhost";
    }
    if (secret == NULL) {
        secret = "supersecret";
    }
    if (regproc == NULL) {
        regproc = "mochevent_handler";
    }
    if (daemon_mode == 1) {
        daemonize();
    }

    pthread_t helper;
    pthread_create(&helper, NULL, (void *) cnode_run, NULL);

    event_init();
    struct evhttp *httpd = evhttp_start(ipaddress, port);
    evhttp_set_gencb(httpd, request_handler, NULL);
    event_dispatch();
    evhttp_free(httpd);

    return 0;
}
