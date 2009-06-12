
CFLAGS = -I /opt/local/lib/erlang/lib/erl_interface-3.6.1/include/ \
	-arch i386 -Wall -fPIC \
	-L/opt/local/lib/erlang/lib/erl_interface-3.6.1/lib/ 

all: mocheventcnode

mocheventcnode:
	gcc $(CFLAGS) -g -o ../bin/mocheventcnode mocheventcnode.c -lerl_interface -lei -levent

clean:
	rm -rfv ../bin/mocheventcnode*
