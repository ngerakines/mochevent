
# About

mochevent is a libevent based front-end to a mochiweb compliant request module that can dispatch a subset of request types.

This is BETA software at best. There are memory leaks and compatibility concerns that should lead you to not using it in production until they get worked out.

# Prerequisites 

Libevent needs to be installed using environment CFLAGS="-arch i386"

# Building

This project has not been built or tested on any environments other than the following:

 * 9.7.0 Darwin Kernel Version 9.7.0: Tue Mar 31 22:52:17 PDT 2009; root:xnu-1228.12.14~1/RELEASE_I386 i386

# Quick Start Guide

It's easy, first start the httpdmaster node and start the mochevent handler.

    $ erl -pa ./ebin -setcookie secretcookie -sname httpdmaster@yourhost
    1> mochevent:start({mochevent, default}).
    <0.x.y>

Then start up the app.

    $ ./bin/mocheventcnode

You can test it with curl.

    $ curl http://localhost:8000/oooohyeaaaah
    The rain in Spain falls gently on the plain.

# What's Next?

 * Processing command line arguments for: ip/port binding, remote node name and dispatch process name
 * Clean up object creation and destruction
 * Clean up race conditions with proper mutex calls/locks
 * Adding init.d scripts
