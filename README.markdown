
# About

mochevent is a libevent based front-end to a mochiweb compliant request module that can dispatch a subset of request types.

This is release candidate software. We've worked hard to get as many of the kinks out as possible but there may be more that we don't know about. Please use this in your dev and test environments and send bug reports. Do send as much information as possible to help us replicate any issues.

# Prerequisites 

Libevent needs to be installed using environment CFLAGS="-arch i386"

# Building

This project has not been built or tested on any environments other than the following:

 * 9.7.0 Darwin Kernel Version 9.7.0: Tue Mar 31 22:52:17 PDT 2009; root:xnu-1228.12.14~1/RELEASE_I386 i386

# Quick Start Guide

It's easy, first start the httpdmaster node and start the mochevent handler.

    $ erl -pa ./ebin -setcookie secretcookie -sname httpdmaster@localhost
    1> mochevent:start({mochevent, default}).
    <0.x.y>

Then start up the app.

    $ ./bin/mocheventcnode

You can test it with curl.

    $ curl http://127.0.0.1:8000/oooohyeaaaah
    The rain in Spain falls gently on the plain.

# Command-line Arguments

 * --ip &lt;ip address&gt;, defaults to "127.0.0.1"
 * --port &lt;port&gt;, defaults to 8000
 * --master &lt;node name&gt;, defaults to "httpdmaster@localhost"
 * --secret &lt;node cookie&gt;, defaults to "supersecret"
 * --timeout &lt;seconds&gt;, defaults to 10
 * --remote &lt;registered process name&gt;, defaults to "mochevent_handler"
 * --daemon, runs the application in daemon mode

## Daemon mode

When running in daemon mode with the `--daemon` flag, it will run the application in the background and write to the /tmp/mochevent.log and /tmp/mochevent.lock files.

# What's Next?

 * Adding init.d scripts
 * Better build process, porting to other environments
 * Code cleanup
 * Tests
