
# About

The mochevent project provides a libevent based Erlang cnode that can compose  [mochiweb](http://code.google.com/p/mochiweb/) compliant requests that can dispatch for distributed request processing.

In a nutshell, as you have nodex running to process web requests that would be handled natively by mochiweb, you can run the mocheventcnode daemon to handle incoming http responses. The mochevent daemon will break down requests and dispatch them to the node much like mochiweb would. The mochevent\_request module is mostly compliant with the mochiweb\_request module and the two can be switched out transparently. The mochevent project was made to allow you to scale Erlang based web services cheaply and efficiently by offloading socket handling, request parsing and response composition to a c application.

This is release candidate software. We've worked hard to get as many of the kinks out as possible but there may be more that we don't know about. Please use this in your dev and test environments and send bug reports. Do send as much information as possible to help us replicate any issues.

# Building

    $ autoconf
    $ ./configure [--enable-64bit]
    $ make

If you encounter the following error you may need to run configure with the --enable-64bit command line argument.

    xxx.a, file is not of required architecture

# Quick Start Guide

It's easy, first start the httpdmaster node and start the mochevent handler.

    $ erl -pa ./ebin -setcookie supersecret -name httpdmaster@localhost
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
