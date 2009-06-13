#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name httpdmaster -pa ./ebin -boot start_sasl -setcookie supersecret

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    inets:start(),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("Err ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.

start() ->
    mochevent:start({mochevent, default}),
    os:cmd("for i in `cat /tmp/mochevent.lock`; do kill $i; done"),
    os:cmd("./bin/mocheventcnode --daemon --ip 0.0.0.0 --port 5001 --master httpdmaster@`hostname`"),

    (fun() ->
        Request = etap_web:build_request(get, "http://127.0.0.1:5001/nick", [], []),
        Request:status_is(200, "status code ok"),
        Request:body_is("The rain in Spain falls gently on the plain.", "body ok"),
        ok
    end)(),

    ok.
