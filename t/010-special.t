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
    Pid = spawn_link(fun() -> server() end),
    register(mochevent_handler, Pid),

    os:cmd("for i in `cat /tmp/mochevent.lock`; do kill $i; done"),
    os:cmd("./bin/mocheventcnode --daemon --ip 0.0.0.0 --port 5001 --master httpdmaster@`hostname`"),

    (fun() ->
        Request = etap_web:build_request(get, "http://127.0.0.1:5001/foo.bin", [], []),
        Request:status_is(200, "status code ok"),
        Request:body_is("It's a secret to everybody.\n", "body ok"),
        ok
    end)(),

    ok.

server() ->
    receive
        {Pid, Id, Method, <<"/foo.bin">>, Headers, Body} ->
            Req = mochevent_request:new(Pid, Id, Method, <<"/foo.bin">>, {1, 0}, mochiweb_headers:make(Headers), Body),
            {ok, Binary} = file:read_file("foo.bin"),
            Req:respond({200, [{<<"content-type">>, <<"application/octet-stream">>}], Binary})
    end,
    server().
