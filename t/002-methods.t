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
        Request = etap_web:build_request(get, "http://127.0.0.1:5001/nick", [], []),
        Request:status_is(200, "status code ok"),
        Request:body_is("Received request method was GET", "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(post, "http://127.0.0.1:5001/nick", [], "POST body"),
        Request:status_is(200, "status code ok"),
        Request:body_is("Received request method was POST", "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(put, "http://127.0.0.1:5001/nick", [], "PUT body"),
        Request:status_is(200, "status code ok"),
        Request:body_is("Received request method was PUT", "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(delete, "http://127.0.0.1:5001/nick", [], []),
        Request:status_is(200, "status code ok"),
        Request:body_is("Received request method was DELETE", "body ok"),
        ok
    end)(),

    ok.

server() ->
    receive
        {Pid, Id, Method, Uri, Headers, Body} ->
            Req = mochevent_request:new(Pid, Id, Method, Uri, {1, 0}, mochiweb_headers:make(Headers), Body),
            OutBody = erlang:iolist_to_binary([<<"Received request method was ">>, clean_method(Method)]),
            Req:respond({200, [{<<"content-type">>, <<"text/plain">>}], OutBody})
    end,
    server().

clean_method(0) -> "GET";
clean_method(1) -> "POST";
clean_method(3) -> "PUT";
clean_method(4) -> "DELETE";
clean_method(_) -> "OTHER".
