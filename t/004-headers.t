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

gen_headers(N) ->
    [{"header" ++ integer_to_list(X), "value" ++ integer_to_list(X)} || X <- lists:seq(1, N)].

start() ->
    Pid = spawn_link(fun() -> server() end),
    register(mochevent_handler, Pid),

    os:cmd("for i in `cat /tmp/mochevent.lock`; do kill $i; done"),
    os:cmd("./bin/mocheventcnode --daemon --ip 0.0.0.0 --port 5001 --master httpdmaster@`hostname`"),

    (fun() ->
        Request = etap_web:build_request(get, "http://127.0.0.1:5001/nick", gen_headers(25), "GET body"),
        Request:status_is(200, "status code ok"),
        Request:body_is(header_body(gen_headers(25)), "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(get, "http://127.0.0.1:5001/nick", gen_headers(51), "GET body"),
        Request:status_is(200, "status code ok"),
        Request:body_is(header_body(gen_headers(51)), "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(post, "http://127.0.0.1:5001/nick", gen_headers(25), "POST body"),
        Request:status_is(200, "status code ok"),
        Request:body_is(header_body(gen_headers(25)), "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(put, "http://127.0.0.1:5001/nick", gen_headers(25), "PUT body"),
        Request:status_is(200, "status code ok"),
        Request:body_is(header_body(gen_headers(25)), "body ok"),
        ok
    end)(),
    
    (fun() ->
        Request = etap_web:build_request(delete, "http://127.0.0.1:5001/nick", gen_headers(25), "DELETE body"),
        Request:status_is(200, "status code ok"),
        Request:body_is(header_body(gen_headers(25)), "body ok"),
        ok
    end)(),

    ok.

server() ->
    receive
        {Pid, Id, Method, Uri, Headers, Body} ->
            Req = mochevent_request:new(Pid, Id, Method, Uri, {1, 0}, mochiweb_headers:make(Headers), Body),
            OutBody = header_body(Headers),
            Req:respond({200, [{<<"content-type">>, <<"text/plain">>}], list_to_binary(OutBody)})
    end,
    server().

header_body(Headers) ->
    lists:foldl(
        fun ({"header" ++ _ = K, V}, Str) -> lists:append([Str, K ++ ":" ++ V ++ "\n"]);
            (_, Str) -> Str
        end,
        "",
        Headers
    ).
