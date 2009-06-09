-module(mochevent).
-export([start/1, server/1, server/2, default/1]).

%% @doc Starts the server process
start() ->
    start({?MODULE, default}).

start(Loop) ->
    Pid = spawn_link(?MODULE, server, [Loop]),
    register(mochevent_handler, Pid),
    Pid.
 
%% @private
server(Loop) ->
    server(Loop, 1).
server(Loop, Count) ->
    receive
        {Pid, Id, Method, Uri, Headers, Body} ->
            Req = mochevent_request:new(Pid, Id, Method, Uri, {1, 0}, mochiweb_headers:make(Headers), Body),
            handle_request(Loop, Req);
        Other ->
            error_logger:error_report({?MODULE, ?LINE, unhandled_receive, Other}),
            ok
    end,
    server(Loop, Count + 1).

handle_request({M, F}, Req) ->
    M:F(Req);
handle_request(Fun, Req) when is_function(Fun) ->
    Fun(Req).

default(Req) ->
    Req:respond({200, [{"content-type", "text/plain"}], <<"The rain in Spain falls gently on the plain.">>}).
