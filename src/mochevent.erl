%% Copyright (c) 2009 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(mochevent).
-export([start/0, start/1, server/1, server/2, default/1]).

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
            spawn_link(fun() -> handle_request(Loop, Req) end);
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
    Req:respond({200, [{<<"content-type">>, <<"text/plain">>}], <<"The rain in Spain falls gently on the plain.">>}).
