%% Copyright (c) 2007 Mochi Media, Inc.
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
%%
%% @doc A light-weight (hahahaha) request module.
%% This module is based on the mochiweb_request module developed by Bob
%% Ippolito of Mochi Media, Inc.
-module(mochevent_request, [Pid, ReqID, Method, RawPath, Version, Headers, Body]).

-export([get_header_value/1, get_primary_header_value/1, get/1]).
-export([recv_body/0, recv_body/1]).
-export([respond/1, ok/1]).
-export([not_found/0, not_found/1]).
-export([redirect/1, redirect/2]).
-export([parse_post/0, parse_qs/0]).
-export([parse_cookie/0, get_cookie_value/1]).
-export([parse_range_request/1]).
-export([body_length/0, server_headers/0, make_code/1, make_version/1]).

-define(SAVE_QS, mochiweb_request_qs).
-define(SAVE_PATH, mochiweb_request_path).
-define(SAVE_BODY_LENGTH, mochiweb_request_body_length).
-define(SAVE_POST, mochiweb_request_post).
-define(SAVE_COOKIE, mochiweb_request_cookie).

get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

get_primary_header_value(K) ->
    mochiweb_headers:get_primary_value(K, Headers).

get(socket) -> exit(unimplemented);
get(method) -> Method;
get(raw_path) -> RawPath;
get(version) -> Version;
get(headers) -> Headers;
get(peer) -> exit(unimplemented);
get(path) ->
    case erlang:get(?SAVE_PATH) of
        undefined ->
            {Path0, _, _} = mochiweb_util:urlsplit_path(RawPath),
            Path = mochiweb_util:unquote(Path0),
            put(?SAVE_PATH, Path),
            Path;
        Cached ->
            Cached
    end;
get(body_length) -> erlang:get(?SAVE_BODY_LENGTH);
get(range) ->
    case get_header_value(range) of
        undefined ->
            undefined;
        RawRange ->
            parse_range_request(RawRange)
    end.

body_length() ->
    case get_header_value("transfer-encoding") of
        undefined ->
            case get_header_value("content-length") of
                undefined ->
                    undefined;
                Length ->
                    list_to_integer(Length)
            end;
        "chunked" ->
            chunked;
        Unknown ->
            {unknown_transfer_encoding, Unknown}
    end.

recv_body() ->
    Body.

recv_body(_) ->
    Body.

respond({Code, ResponseHeaders, ResponseBody}) ->
    CleanHeaders = [{list_to_binary(K), list_to_binary(V)} || {K,V} <- ResponseHeaders, is_list(K), is_list(V)],
    Pid ! {ReqID, Code, CleanHeaders, ResponseBody}.

not_found() ->
    not_found([]).

not_found(ExtraHeaders) ->
    respond({404, [{"Content-Type", "text/plain"} | ExtraHeaders],
             <<"Not found.">>}).

redirect(Path) ->
    redirect(Path, []).

redirect(Path, ExtraHeaders) ->
    respond({302, % using a Found instead of See Other as per compatibility note http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.3
             [{"Content-Type", "text/plain"},
              {"Location", Path} | ExtraHeaders],
             <<"Found.">>}).

ok({ContentType, Body}) ->
    ok({ContentType, [], Body});
ok({ContentType, ResponseHeaders, Body}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    case THIS:get(range) of
        X when X =:= undefined; X =:= fail ->
            HResponse1 = mochiweb_headers:enter("Content-Type", ContentType, HResponse),
            respond({200, HResponse1, Body});
        Ranges ->
            {PartList, Size} = range_parts(Body, Ranges),
            case PartList of
                [] -> %% no valid ranges
                    HResponse1 = mochiweb_headers:enter("Content-Type",
                                                        ContentType,
                                                        HResponse),
                    %% could be 416, for now we'll just return 200
                    respond({200, HResponse1, Body});
                PartList ->
                    {RangeHeaders, RangeBody} =
                        parts_to_body(PartList, ContentType, Size),
                    HResponse1 = mochiweb_headers:enter_from_list(
                                   [{"Accept-Ranges", "bytes"} |
                                    RangeHeaders],
                                   HResponse),
                    respond({206, HResponse1, RangeBody})
            end
    end.

parse_qs() ->
    case erlang:get(?SAVE_QS) of
        undefined ->
            {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
            Parsed = mochiweb_util:parse_qs(QueryString),
            put(?SAVE_QS, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

parse_cookie() ->
    case erlang:get(?SAVE_COOKIE) of
        undefined ->
            Cookies = case get_header_value("cookie") of
                          undefined ->
                              [];
                          Value ->
                              mochiweb_cookies:parse_cookie(Value)
                      end,
            put(?SAVE_COOKIE, Cookies),
            Cookies;
        Cached ->
            Cached
    end.

parse_post() ->
    case erlang:get(?SAVE_POST) of
        undefined ->
            Parsed = case recv_body() of
                         undefined ->
                             [];
                         Binary ->
                             case get_primary_header_value("content-type") of
                                 "application/x-www-form-urlencoded" ++ _ ->
                                     mochiweb_util:parse_qs(Binary);
                                 _ ->
                                     []
                             end
                     end,
            put(?SAVE_POST, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

server_headers() ->
    [{"Server", "RuPl/0.1"},
     {"Date", httpd_util:rfc1123_date()}].

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

parts_to_body([{Start, End, Body}], ContentType, Size) ->
    %% return body for a range reponse with a single body
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    make_io(Start), "-", make_io(End),
                    "/", make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, ContentType, Size) when is_list(BodyList) ->
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", make_io(Start), "-", make_io(End),
             "/", make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.

range_parts({file, IoDevice}, Ranges) ->
    Size = iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(Body0, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.
