%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Misultin Based HTTP Handler Callbacks
%%
%% Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% ------------------------------------------------------------------------------
%%
%% @doc This library module provides a startup function to initialise misultin,
%% and exposes the callback methods used during HTTP request processing.
%%
%% ------------------------------------------------------------------------------

-module(dxweb_http_handler).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([start_link/1
        ,http_handler_loop/1
        ,websocket_handler_loop/1]).

start_link(Options) ->
    F = fun({K,_}=E, Acc) -> lists:keystore(K, 1, Acc, E) end,
    InternalOpts = [{loop, fun http_handler_loop/1},
                    {ws_loop, fun websocket_handler_loop/1},
                    {ws_autoexit, false}],
    StartOpts = lists:foldl(F, Options, InternalOpts),
    misultin:start_link(StartOpts).

websocket_handler_loop(_Ws) ->
    ok.

http_handler_loop(Req) ->
    PathComponents = Req:resource([lowercase, urldecode]),
    SID = check_session(Req),
    handle_http_request(Req, PathComponents, SID).

check_session(Req) ->
    Cookie = dxweb_util:parse_cookie(Req),
    proplists:get_value(sid, Cookie, invalid).
    
handle_http_request(Req, invalid, ["service", "login"]) ->
    case dxweb_session:login(Req) of
        invalid ->
            handle_http_request(Req, invalid, []);
        SID ->
            Req:ok([{"Content-Type", "text/plain"},
                    {"Set-Cookie", "sid=" ++ SID}], "login successful.")
    end;
handle_http_request(Req, invalid, _) ->
    Req:respond(401);
handle_http_request(Req, _, ["static"|ResourcePath]) ->
    Req:file(resolve_path(ResourcePath));
handle_http_request(Req, SID, ["service", Resource|Rest]) ->
    Mod = "dxweb_" ++ Resource ++ "_controller",
    apply(Mod, string:to_lower(atom_to_list(Req:get(method))), [Req,SID|Rest]).

resolve_path(ResourcePath) ->
    BaseDir = filename:join(code:priv_dir(dxweb), "www"),
    SubDir = lists:foldl(fun filename:join/2, [], lists:reverse(ResourcePath)),
    filename:join(BaseDir, SubDir).

%% handle_websocket(Ws) ->
%%    handle_websocket(Ws).
