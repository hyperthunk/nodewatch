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

-export([start_link/1, http_handler_loop/1, websocket_handler_loop/1]).

start_link(Options) ->
    F = fun({K,_}=E, Acc) -> lists:keystore(K, 1, Acc, E) end,
    InternalOpts = [{loop, fun http_handler_loop/1},
                    {ws_loop, fun websocket_handler_loop/1},
                    {ws_autoexit, false}],
    io:format("~p Options: ~p~n", [?MODULE, Options]),    
    StartOpts = lists:foldl(F, Options, InternalOpts),
    io:format("StartOpts: ~p~n", [StartOpts]),
    misultin:start_link(StartOpts).

http_handler_loop(Req) ->
    PathComponents = Req:resource([lowercase, urldecode]),
    SID = check_session(Req),
    fastlog:info("Request[~p]-SID[~p]~n", [PathComponents, SID]),
    handle_http_request(Req, SID, PathComponents).

websocket_handler_loop(Ws) ->
    SID = get_ws_client_id(Ws),
    ok = dxweb_session:set_websocket(SID, Ws),
    handle_websocket(Ws).

handle_http_request(Req, ExistingSID, ["service", "login"]) ->
    %% TODO: stop this double checking of the session id
    case dxweb_session:is_valid(ExistingSID) of
        true ->
            Req:ok([{"Content-Type", "text/plain"}], "Already logged in!");
        false ->
            case dxweb_session:establish_session(Req) of
                {error, Reason} ->
                    Req:respond(401, [Reason]);
                {ignored, OtherSID} ->
                    send_sid(Req, OtherSID);
                NewSID ->
                    send_sid(Req, NewSID)
            end
    end;
handle_http_request(Req, _, ["dashboard.html"]=Path) ->
    Req:file(resolve_path(Path));
handle_http_request(Req, _, ["static"|ResourcePath]) ->
    Req:file(resolve_path(ResourcePath));
handle_http_request(Req, invalid, _) ->
    Req:respond(401, [{"Location", "service/login"}], []);
handle_http_request(Req, SID, ["service"|Resource]) ->
    apply(dxweb_controller, 
            http_method_to_function(Req), 
            [Req,SID,Resource]).

%%
%% NB: This callback (loop) simply idles the websocket to keep it open,
%% since websocket traffic is only used for outbound event publication
%%
handle_websocket(Ws) ->
    %% TODO: because this loop sits idle for the most part, a hibernating
    %% gen_server might make for a better home - can misultin do this?
    receive
        closed ->
            %% NB: the disconnection of a websocket resets/clears the session
            SessionID = get_ws_client_id(Ws),
            fastlog:info("Websocket ~p-~pwas closed!~n", [SessionID, Ws]),
            ok = dxweb_session:suspend_session_websocket(SessionID);
        Other ->
            fastlog:debug("Websocket *other* => ~p~n", [Other]),
            handle_websocket(Ws)
    end.

send_sid(Req, SID) ->
    Req:ok([{"Content-Type", "application/json"},
            {"Set-Cookie", "sid=" ++ SID ++ "; path=/"}], 
            "login successful.").

http_method_to_function(Req) ->
    list_to_atom(string:to_lower(atom_to_list(Req:get(method)))).

get_ws_client_id(Ws) ->
    [$/|ClientID] = Ws:get(path),
    fastlog:debug("Websocket connection for client ~p~n", [ClientID]),
    ClientID.

check_session(Req) ->
    Cookie = dxweb_util:parse_cookie(Req),
    SID = proplists:get_value("sid", Cookie, "ignored"),
    fastlog:info("Checking Request Cookie (sid): ~p~n", [SID]),
    case dxweb_session:is_valid(SID) of
        true ->
            fastlog:debug("SID is valid"),
            SID;
        false ->
            fastlog:debug("SID is invalid"),
            invalid
    end.

resolve_path(ResourcePath) ->
    BaseDir = filename:join(code:priv_dir(dxweb), "www"),
    SubDir = lists:foldl(fun filename:join/2, [], lists:reverse(ResourcePath)),
    filename:join(BaseDir, SubDir).
