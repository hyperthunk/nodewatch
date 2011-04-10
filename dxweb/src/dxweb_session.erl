%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Session (utilities) API
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
%% @doc 
%%
%% ------------------------------------------------------------------------------

-module(dxweb_session).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([establish_session/1
        ,send/2
        ,send_term/2]).

%%
%% @doc Converts `Term' to json and sends it to 
%% any websocket registered, against the supplied `SessionID'.
%%
send(SessionID, Data) ->
    send_term(SessionID, jsx:term_to_json(Data)).

%%
%% @doc Sends `Term' directly to any websocket registered
%% against the supplied `SessionID'.
%%
send_term(SessionID, Term) ->
    case dxweb_websocket_registry:lookup(SessionID) of
        error ->
            {error, {invalid_sid, SessionID}};
        WebSock ->
            WebSock:send(Term)
    end.

%%
%% @doc Creates a new session, authenticating the user's
%% credentials and returning a valid session-id (or the atom)
%% or {error, Reason} if login fails.
%%
establish_session(Req) ->
    case validate_user(auth_credentials(Req)) of
        true ->
            UUID = dxweb_util:make_uuid(),
            {registered, UUID} = dxweb_websocket_registry:register(UUID),
            UUID;
        _NoAuth -> 
            {error, <<"login failed">>}
    end.

auth_credentials(Req) ->
    PostData = Req:parse_post(),
    {proplists:get_value(username, PostData), 
     proplists:get_value(password, PostData)}.

validate_user({UserName, Password}) ->
    dxdb:check_user(UserName, Password).
