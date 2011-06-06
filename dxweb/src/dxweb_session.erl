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
%% @doc This server maintains a set of mappings between authenticated clients,
%% session ids and open web sockets. As well as establishing and removing a
%% session, this module will allow you to configure the websocket connection for
%% it and send terms (as json) to the websocket client for a given session id.
%%
%% ------------------------------------------------------------------------------

-module(dxweb_session).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/0, start_link/0]).

-export([establish_session/1, send/2, send_term/2, send_all/1,
         set_websocket/2, remove_session/1, is_valid/1,
         all_sessions/0, get_user_from_req/1, suspend_session_websocket/1]).

-include_lib("dxcommon/include/dxcommon.hrl").
-include_lib("fastlog_parse_trans/include/fastlog.hrl").

-record(session, {sid, user, websock}).

%%
%% Public API
%%

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Converts `Term' to json and sends it to
%% any websocket registered, against the supplied `SessionID'.
%%
send(SessionID, Data) ->
    send_term(SessionID, dxweb_util:marshal(Data)).

%%
%% @doc Sends `Term' directly to any websocket registered
%% against the supplied `SessionID'.
%%
send_term(SessionID, Term) ->
    case lookup_websocket(SessionID) of
        {error, Reason} ->
            ?WARN("No Websocket(s) Registered for SID "
                  "~p~n@BEGIN_DATA@~n~p~n@END_DATA@~n", 
                  [{SessionID, Reason}, Term]);
        WebSock ->
            WebSock:send(Term)
    end.

%%
%% @doc Sends `Term' to all open websockets - runs out of band.
%%
send_all(Term) ->
    gen_server:call(?MODULE, {send_all, Term}, infinity).

set_websocket(SessionID, WebSock) ->
    gen_server:call(?MODULE, {set_websocket, SessionID, WebSock}).

%%
%% @doc Creates a new session, authenticating the user's
%% credentials and returning a valid session-id (or the atom)
%% or {error, Reason} if login fails.
%%
establish_session(Req) ->
    User = get_user_from_req(Req),
    case authenticate_user(User) of
        true ->
            gen_server:call(?MODULE,
                {establish, #session{ user=User#user.name,
                                      websock=dxweb_invalid_websocket }});
        _NoAuth ->
            {error, <<"login failed">>}
    end.

is_valid(SessionID) ->
    gen_server:call(?MODULE, {check_valid, SessionID}).

all_sessions() ->
    gen_server:call(?MODULE, list).

suspend_session_websocket(SessionID) ->
    set_websocket(SessionID, dxweb_invalid_websocket).

remove_session(SessionID) ->
    gen_server:call(?MODULE, {remove, SessionID}).

get_user_from_req(Req) ->
    PostData = Req:parse_post(),
    Name = proplists:get_value("username", PostData),
    Password = proplists:get_value("password", PostData),
    #user{name=Name, password=Password}.

%%
%% gen_server callbacks
%%

%% @hidden
init(_) ->
    process_flag(trap_exit, true),
    %% The sessions table stores a mapping from session id to user
    ets:new('dxweb.sessions',
            [named_table, private, {keypos, 2}, %% sid == key
            {read_concurrency, erlang:system_info(smp_support)}]),
    {ok, []}.

handle_call({establish, #session{user=User}=Session}, _, State) ->
    case find_user_session(User) of
        {error, _} ->
            SID = dxweb_util:make_uuid(),
            ets:insert('dxweb.sessions', Session#session{sid=SID}),
            {reply, SID, State};
        SID ->
            %% so a user can only be logged in to one session at once
            {reply, {ignored, SID}, State}
    end;
handle_call({check_valid, SessionID}, _, State) ->
    {reply, ets:member('dxweb.sessions', SessionID), State};
handle_call({remove, SessionID}, _, State) ->
    ets:delete('dxweb.sessions', SessionID),
    {reply, ok, State};
handle_call({get_session_id, User}, _, State) ->
    {reply, find_user_session(User), State};
handle_call(list, _, State) ->
    {reply, ets:tab2list('dxweb.sessions'), State};
handle_call({set_websocket, SessionID, WebSock}, _, State) ->
    %% would be better to combine both these operations and use a match spec?
    case ets:lookup('dxweb.sessions', session_id(SessionID)) of
        [Session] ->
            ets:insert('dxweb.sessions', Session#session{websock=WebSock}),
            {reply, ok, State};
        _Other ->
            {reply, {error, "Invalid User Session"}, State}
    end;
handle_call({websocket, SessionID}, _, State) ->
    case ets:lookup('dxweb.sessions', session_id(SessionID)) of
        [Session] ->
            {reply, Session#session.websock, State};
        _Other ->
            {reply, {error, "Invalid User Session"}, State}
    end;
handle_call({send_all, Term}, _, State) ->
    Data = dxweb_util:marshal(Term),
    lists:foreach(fun(S) -> (S#session.websock):send(Data) end,
                  ets:tab2list('dxweb.sessions')),
    {reply, ok, State};
handle_call(Msg, {_From, _Tag}, State) ->
    ?DEBUG("In ~p `Call': ~p~n", [self(), Msg]),
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    ?DEBUG("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% I'm assuming that `misultin' closes websockets when their ws_loop dies?
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

session_id(SessionID) when is_binary(SessionID) ->
    binary_to_list(SessionID);
session_id(SessionID) when is_atom(SessionID) ->
    atom_to_list(SessionID);
session_id(SessionID) when is_list(SessionID) ->
    SessionID.

find_user_session(User) ->
    case ets:match_object('dxweb.sessions', {'_', '_', User, '_'}) of
        [Session] ->
            Session#session.sid;
        [] ->
            {error, "Invalid User Session"}
    end.

lookup_websocket(SessionID) ->
    gen_server:call(?MODULE, {websocket, SessionID}).

authenticate_user(#user{name=UserName, password=Password}) ->
    dxdb:check_user(UserName, Password).
