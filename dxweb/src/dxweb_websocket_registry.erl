%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: WebSocket (to ID) Session Registry
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
%% @doc This process maintains a wrapper around a dict, mapping client 
%% session ids to open web sockets. The API provides for managing these mappings
%% and for broadcasting to some/any/all websockets associated with a session id.
%% 
%% ------------------------------------------------------------------------------

-module(dxweb_websocket_registry).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server2).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0, start_link/0, stop/0]).

-export([store/2
        ,remove/2
        ,lookup/1
        ,register/1
        ,unregister/1]).

-record(state, {
    sessions :: dict()
}).

%% -----------------------------------------------------------------------------
%%      Public API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts the server for standalone use.
%%
start() ->
    gen_server2:start({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Starts and registers the server locally, for use in a supervision tree.
%%
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Manually stops the server.
%%
stop() ->
    gen_server2:cast(?MODULE, stop).

%% code gen would be pretty useful here....

%%
%% @doc Registers a new session and returns the session id.
%%
register(SessionID) ->
    dispatch_call({register, SessionID}).

%%
%% @doc Stores a websocket reference against a given session
%%
store(SessionID, WebSock) ->
    dispatch_call({store, SessionID, WebSock}).

%%
%% @doc Removes a websocket reference from the list of websockets for a session
%%
remove(SessionID, WebSock) ->
    dispatch_call({remove, SessionID, WebSock}).

%%
%% @doc Gets a list of all websockets references for the specified session
%%
lookup(SessionID) ->
    dispatch_call({lookup, SessionID}).

%%
%% @doc Removes a session and clears all its websocket references.
%%
unregister(SessionID) ->
    dispatch_call({unregister, SessionID}).

%% @hidden
dispatch_call(Msg) ->
    gen_server2:call(?MODULE, Msg).

%% -----------------------------------------------------------------------------
%% gen_server2 callbacks
%% -----------------------------------------------------------------------------

%% @hidden
init(_) ->
    {ok, #state{ sessions=dict:new() }}.

%% @hidden
handle_call({register, SessionID}, _From, #state{ sessions=S }=State) ->
    {reply, {registered, SessionID}, State#state{ sessions=dict:store(SessionID, [], S) }};
handle_call({unregister, SessionID}, _From, #state{ sessions=S }=State) ->
    {reply, unregistered, State#state{ sessions=dict:erase(SessionID, S) }};
handle_call({store, SessionID, WebSocket}, _From, #state{ sessions=S }=State) ->
    {reply, stored, State#state{ sessions=dict:append(SessionID, WebSocket, S) }};
handle_call({remove, SessionID, WebSocket}, _From, #state{ sessions=Sessions }=State) ->
    {ok, WebSocks} = dict:find(SessionID, Sessions),
    WebSocks2 = [S || S <- WebSocks, S =/= WebSocket],
    {reply, stored, State#state{ sessions=dict:store(SessionID, WebSocks2, Sessions) }};
handle_call({lookup, SessionID}, _From, #state{ sessions=S }=State) ->
    {reply, dict:find(SessionID, S), State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    terminated.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

