%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Database: Schema Server
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
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
%% -----------------------------------------------------------------------------
%% @author Tim Watson [http://hyperthunk.wordpress.com]
%% @copyright (c) Tim Watson, 2010
%% @since: March 2010
%%
%% @doc Manages the setup/teardown of the database schema. This process sits 
%% directly in the application supervision tree, guaranteeing that the database
%% is properly initialized before we start trying to consume it.
%%
%% -----------------------------------------------------------------------------
-module(dxdb_schema).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-include("../include/types.hrl").

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start_link/0, stop/0]).

%% -----------------------------------------------------------------------------
%%            Public API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts and registers the database subsystem
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init(_) ->
    Tabs = [{user, record_info(fields, user)}, 
            {subscription, record_info(fields, subscription)}],
    [mnesia:subscribe({table, T, simple}) || {T, _} <- Tabs],
    {ok, []}.

%% @hidden
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({write, #subscription{}=S, _}, State) ->
    gen_event:notify(dxdb_event_handler, {subscribe, S}),
    {noreply, State};    
handle_info({delete_object, #subscription{}=S, _}, State) ->
    gen_event:notify(dxdb_event_handler, {unsubscribe, S}),
    {noreply, State};
handle_info({delete_object, #user{}=User, _}, State) ->
    gen_event:notify(dxdb_event_handler, {unsubscribe, User}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    terminated.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

