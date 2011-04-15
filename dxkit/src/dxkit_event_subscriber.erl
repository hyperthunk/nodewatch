%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Kit: A bridge between event manager and handler
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

-module(dxkit_event_subscriber).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/2]).

-include_lib("fastlog/include/fastlog.hrl").

%%
%% Public API
%%

start_link(Handler, Options) ->
    fastlog:info(dxkit.event, "Starting Handler ~p~n", [Handler]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Handler, Options], []).

%%
%% gen_server callbacks
%%

%% @hidden
init([Handler, Args]) ->
    Result = gen_event:add_sup_handler(dxkit_event_handler, Handler, Args),
    fastlog:info(dxkit.event, "Subscription: ~p~n", [Result]),
    case Result of
        ok ->
            process_flag(trap_exit, true),
            {ok, []};
        Other ->
            {stop, {subscription_failed, Other}}
    end.

handle_call(Msg, _From, State) ->
    fastlog:debug("In ~p `Call': ~p~n", [self(), Msg]),
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, _Handler, normal}, State) ->
    {noreply, State};
handle_info({gen_event_EXIT, _Handler, shutdown}, State) ->
    {noreply, State};
handle_info({gen_event_EXIT, _Handler, {swapped,_,_}}, State) ->
    {noreply, State};
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    {stop, {exit, Handler, Reason}, State};
handle_info(Info, State) ->
    fastlog:debug("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
