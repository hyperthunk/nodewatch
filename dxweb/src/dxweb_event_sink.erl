%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Event Sink
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
%% @doc Attaches to the dxkit event bridge, proxying events to the right session
%%
%% -----------------------------------------------------------------------------
-module(dxweb_event_sink).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/0, start_listening/0, sink_event/1]).

-include_lib("fastlog/include/fastlog.hrl").

%%
%% Public API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_listening() ->
    gen_server:cast(?MODULE, start),
    ignore.

sink_event(Message) ->
    gen_server:cast(?MODULE, {sink, Message}).

%%
%% gen_server callbacks
%%

%% @hidden
init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

%% TODO: Use the event SID if present.....

handle_call(get, _, State) ->
    {reply, State, State};
handle_call(Msg, _From, State) ->
    fastlog:debug("In ~p `Call': ~p~n", [self(), Msg]),
    {noreply, State}.

handle_cast({sink, {world, {NodeStatus, NodeInfo}}}, State) ->
    Ev = [{event, [{tag, atom_to_binary(NodeStatus, utf8)},
                   {data, dxcommon.data:jsonify(NodeInfo)}]}],
    dxweb_session:send_all(Ev),
    {noreply, [Ev|State]};
handle_cast({sink, {SID, _Node, Event}}, State) ->
    dxweb_session:send(SID, Event),
    {noreply, State};
handle_cast({sink, Ev}, State) ->
    {noreply, [Ev|State]};
handle_cast(start, State) ->
    Res = dxkit:add_event_sink(dxweb_event_handler),
    ?DEBUG("Event sink added: ~p~n", [Res]),
    {noreply, State}.

handle_info(Info, State) ->
    fastlog:debug("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
