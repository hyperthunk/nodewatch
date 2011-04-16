%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring: (Eper) Event Consumer.
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
%% @doc When used in conjunction with eper, this module handles monitoring events.
%% One module is created per active subscriber, with the subscriber's client key
%% providing a reply-to mechanism outside this application.
%%
%% This process runs on the `nodewatch' host, not the node(s) being monitored.
%%
%% ------------------------------------------------------------------------------

-module(dxkit_event_consumer, [SubscriberKey, Node, Subscriptions]).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([name/0,
         target/0]).

-export([init/1,
         terminate/1,
         tick/2,
         collectors/0,
         config/2]).

-compile(export_all).

-include_lib("dxcommon/include/dxcommon.hrl").
-include_lib("fastlog/include/fastlog.hrl").

-record(state, {node}).

init(Node) ->
    #state{node=Node}.

terminate(_) -> ok.

name() -> SubscriberKey.
target() -> Node.

collectors() ->
    [ X || X <- lists:map(fun collector_tag/1, Subscriptions),
           X =/= invalid ].

config(State, _) -> State.

tick(State, []) ->
    State;
tick(State, [[]]) ->
    State;
tick(#state{node=Node}=State, [{prfSys, Data}]) ->
    ?DEBUG("Event Consumer Data: ~p~n", [Data]),
    dxkit_event_bridge:publish_event({SubscriberKey, Node, {system, Data}}),
    State;
tick(State, [Data]) ->
    tick(State, Data);
tick(State, Data) ->
    ?DEBUG("Unexpected Event Consumer Data: ~p~n", [Data]),
    State.


%% Internal API

%% filter_events(ID, Node, Ev)
filter_events({prfNet, Data}) ->
    EvData = lists:foldl(fun filter_network_events/2, [], Data),
    {network, EvData};
filter_events({prfPrc, Data}) ->
    EvData = lists:foldl(fun filter_process_events/2, [], Data),
    {process, EvData};
filter_events({prfSys, Data}) ->
    {system, Data}.

%% most of the formatting concerns are elsewhere - here we just want
%% to ensure a bit of sanity in the data structures we're firing off
%% into the ether, filtering out well known badness and so on.

filter_process_events({now, Now}, Acc) ->
    %% TODO: consider whether this term is generic enough to be
    %% handled elsewhere
    TS = {now, dxcommon:iso_8601_time(Now)},
    [TS|Acc];
filter_process_events(Node={node, _}, Acc) ->
    [Node|Acc];
filter_process_events(MsgQ={msgq, _}, Acc) ->
    [MsgQ|Acc];
filter_process_events({info, Data}, Acc) ->
    MoreData = lists:foldl(fun filter_process_events/2, Acc, Data),
    MoreData ++ Acc;  %% efficiency is fine (>= R14)
filter_process_events({Pid, Data}, Acc) when is_pid(Pid)->
    [PidString] = io_lib:format("~p", [Pid]),
    PidEntry = {pid, list_to_binary(PidString)},
    lists:foldl(fun filter_process_events/2, [PidEntry|Acc], Data);
filter_process_events(Ev={_, _}, Acc) ->
    filter_events(Ev, Acc).

%% we're ignoring driver data for now...
filter_network_events({{driver,_}, _}, Acc) ->
    Acc;
filter_network_events({{tcp, {Host, Port}}, Data}, Acc) ->
    FilteredData = lists:fold(fun filter_network_events/2, [], Data),
    Ev = {network, [
            {type, <<"tcp">>},
            {addr, Host},
            {port, Port}|FilteredData]},
    [Ev|Acc];
filter_network_events({links, _}, Acc) -> Acc;
filter_network_events({connected, _}, Acc) -> Acc;
filter_network_events(Ev, Acc) -> filter_events(Ev, Acc).

filter_events({_, EvData}, Acc) when is_pid(EvData) -> Acc;
filter_events({_, EvData}, Acc) when is_port(EvData) -> Acc;
filter_events({_EvTag, _EvData}=Ev, Acc) -> [Ev|Acc];
filter_events(Other, Acc) -> [Other|Acc].

collector_tag(system) -> prfSys;
collector_tag(network) -> prfNet;
collector_tag(process) -> prfPrc;
collector_tag(_) -> invalid.

