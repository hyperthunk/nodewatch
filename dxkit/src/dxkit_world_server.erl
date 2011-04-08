%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: World Server*
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
%% @since: May 2010
%%
%% Provides a server that holds the "current state of the world", in terms of
%% an erlang cluster (list of nodes, node configuration info, connectivity
%% between nodes, etc). The name is a nod to the stdlib (net_adm:world).
%%
%% -----------------------------------------------------------------------------

-module(dxkit_world_server).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start/1
        ,start_link/1]).

-behavior(gen_server2).
-include("../include/types.hrl").
-include("dxkit.hrl").

-record(wstate, {
    start_ts        :: timestamp(),
    timeout         :: integer(),
    options  = []   :: [{atom(), term()}],
    nodes    = []   :: [#node_info{}]
}).

-type(server_option()   :: {refresh, {interval(), unit_of_measure()}} |
                           {startup, {scan, all}} |
                           {nodes, [node()]}).

-define(DEFAULT_TIMEOUT, 60000).

%% -----------------------------------------------------------------------------
%%      Public API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts the server without any configuration.
%%
start() ->
    start([]).

%%
%% @doc Starts the server with the supplied configuration.
%%
%% Options = [
%%            {refresh, {Interval, UnitOfMeasure}}, connection refresh policy |
%%            {startup, {scan, all}}, scan for nodes using net_adm:world/1 |
%%            {nodes, [node()]}, list of nodes to connect
%%            ]
%%
-spec(start/1 :: (Options::[server_option()]) -> term()).
start(Options) ->
    gen_server2:start({local, ?MODULE}, ?MODULE, Options, []).

%%
%% @doc Starts the server with the supplied configuration.
%%
-spec(start_link/1 :: (Options::[server_option()]) -> term()).
start_link(Options) ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, Options, []).


%% -----------------------------------------------------------------------------
%% gen_server2 callbacks
%% -----------------------------------------------------------------------------

%% @hidden
%% initializes the server with the current "state of the world"
init(Args) ->
    process_flag(trap_exit, true),
    Start = erlang:now(), End = Start,
    Timestamp = ?TS(Start, End),
    Timeout = refresh_interval(Args),
    State = #wstate{start_ts=Timestamp, 
                    timeout=Timeout, 
                    options=Args, 
                    nodes=[]},
    set_timer(Timeout),
    case net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]) of
        ok  ->
            io:fwrite("ok we're good to go!~n"),
            {ok, State};
        Err ->
            io:format("we're stopping because of ~p!~n", [Err]),
            {stop, Err}
    end.

handle_call(nodes, {_From, _Tag}, #wstate{nodes=Nodes}=State) ->
    {reply, Nodes, State};
handle_call(Msg, {_From, _Tag}, State) ->
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    fastlog:debug("In ~p `Call': ~p~n", [self(), Msg]),
    {reply, State, State}.

handle_cast(_Msg, State) ->
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    %%fastlog:debug("In ~p `Cast': ~p~n", [self(), Msg]),
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    reset_state({nodeup, Node, []}, State);
handle_info({nodeup, Node, InfoList}, State) ->
    reset_state({nodeup, Node, InfoList}, State);
handle_info({nodedown, Node}, State) ->
    reset_state({nodedown, Node, []}, State);
handle_info({nodedown, Node, InfoList}, State) ->
    reset_state({nodedown, Node, InfoList}, State);
handle_info({timeout, _TRef, refresh}, State) ->
    %% Connections = [dxkit_net:connect(NI) || NI <- State#wstate.nodes],
    %% fastlog:debug("Connections: ~p~n", [Connections]),
    {noreply, refresh(State)};
handle_info(Info, State) ->
    fastlog:debug("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%%      Private API
%% -----------------------------------------------------------------------------

refresh(#wstate{nodes=ExistingNodes, options=Opt, timeout=Timeout}=State) ->
    Nodes = case lists:keyfind(nodes, 1, Opt) of
        {nodes, N} -> N;
        _ -> []
    end,
    %% this might take a while - hence we do this in handle_cast
    Scan = case lists:keyfind(startup, 1, Opt) of
        {startup, {scan, all}} ->
            dxkit_net:find_nodes();
        {startup, {scan, Hosts}} when is_list(Hosts) ->
            lists:flatten([dxkit_net:find_nodes(Host) || Host <- Hosts]);
        _ -> []
    end,
    fastlog:debug("ExistingNodes=~p, Nodes=~p, Scan=~p~n", 
                  [ExistingNodes, Nodes, Scan]),
    Current = [N#node_info.node_name || N <- ExistingNodes],
    NodeList = merge([Current, Nodes, Scan]),
    fastlog:debug("Connected to ~p~n", [NodeList]),
    set_timer(Timeout),
    Updated = [dxkit_net:connect(N) || N <- NodeList, N =/= node()],
    State#wstate{nodes=Updated}.

set_timer(Timeout) ->
    fastlog:debug("Starting refresh timer with a ~p ms interval.~n", [Timeout]),
    erlang:start_timer(Timeout, ?MODULE, refresh).

reset_state({NodeStatus, Node, InfoList}, #wstate{nodes=Nodes}=State) ->
    fastlog:debug("node ~p status change: ~p~n", [Node, NodeStatus]),
    case [N || N <- Nodes, N#node_info.node_name == Node] of
        [NI] when is_record(NI, node_info) ->
            Nodes3 = [dxkit_net:update_node(NI, {NodeStatus, InfoList}) |
                lists:filter(fun ignore_node/1, Nodes)],
            {noreply, State#wstate{nodes=Nodes3}};
        [] ->
            {noreply, State#wstate{nodes=[dxkit_net:connect(Node)|Nodes]}}
    end.

refresh_interval(Config) ->
    %% TODO: move this into init/reconfigure so it isn't running so often
    case lists:keytake(refresh, 1, Config) of
        {value, {refresh, {Int, Uom}}, _} ->
            case Uom of
                milliseconds -> Int;
                _ -> apply(timer, Uom, [Int])
            end;
        {value, {refresh, Millis}, _} ->
            Millis;
        _ -> 
            ?DEFAULT_TIMEOUT
    end.

merge(NodeLists) ->
    Set = sets:from_list(lists:flatten(NodeLists)),
    sets:to_list(Set).

ignore_node(Node) ->
    fun(N) -> N#node_info.node_name =/= Node end.
