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

-behavior(gen_server2).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start/1
        ,start_link/1]).

-include("../include/types.hrl").

-record(wstate, {
    start_ts        :: timestamp(),
    options  = []   :: [{atom(), term()}],
    nodes    = []   :: [#node_info{}]
}).

-type(server_option()   :: {refresh, {interval(), unit_of_measure()}} |
                           {startup, {scan, all}} |
                           {nodes, [node()]}).

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
    startup(gen_server2:start({local, ?MODULE}, ?MODULE, Options, [])).

%%
%% @doc Starts the server with the supplied configuration.
%%
-spec(start_link/1 :: (Options::[server_option()]) -> term()).
start_link(Options) ->
    startup(gen_server2:start_link({local, ?MODULE}, ?MODULE, Options, [])).

startup({ok, Pid}) ->
    ok = gen_server:cast(Pid, scan),
    Pid;
startup(Other) ->
    Other.

%% -----------------------------------------------------------------------------
%% gen_server2 callbacks
%% -----------------------------------------------------------------------------

%% @hidden
%% initializes the server with the current "state of the world"
init(Args) ->
    %% FIXME: what were you for again?
    %% Start = erlang:now(), 
    %% End = Start, 
    %% Timestamp = ?TS(Start, End),
    World = case lists:keytake(startup, 1, Args) of
        {value, {startup, {scan, all}}, _} ->
            net_adm:world();
        _ ->
            []
    end,
    io:format("World = ~p~n", [World]),
    {Nodes, Config} = case lists:keytake(nodes, 1, Args) of
        false -> {World, Args};
        {value, {nodes, N}, Cfg} -> {N ++ World, Cfg}
    end,
    NodeList = [dxkit_net:connect(Node) || Node <- Nodes, Node =/= node()],

    State = #wstate{start_ts=erlang:now(), options=Config, nodes=NodeList},
    case net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]) of
        ok  ->
            refresh_timer(Config), 
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

handle_cast(Msg, State) ->
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    fastlog:debug("In ~p `Cast': ~p~n", [self(), Msg]),
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
    Connections = [dxkit_net:connect(NI) || NI <- State#wstate.nodes],
    fastlog:debug("Connections: ~p~n", [Connections]),
    refresh_timer(State#wstate.options),
    {noreply, State};
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

reset_state({NodeStatus, Node, InfoList}, #wstate{nodes=Nodes}=State) ->
    fastlog:debug("node '~p' status change: ~p~n", [Node, NodeStatus]),
    case [N || N <- Nodes, N#node_info.node_name == Node] of
        [NI] when is_record(NI, node_info) ->
            Nodes3 = [dxkit_net:update_node(NI, {NodeStatus, InfoList}) |
                lists:filter(fun ignore_node/1, Nodes)],
            {noreply, State#wstate{nodes=Nodes3}};
        [] ->
            {noreply, State#wstate{nodes=[dxkit_net:connect(Node)|Nodes]}}
    end.

refresh_timer(Config) ->
    case lists:keytake(refresh, 1, Config) of
        {value, {refresh, {Int, Uom}}, _} ->
            Timeout = case Uom of
                milliseconds -> Int;
                _ -> apply(timer, Uom, [Int])
            end,
            fastlog:debug("Starting refresh timer with a ~p ms interval.~n", [Timeout]),
            erlang:start_timer(Timeout, ?MODULE, refresh);
        _ -> ignored
    end.

ignore_node(Node) ->
    fun(N) -> N#node_info.node_name =/= Node end.
