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

-module(esmt_world_server).
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

-include("types.hrl").
-include("esmt.hrl").

-record(wstate, {
    options  = []   :: [{atom(), term()}],
    nodes    = []   :: [#node_info{}]
}).

-type(interval()        :: integer()).
-type(unit_of_measure() :: seconds | minutes | hours | milliseconds).
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
    Start = erlang:now(), End = Start, Timestamp = ?TS(Start, End),
    World = case lists:keytake(startup, 1, Args) of
        false -> [];
        {value, {startup, Startup}, _} ->
            case lists:keytake(scan, 1, Startup) of
                false -> [];
                _     -> net_adm:world()
            end
    end,
    {Nodes, Config} = case lists:keytake(nodes, 1, Args) of
        false -> {World, Args};
        {value, {nodes, N}, Cfg} -> {N ++ World, Cfg}
    end,
    NodeList = [ esmt_net:connect(Node) || Node <- Nodes, Node =/= node()],
    State = #wstate{options=Config, nodes=NodeList},
    case net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]) of
        ok  ->
            refresh_timer(Config), {ok, State};
        Err -> {stop, Err}
    end.

handle_call(Msg, {From, Tag}, State) ->
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    io:format("In ~p `Call': ~p~n", [self(), Msg]),
    {reply, State, State}.

handle_cast(Msg, State) ->
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    io:format("In ~p `Cast': ~p~n", [self(), Msg]),
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    reset_state({nodeup, Node, []}, State);
handle_info({nodeup, Node, InfoList}, State) ->
    reset_state({nodeup, Node, InfoList}, State);
handle_info({nodedown, Node}, State) ->
    reset_state({nodedown, Node, []}, State);
handle_info({nodedown, Node, InfoList}, State) ->
    reset_state({nodedown, Node, InfoList}, State);
handle_info({timeout, TRef, refresh}, State) ->
    Connections = [ esmt_net:connect(NI)
                    || NI <- State#wstate.nodes ],
    ?DEBUG("Connections: ~p~n", [Connections]),
    refresh_timer(State#wstate.options),
    {noreply, State};
handle_info(Info, State) ->
    ?INFO("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%%      Private API
%% -----------------------------------------------------------------------------

reset_state({NodeStatus, Node, InfoList}, #wstate{nodes=Nodes}=State) ->
    ?INFO("node '~p' status change: ~p~n", [Node, NodeStatus]),
    Match = esmt_utils:find(match_node(Node), undefined, Nodes),
    case Match of
        undefined ->
            ?WARN("Unable to find #node_info record matching ~p~n", [Node]),
            {noreply, State};
        NI when is_record(NI, node_info) ->
            Nodes3 = [ esmt_net:update_node(NI, {NodeStatus, InfoList})
                        | lists:filter(fun ignore_node/1, Nodes) ],
            {noreply, State#wstate{nodes=Nodes3}}
    end.

refresh_timer(Config) ->
    case lists:keytake(refresh, 1, Config) of
        {value, {refresh, {Int, Uom}}, _} ->
            Timeout = case Uom of
                milliseconds -> Int;
                _ -> apply(timer, Uom, [Int])
            end,
            ?INFO("Starting refresh timer with a ~p ms interval.~n", [Timeout]),
            erlang:start_timer(Timeout, ?MODULE, refresh);
        _ -> ignored
    end.

ignore_node(Node) ->
    fun(N) -> not(Node#node_info.node_name == Node) end.

match_node(Node) ->
    fun(E) -> (E#node_info.node_name == Node) end.
