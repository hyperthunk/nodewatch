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

-module(dxkit_world).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/0, start/1, start_link/1]).

-export([nodes/0, node/1]).

-behavior(gen_server2).

-include_lib("dxcommon/include/dxcommon.hrl").
-include_lib("fastlog/include/fastlog.hrl").
-include("dxkit.hrl").

-record(wstate, {
    start_ts        :: timestamp(),
    timeout         :: integer(),
    options  = []   :: [{atom(), term()}],
    nodes           :: term()
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

node(NodeId) ->
    gen_server:call(?MODULE, {node, NodeId}).

%%
%% @doc Returns all the nodes that are currently being monitored.
%%
nodes() ->
    gen_server:call(?MODULE, nodes).

%% -----------------------------------------------------------------------------
%% gen_server2 callbacks
%% -----------------------------------------------------------------------------

%% @hidden
init(Args) ->
    process_flag(trap_exit, true),
    Start = erlang:now(), End = Start,
    Timestamp = ?TS(Start, End),
    Timeout = refresh_interval(Args),
    State = #wstate{start_ts=Timestamp,
                    timeout=Timeout,
                    options=Args,
                    nodes=ets:new(dx.world.nodes, [{keypos, 2}])},
    %% initial discovery takes place out of band as this can block for a while....
    erlang:start_timer(3000, ?MODULE, start),
    {ok, State}.

handle_call({node, NodeId}, _, #wstate{nodes=Tab}=State) ->
    {reply, ets:lookup(Tab, NodeId), State};
handle_call(nodes, {_From, _Tag}, #wstate{nodes=Tab}=State) ->
    {reply, ets:tab2list(Tab), State}.

handle_cast(_Msg, State) ->
    {noreply, State}. 

handle_info({nodeup, Node}, State) ->
    reset_state({nodeup, Node, []}, State);
handle_info({nodeup, Node, InfoList}, State) ->
    reset_state({nodeup, Node, InfoList}, State);
handle_info({nodedown, Node}, State) ->
    reset_state({nodedown, Node, []}, State);
handle_info({nodedown, Node, InfoList}, State) ->
    reset_state({nodedown, Node, InfoList}, State);
handle_info({timeout, _TRef, start}, State) ->
    handle_info({timeout, _TRef, refresh}, State),
    case start_monitor() of
        ok  ->
            {noreply, State};
        Err -> {stop, Err, State}
    end;
handle_info({timeout, _TRef, refresh},     
            #wstate{options=Opt,
                    timeout=Timeout}=State) ->
    Worker = fun() ->
        %% FIXME: if find_nodes regularly takes longer than the timeout,
        %%  we need to `up' the timeout accordingly...
        case lists:keyfind(startup, 1, Opt) of
            %% TODO: rename this config element, as it 
            %% doesn't *only* run on startup
            {startup, {scan, all}} ->
                dxkit_net:find_nodes();
            {startup, {scan, Hosts}} when is_list(Hosts) ->
                lists:flatten([dxkit_net:find_nodes(Host) || Host <- Hosts]);
            _ -> 
                ok
        end
    end,
    spawn_link(Worker),
    set_timer(Timeout),
    {noreply, State};
handle_info({'EXIT', _, normal}, State) ->
    %% a `scan' worker has exited normally
    {noreply, State};
handle_info(Other, State) ->
    ?DEBUG("Other info event: ~p~n", [Other]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%%      Private API
%% -----------------------------------------------------------------------------

start_monitor() ->
    net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]).

set_timer(Timeout) ->
    ?DEBUG("Starting refresh timer with a ~pms interval.~n", [Timeout]),
    erlang:start_timer(Timeout, ?MODULE, refresh).

reset_state({NodeStatus, Node, InfoList}, #wstate{nodes=Tab}=State) ->
    NodeInfo = case ets:lookup(Tab, Node) of
        [NI] ->
            dxkit_net:sync(NI#node_info.status, 
                NI#node_info{status=NodeStatus, info=InfoList});
        _ ->
            dxkit_net:connect(Node)
    end,
    ?INFO("Node ~p status change: ~p~n", [NodeInfo, NodeStatus]),
    ets:insert(Tab, NodeInfo),
    %% TODO: just send the actual node!?
    gen_event:notify(dxkit_event_handler, {world, {NodeStatus, NodeInfo}}),
    {noreply, State}.

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
