%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: Network Admin
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
%% @doc Network utilities.
%%
%% -----------------------------------------------------------------------------

-module(dxkit_net).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include("../include/nodewatch.hrl").
-include("dxkit.hrl").

-record(h_name, {host, domain, fullname}).

-define(DEFAULT_EPMD_PORT, 4369).
-define(DEFAULT_TIMEOUT, 5000).

-export([start/0, start_link/0]).
-export([get_blacklist/0, clear_blacklist/0,
         connect/1, find_nodes/0, find_nodes/1,
         force_connect/1, update_node/2,
         hostname/0, prim_ip/1]).

%%
%% Public API
%%

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Find all detectable nodes. This function is equiv.
%% to net_adm:world/0, but is fasteras it avoids needless connection
%% attempts when hosts are inaccessible and uses a short (5 second)
%% timeout when attempting to locate epmd on the target machine.
%%
%% This function observes the blacklist if it is enabled.
%%
find_nodes() ->
    ok = gen_server:call(?MODULE, find),
    await_response().

%%
%% @doc Find all detectable nodes on Host. This function is similar to
%% net_adm:names/1, but it connects to each node whilst avoiding
%% attempts when hosts are inaccessible and uses a short (5 second)
%% timeout when attempting to locate epmd on the target machine.
%%
%% This function observes the blacklist if it is enabled.
%%
find_nodes(Host) ->
    ok = gen_server:call(?MODULE, {find, Host}),
    await_response().

hostname() ->
    {ok, Name} = inet:gethostname(), Name.

prim_ip(Host) ->
    case inet:ip(Host) of
        {ok, IP} -> IP;
        Err -> Err
    end.

get_blacklist() ->
    gen_server:call(?MODULE, get_blacklist).

clear_blacklist() ->
    gen_server:call(?MODULE, clear_blacklist).

%%
%% @doc Check the connection status of Node and return a #node_info{} record.
%%
-spec(connect/1 :: (Node::atom()) -> #node_info{};
                   (Node::#node_info{}) -> #node_info{}).
connect(Node) when is_atom(Node) ->
    fastlog:debug("Connecting to node '~p'~n", [Node]),
    Status = case net_kernel:connect_node(Node) of
        ignored -> unknown;
        false   -> {nodedown, []};
        true    -> {nodeup, []}
    end,
    fastlog:debug("Node '~p' status: ~p~n", [Node, Status]),
    update_node(Node, Status);
connect(#node_info{node_name=Name}=Node) ->
    NI = connect(Name),
    update_node(Node, NI#node_info.nodestatus).

%%
%% @doc Force check the connection to Node.
%% Error badmatch if net_adm:ping(Node) =/= Node.
%%
-spec(force_connect/1 :: (Node::atom()) -> atom()).
force_connect(Node) when is_atom(Node) ->
    pong = net_adm:ping(Node),
    Node.

%%
%% @doc When called with Node::atom(), creates a #node_info record representing
%% the state of the specified Node. Otherwise, updates the status of the
%% supplied #node_info record with a up/down time adjusted automatically.
%%
-spec(update_node/2 :: (Node::atom(),       Status::nodestatus()) -> #node_info{};
                       (Node::#node_info{}, Status::nodestatus()) -> #node_info{}).
update_node(Node, Status) when is_atom(Node) ->
    Start = erlang:now(),
    End = Start,
    TS = ?TS(Start, End),
    #node_info{
        node_name=Node,
        nodestatus=Status,
        uptime=TS,
        downtime=TS
    };
update_node(
    #node_info{
        nodestatus={nodedown, _},
        uptime={ElapsedUpTime, _},
        downtime={ElapsedDownTime, LastDown}}=Node,
        {nodeup, _}=Status) ->
    Now = erlang:now(),
    Diff = ?DIFF_SEC(LastDown, Now),
    DownTime = {ElapsedDownTime + Diff, LastDown},
    UpTime = {ElapsedUpTime, Now},
    Node#node_info{
        nodestatus=Status,
        uptime=UpTime,
        downtime=DownTime
    };
update_node(
    #node_info{
        nodestatus={nodeup, _},
        uptime={ElapsedUpTime, LastUp},
        downtime={ElapsedDownTime, LastDown}}=Node,
        {nodedown, _}=Status) when is_record(Node, node_info) ->
    Now = erlang:now(),
    Diff = ?DIFF_SEC(LastUp, Now),
    DownTime = {ElapsedDownTime, LastDown},
    UpTime = {ElapsedUpTime + Diff, Now},
    Node#node_info{
        nodestatus=Status,
        uptime=UpTime,
        downtime=DownTime
    };
update_node(Node, _Status)
    when is_record(Node, node_info) -> Node.

%%
%% gen_server callbacks
%%

%% @hidden
init(_) ->
    process_flag(trap_exit, true),
    HOME = case os:getenv("DXKIT_NET_CONF") of
        false ->
            ".";
        Path ->
            Path
    end,
    SearchPath = [HOME, code:priv_dir(dxkit)],
    case file:path_consult(SearchPath, ".net.conf") of
        {ok, Terms, _} ->
            Host = hostname(),
            InetRC = inet:get_rc(),
            Search = read_conf(search, InetRC),
            {IP, RevisedConf} = case length(Search) > 0 of
                true ->
                    HN = hostname(Host, hd(Search)),
                    PrimIP = prim_ip(HN#h_name.fullname),
                    OrigHosts = read_conf(hosts, Terms),
                    RevisedHosts = 
                        [ (hostname(Host, S))#h_name.fullname || S <- Search ],
                    Updated = lists:keyreplace(hosts, 1, Terms,
                                    {hosts, lists:concat([OrigHosts, RevisedHosts])}),
                    {PrimIP, Updated};
                false ->
                    {prim_ip(Host), Terms}
            end,
            ets:new(dx.net.workers,
                    [named_table, protected,
                    {read_concurrency, erlang:system_info(smp_support)}]),
            %% FIXME: check for blacklist table before creating it....
            DB = case read_conf(blacklist, Terms, disabled) of
                enabled ->
                    Spec = [bag, named_table, protected,
                           {keypos, 4},
                           {read_concurrency,
                                erlang:system_info(smp_support)}],
                    ets:new(dx.net.blacklist, Spec);
                _ ->
                    disabled
            end,
            Conf = 
            [{blacklist_db, DB},
             {hostname, Host},
             {prim_ip, IP},
             {domain, read_conf(domain, InetRC)},
             {search, Search}|RevisedConf],
            {ok, Conf};
        Error ->
            {stop, Error}
    end.

handle_call(get_blacklist, _, State) ->
    {reply, ets:tab2list(dx.net.blacklist), State};
handle_call(clear_blacklist, _, State) ->
    true = ets:delete_all_objects(dx.net.blacklist),
    {reply, ok, State};
handle_call(get_conf, _, State) ->
    {reply, {ok, State}, State};
handle_call(find, From, State) ->
    WorkerPid = spawn_link(start_worker(From, State)),
    fastlog:debug("find ~p INSERT -> dx.net.workers, {~p, ~p}~n", [self(), WorkerPid, From]),
    ets:insert(dx.net.workers, {WorkerPid, From}),
    {reply, ok, State};
handle_call({find, Host}, From, State) ->
    WorkerPid = spawn_link(start_worker(From, {Host, State})),
    fastlog:debug("find-host ~p INSERT -> dx.net.workers, {~p, ~p}~n", 
                    [self(), WorkerPid, From]),
    ets:insert(dx.net.workers, {WorkerPid, From}),
    {reply, ok, State};
handle_call(Msg, {_From, _Tag}, State) ->
    fastlog:debug("In ~p `Call': ~p~n", [self(), Msg]),
    {reply, State, State}.

handle_cast({blacklist, HN}, State) ->
    ets:insert(dx.net.blacklist, HN),
    fastlog:info("Host ~p is now blacklisted!~n", [HN#h_name.fullname]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Worker, normal}, State) ->
    %% worker has completed successfully
    ets:delete(dx.net.workers, Worker),
    {noreply, State};
handle_info({'EXIT', Worker, Reason}, State) ->
    %% TODO: what does it mean if we have no client? Is this part
    %%             of the error kernel for this server?
    Client = ets:lookup(dx.net.workers, Worker),
    ets:delete(dx.net.workers, Worker),
    gen_server:reply(Client, {error, Reason}),
    {noreply, State};
handle_info(Info, State) ->
    fastlog:debug("node ~p unknown status message; state=~p", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

await_response() ->
    receive
        {_Ref, {nodes, Nodes}} ->
            Nodes;
        Other ->
            Other
    end.

read_conf(Key, Conf, Default) ->
    kvc:value(Key, Conf, Default).

read_conf(KeyPath, Conf) when is_atom(KeyPath) orelse is_list(KeyPath) ->
    kvc:path(KeyPath, Conf).

timeout(Host, Domain, Conf) ->
    case read_conf("connect_timeout", Conf, ?DEFAULT_TIMEOUT) of
        Settings when is_list(Settings) ->
            case read_conf(Domain, Settings, []) of
                %% if config is {connect_timeout, [{domain, [some_other_crap]}]}
                %% then we fall back to the default setting
                [] ->
                    ?DEFAULT_TIMEOUT;
                DomainSettings when is_list(DomainSettings) ->
                    read_conf(Host, DomainSettings, ?DEFAULT_TIMEOUT);
                DomainValue ->
                    DomainValue
            end;
        Value ->
            Value
    end.

start_worker(Client, {Host, Conf}) ->
    fun() ->
        Reply = {nodes, lists:flatten(find_nodes(Host, Conf))},
        gen_server:reply(Client, Reply)
    end;
start_worker(Client, Conf) ->
    fun() ->
        NodeList = lists:map(fun(H) -> find_nodes(H, Conf) end,
                             filter_hosts(Conf)),
        gen_server:reply(Client, {nodes, lists:flatten(NodeList)})
    end.

%find_nodes({hostname, Host}) ->
%    find_nodes(Host);
find_nodes(Host, Conf) ->
    HostNames = find_entries(Host, Conf),
    Nodes =
    lists:flatten(lists:map(
        fun(H) ->
            HostName = H#h_name.fullname,
            case net_adm:names(HostName) of
                {ok, Names} ->
                    %% TODO: use list_to_exiting_atom instead
                    [splice_names(Name, "@", HostName)
                                        || {Name, _} <- Names];
                {error, nxdomain} ->
                    []
            end
        end, HostNames)),
    Connections = [net_adm:ping(N) || N <- Nodes],
    NodeStats = lists:zip(Connections, Nodes),
    Found = [N || {Ping, N} <- NodeStats, Ping =:= pong],
    sets:to_list(sets:from_list(Found)).

find_entries(Host, Conf) ->
    Parts = string:tokens(stringify(Host), "."),
    {_, _, Found} = case Parts of
        [_] ->
            %% short host names are searched for in all listed domains
            lists:foldl(fun find_host_entries/2,
                                        {Host, Conf, sets:new()},
                                        read_conf(domains, Conf));
        [HostPart|Rest] ->
            %% fully qualified host entries get checked only once
            DomainPart = rejoin(Rest, "."),
            find_host_entries(DomainPart, {HostPart, Conf, sets:new()})
    end,
    sets:to_list(Found).

find_host_entries(Domain, {Host, Conf, Entries}) ->
    fastlog:debug("Checking domain ~p for hosts named ~p~n", [Domain, Host]),
    Revised = case read_conf(shortnames, Conf) of
        [] ->
            Entries;
        ShortNames ->
            case lists:member(Host, lists:map(fun stringify/1, ShortNames)) of
                true ->
                    sets:add_element(hostname(Host), Entries);
                false ->
                    Entries
            end
    end,
    Name = hostname(Host, Domain),
    case is_blacklisted(Name, Conf) of
        true ->
            fastlog:debug("Blacklisted host ~p ignored~n", [Name#h_name.fullname]),
            {Host, Conf, Entries};
        false ->
            case sets:is_element(Name, Revised) of
                true ->
                    {Host, Conf, Revised};
                false ->
                    Timeout = timeout(stringify(Host), stringify(Domain), Conf),
                    %% TODO: IPv6
                    case inet:gethostbyname(Name#h_name.fullname, inet, Timeout) of
                        {ok, #hostent{h_name=H_Name}} ->
                            %% unreachable hosts are of no interest
                            case gen_tcp:connect(H_Name, epmd_port(), [inet], Timeout) of
                                {error, _} ->
                                    fastlog:debug("Unable to connect to host ~p "
                                                  "- skipping~n", [Host]),
                                    maybe_blacklist(Name, Conf),
                                    {Host, Conf, Revised};
                                {ok, Sock} ->
                                    fastlog:debug("Connected to epmd on ~p~n", [Host]),
                                    ok = gen_tcp:close(Sock),
                                    %% there is a host `Name' on this domain, so...
                                    {Host, Conf, sets:add_element(Name, Revised)}
                            end;
                        {error, _} ->
                            fastlog:debug("No dns resolution for ~p~n", [Name#h_name.fullname]),
                            maybe_blacklist(Name, Conf),
                            {Host, Conf, Revised}
                    end
            end
    end.

is_blacklisted(#h_name{fullname=Name}, Conf) ->
    case read_conf(blacklist, Conf, disabled) of
        disabled ->
            false;
        _ ->
            ets:member(dx.net.blacklist, Name)
    end.

maybe_blacklist(#h_name{}=HN, Conf) ->
    case read_conf(blacklist, Conf, disabled) of
        disabled ->
            disabled;
        _ ->
            gen_server:cast(?MODULE, {blacklist, HN})
    end.

filter_hosts(Conf) ->
    sets:to_list(sets:from_list(read_conf(hosts, Conf))).

hostname(H) ->
    #h_name{host=H, fullname=H}.

hostname(Host, Domain) ->
    HostName = stringify(Host),
    DomainName = stringify(Domain),
    FullName = splice_names(HostName, ".", DomainName),
    #h_name{host=HostName,
            domain=DomainName,
            fullname=FullName}.

splice_names(A, With, B) ->
    list_to_atom(lists:flatten([stringify(A), With, stringify(B)])).

stringify(Term) when is_atom(Term) ->
    atom_to_list(Term);
stringify(Term) when is_binary(Term) ->
    binary_to_list(Term);
stringify(Term) when is_list(Term) ->
    Term.

rejoin(Parts, With) ->
    lists:foldl(
        fun(E, Acc) ->
            case Acc of
                [] ->
                    E;
                _ ->
                    string:join([E, Acc], With)
            end
        end, "", lists:reverse(Parts)).

epmd_port() ->
    %% this seems a bit limiting, as passing -port <num> is all you need
    %% get epmd running on another port - maybe should be configurable
    %% and/or support a range of possible values?
    case os:getenv("ERL_EPMD_PORT") of
        false -> ?DEFAULT_EPMD_PORT;
        PortNum -> list_to_integer(PortNum)
    end.
