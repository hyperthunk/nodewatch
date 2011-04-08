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

-include_lib("kernel/include/inet.hrl").
-include("../include/types.hrl").
-include("dxkit.hrl").

-compile(export_all).
%-export([force_connect/1
%        ,connect/1
%        ,update_node/2
%        ,find_nodes/1]).

-record(h_name, {host, domain, fullname}).

-define(DEFAULT_EPMD_PORT, 4369).
-define(DEFAULT_TIMEOUT, 5000).

get_conf() ->
    Home = case init:get_argument(home) of
        {ok, [[H]]} -> [H];
        _ ->
            case os:getenv("HOME") of
                false ->
                    [];
                Path ->
                    Path
            end
    end,
    SearchPath = [".", Home, code:priv_dir(dxkit)],
    case file:path_consult(SearchPath, ".net.conf") of
        {ok, Terms, _} ->
            Host = hostname(),
            InetRC = inet:get_rc(),
            Search = read_conf(search, InetRC),
            IP = case length(Search) > 0 of
                true ->
                    HN = hostname(Host, Search),
                    prim_ip(HN#h_name.fullname);
                false ->
                    prim_ip(Host)
            end,
            [{hostname, Host}, 
             {prim_ip, IP},
             {domain, read_conf(domain, InetRC)},
             {search, Search}|Terms];
        Error ->
            Error
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

hostname() ->
    {ok, Name} = inet:gethostname(), Name.

prim_ip(Host) ->
    case inet:ip(Host) of
        {ok, IP} -> IP;
        Err -> Err
    end.

get_hosts() ->
    read_conf(hosts, get_conf()).

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

%% Mod:start([{refresh, {20, seconds}}, {nodes, [s1@malachi, s2@malachi]}]).
%% Args =
%%            [{node_info,s2@malachi,
%%                 {nodeup,[]},
%%                 {0.0,{1274,431579,810451}},
%%                 {0.0,{1274,431579,810451}}},
%%             {nodeup,[]}].

%%
%% @doc Find all connect-able nodes. This function is equiv.
%% to net_adm:world/0, but is fasteras it avoids needless connection
%% attempts when hosts are inaccessible and uses a short (5 second)
%% timeout when attempting to locate epmd on the target machine.
%%
find_nodes() ->
    Conf = get_conf(),
    NodeList = lists:map(fun(H) -> find_nodes(H, Conf) end,
                         filter_hosts(Conf)),
    lists:flatten(NodeList).

%%
%% @doc Find all connect-able nodes on Host. This function is similar to
%% net_adm:names/1, but is faster as it avoids needless connection
%% attempts when hosts are inaccessible and uses a short (5 second)
%% timeout when attempting to locate epmd on the target machine.
%%
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
    {_, _, Found} = lists:foldl(fun find_host_entries/2,
                                {Host, Conf, sets:new()},
                                read_conf(domains, Conf)),
    sets:to_list(Found).

find_host_entries(Domain, {Host, Conf, Entries}) ->
    Revised = case read_conf(shortnames, Conf) of
        [] ->
            Entries;
        ShortNames ->
            fastlog:debug("Appending shortnames ~p~n", [ShortNames]),
            case lists:member(Host, lists:map(fun stringify/1, ShortNames)) of
                true -> sets:add_element(hostname(Host), Entries);
                false -> Entries
            end
    end,
    Name = hostname(Host, Domain),
    fastlog:debug("hostname set to ~p~n", [Name]),
    case sets:is_element(Name, Revised) of
        true ->
            {Host, Conf, Revised};
        false ->
            Timeout = timeout(stringify(Host), stringify(Domain), Conf),
            fastlog:debug("Timeout for ~p set to ~p~n", [Host, Timeout]),
            %% TODO: IPv6
            case inet:gethostbyname(Name#h_name.fullname, inet, Timeout) of
                {ok, #hostent{h_name=H_Name}} ->
                    %% unreachable hosts are of no interest
                    case gen_tcp:connect(H_Name, epmd_port(), [inet], Timeout) of
                        {error, _} ->
                            fastlog:debug("Unable to connect to host ~p "
                                          "- skipping~n", Host),
                            {Host, Conf, Revised};
                        {ok, Sock} ->
                            fastlog:debug("Connected to epmd on ~p~n", [Host]),
                            ok = gen_tcp:close(Sock),
                            %% there is a host `Name' on this domain, so...
                            {Host, Conf, sets:add_element(Name, Revised)}
                    end;
                {error, _} ->
                    fastlog:debug("No dns resolution for ~p~n", [Name#h_name.fullname]),
                    {Host, Conf, Revised}
            end
    end.

filter_hosts(Conf) ->
    sets:to_list(sets:from_list(read_conf(hosts, Conf))).

%    {Hosts, _} = lists:foldl(fun uniq_hosts/2,
%                            {Conf, [], []},
%                            read_conf(hosts, Conf)),
%    Hosts.

nodename(Name, {hostname, Host}) when is_list(Name), is_atom(Host) ->
    list_to_atom(Name ++ "@" ++ atom_to_list(Host));
nodename(Name, {hostname, Host}) when is_list(Name), is_list(Host) ->
    list_to_atom(Name ++ "@" ++ Host);
nodename(Name, Host) when is_list(Name), is_list(Host) ->
    {hostname, HostName} = hostname(Host),
    list_to_atom(Name ++ "@" ++ HostName).

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

epmd_port() ->
    %% this seems a bit limiting, as passing -port <num> is all you need
    %% get epmd running on another port - maybe should be configurable
    %% and/or support a range of possible values?
    case os:getenv("ERL_EPMD_PORT") of
        false -> ?DEFAULT_EPMD_PORT;
        PortNum -> list_to_integer(PortNum)
    end.
