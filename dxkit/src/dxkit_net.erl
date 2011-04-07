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

-define(DEFAULT_EPMD_PORT, 4369).

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
    lists:flatten(lists:map(fun find_nodes/1, filter_hosts())).

%% 
%% @doc Find all connect-able nodes on Host. This function is similar to
%% net_adm:names/1, but is faster as it avoids needless connection
%% attempts when hosts are inaccessible and uses a short (5 second) 
%% timeout when attempting to locate epmd on the target machine.
%%
find_nodes({hostname, Host}) ->
    find_nodes(Host);
find_nodes(Host) ->
    Nodes = case net_adm:names(Host) of
        {ok, Names} ->
            %% TODO: use list_to_exiting_atom instead
            [nodename(Name, Host) || {Name, _} <- Names];
        {error, nxdomain} -> 
            []
    end,
    Connections = [net_adm:ping(N) || N <- Nodes],
    NodeStats = lists:zip(Connections, Nodes),
    Found = [N || {Ping, N} <- NodeStats, Ping =:= pong],
    sets:to_list(sets:from_list(Found)).

filter_hosts() ->
    {Hosts, _} = lists:foldl(fun uniq_hosts/2, {[], []}, net_adm:host_file()),
    Hosts.

uniq_hosts(Host, {Hosts, Addrs}=Found) ->
    case inet:gethostbyname(Host) of
        {ok, #hostent{h_addr_list=IPs}} ->
            case lists:any(fun(IP) -> lists:member(IP, Addrs) end, IPs) of
                true -> Found;
                _ ->
                    %% FIXME: check to IPv6
                    case gen_tcp:connect(Host, epmd_port(), [inet], 5000) of
                        {error, _} -> Found;
                        {ok, Sock} ->
                            ok = gen_tcp:close(Sock),
                            Name = case inet:peername(Sock) of
                                {ok,{{127,0,0,1},_}} ->
                                    hostname(node_hostname());
                                _ -> 
                                    hostname(Host)
                            end,
                            case Name of 
                                {einvalid_hostname, _} -> 
                                    Found;
                                HostName ->
                                    {[HostName|Hosts], lists:concat([IPs, Addrs])}
                            end
                    end
            end;
        {error, _} -> 
            Found
    end.

nodename(Name, {hostname, Host}) when is_list(Name), is_atom(Host) ->
    list_to_atom(Name ++ "@" ++ atom_to_list(Host));
nodename(Name, {hostname, Host}) when is_list(Name), is_list(Host) ->
    list_to_atom(Name ++ "@" ++ Host);
nodename(Name, Host) when is_list(Name), is_list(Host) ->
    {hostname, HostName} = hostname(Host),
    list_to_atom(Name ++ "@" ++ HostName).

hostname(Host) when is_atom(Host) ->
    hostname(atom_to_list(Host));
hostname(Host) when is_list(Host) ->
    Parts = string:tokens(Host, "."),
    case net_kernel:longnames() of
        false -> 
            fastlog:error("** dxkit_net does not support shortnames! **", Host),
            {einvalid_hostname, Host};
        true -> 
            case Parts of
                [Host] -> 
                    fastlog:warn("** System running to use fully qualified hostnames **~n" 
                                 "** Hostname ~p is illegal **", Host),
                    {einvalid_hostname, Host};
                [_HostPart|_DomainParts] -> 
                    {hostname, Host}
            end
    end.

node_hostname() ->
    node_hostname(node()).

node_hostname(Node) ->
    [_,Hostname] = string:tokens(atom_to_list(Node), "@"),
    Hostname.

shortname([$.|_]) -> [];
shortname([])-> [];
shortname([H|T]) -> [H|shortname(T)].

epmd_port() ->
    %% this seems a bit limiting, as passing -port <num> is all you need
    %% get epmd running on another port - maybe should be configurable
    %% and/or support a range of possible values? 
    case os:getenv("ERL_EPMD_PORT") of
        false -> ?DEFAULT_EPMD_PORT;
        PortNum -> list_to_integer(PortNum)
    end.
