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

-include("../include/types.hrl").

-export([force_connect/1, connect/1, update_node/2]).

%%
%% @doc Check the connection status of Node and return a #node_info{} record.
%%
-spec(connect/1 :: (Node::atom()) -> #node_info{};
                   (Node::#node_info{}) -> #node_info{}).
connect(Node) when is_atom(Node) ->
    io:format("Connecting to node '~p'~n", [Node]),
    Status = case net_kernel:connect_node(Node) of
        ignored -> unknown;
        false   -> {nodedown, []};
        true    -> {nodeup, []}
    end,
    io:format("Node '~p' status: ~p~n", [Node, Status]),
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
        {nodeup, _}=Status) when is_record(Node, node_info) ->
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
