%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Commons: Library API
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
-module(dxcommon.node_info).
-author('Tim Watson <watson.timothy@gmail.com>').
-compile({parse_transform, exprecs}).

-include("dxcommon.hrl").

-export_records([node_info]).

-export([update_node/2]).

-import(erlang).
-import(timer).

%%
%% @doc When called with Node::atom(), creates a #node_info record representing
%% the state of the specified Node. Otherwise, updates the status of the
%% supplied #node_info record with a up/down time adjusted automatically.
%%
-spec(update_node/2 :: (Node::atom(),
                        Status::nodestatus()) -> #node_info{};
                       (Node::#node_info{}, 
                        Status::nodestatus()) -> #node_info{}).
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
