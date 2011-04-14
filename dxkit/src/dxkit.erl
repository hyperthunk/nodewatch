%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: API Module
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
%% @doc DxKit API Module.
%%
%% -----------------------------------------------------------------------------

-module(dxkit).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([activate_subscriptions/2, disable_subscriptions/1]).
-export([add_event_sink/1, add_event_sink/2, add_event_sink/3]).
-export([which_nodes/0, find_node/1]).
-export([start_dev/0]).

%% 
%% Public API
%%

%%
%% @doc Activates all subscriptions for the given user.
%%
activate_subscriptions(User, Key) ->
    dxkit_subscription_sup:add_subscriber(User, Key).

%%
%% @doc Disables all subscriptions for the given user - the opposite
%% of activate_subscriptions/2.
%%
disable_subscriptions(Key) ->
    dxkit_subscription_sup:remove_subscriber(Key).

find_node(NodeId) ->
    dxkit_world_server:node(NodeId).

%%
%% @doc Returns all the nodes that are currently being monitored.
%%
which_nodes() ->
    dxkit_world_server:nodes().

add_event_sink(Dest) when is_pid(Dest) ->
    tbc.

add_event_sink(Mod, Func) ->
    dxkit_event_bridge:add_subscriber(Mod, Func, [[]]).

add_event_sink(Mod, Func, ArgSpec) ->
    dxkit_event_bridge:add_subscriber(Mod, Func, ArgSpec).

%%
%% @doc Starts the dxkit OTP application in dev mode. This is only intended 
%% for use whne you're running nodewatch from the start-dev shell script.
%%
start_dev() ->
    appstart:start_deps(dxkit),
    fastlog:set_level(info),
    application:start(dxkit),
    fastlog:debug("Loading in ~p~n", [file:get_cwd()]),
    {atomic, ok} = mnesia:load_textfile("../inttest/testdb.conf"),
    ok.
