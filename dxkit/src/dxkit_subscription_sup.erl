%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: Instrumented Sensor Supervision Tree
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
%% @since: April 2011
%%
%% @doc Manages dynamic subscription to instrumented sensors.
%%
%% -----------------------------------------------------------------------------
-module(dxkit_subscription_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_subscriber/2, remove_subscriber/1]).
-export([init/1]).

-include_lib("dxcommon/include/dxcommon.hrl").

%%
%% API functions
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_subscriber(User, SubscriberKey) when is_list(SubscriberKey) ->
    add_subscriber(User, list_to_atom(SubscriberKey));
add_subscriber(User, SubscriberKey) when is_atom(SubscriberKey) ->
    [ start_child(User, SubscriberKey, Node) || 
        Node <- dxdb:find_subscribed_nodes_by_user(User) ].

remove_subscriber(SubscriberKey) when is_list(SubscriberKey) ->
    remove_subscriber(list_to_atom(SubscriberKey));
remove_subscriber(SubscriberKey) when is_atom(SubscriberKey) ->
    %% NB: even with supervisor2, sending a kill signal to the sensor 
    %% is the wrong thing to do - so we let the sensor kill itself and remove it.
    ok = dxkit_sensor:stop(SubscriberKey),
    supervisor:delete_child(?MODULE, SubscriberKey).

%%
%% Supervisor callbacks
%%

init(_) ->
    %% NB: a simple_one_for_one supervisor wouldn't let us terminate/remove
    %% children so easily, which is why this is a one_for_one with an initially
    %% empty child specification (which is handled dynamically)
    {ok, {{one_for_one, 10, 10}, []}}.

%%
%% Internal API
%%

start_child(User, Subscriber, Node) ->
    %% TODO: get dxdb to collate the node /w sensors in one shot
    Sensors = dxdb:find_instrumented_sensors_for_node(User, Node),
    Consumer = dxkit_event_consumer:new(Subscriber, Node, Sensors),
    ChildSpec = {Consumer:name(),
                {dxkit_sensor, start, [Consumer]},
                 transient, 5000, worker, dynamic},
    supervisor:start_child(?MODULE, ChildSpec).
