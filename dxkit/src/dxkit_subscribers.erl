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
-module(dxkit_subscribers).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([subscribe/2, unsubscribe/1]).
-export([init/1]).

-include("../include/nodewatch.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

subscribe(User, SubscriberKey) ->
    [ start_child(SubscriberKey, Node, Sensors) || 
        {Node, Sensors} <- [ 
            subscriptions(User, S#subscription.node) ||
                S <- dxdb:find_user_subscriptions(User) ] ].

unsubscribe(SubscriberKey) ->
    ok = dxkit_sensor:stop(SubscriberKey),
    supervisor:delete_child(?MODULE, SubscriberKey).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    %% TODO: review restart frequency settings
    {ok, {{one_for_one, 0, 0}, []}}.

%% 
%% Internal API
%%

subscriptions(User, Node) ->
    Found = dxdb:find_instrumented_sensors_for_node(User, Node),
    {Node, Found}.

start_child(Subscriber, Node, Sensors) ->
    Consumer = dxkit_event_consumer:new(Subscriber, Node, Sensors),
    ChildSpec = {Consumer:name(),  
                {dxkit_sensor, start, [Consumer]},
                 transient, 5000, worker, dynamic},
    supervisor:start_child(?MODULE, ChildSpec).
