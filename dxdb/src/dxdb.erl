%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Database: API
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
%% @since: March 2010
%%
%% @doc External API for the database application.
%%
%% -----------------------------------------------------------------------------
-module(dxdb).
-author('Tim Watson <watson.timothy@gmail.com>').
-export([user/2,
         add_user/2,
         check_user/1,
         check_user/2,
         add_subscription/4,
         find_user_subscriptions/1,
         find_user_subscriptions_for_node/2,
         find_all_subscriptions_for_node/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("../include/nodewatch.hrl").

%%
%% @doc Utility function for creating a new user record.
%%
-spec(user(username(), string()) -> #user{}).
user(Name, Password) ->
    <<Digest:160>> = crypto:sha(Password),
    #user{name=Name, password=Digest}.

%%
%% @doc Adds a user to the database.
%%
-spec(add_user(username(), string()) -> ok | {error, term()}).
add_user(Name, Password) ->
    write(user(Name, Password)).

%%
%% @doc Subscribe a user to a specific set of events on a 
%% node, using the specified mode.
%%
add_subscription(#user{ name=Name }, Node, Sensor, Mode) ->
    add_subscription(Name, Node, Sensor, Mode);
add_subscription(User, Node, Sensor, Mode) ->
    %% Incr = mnesia:dirty_update_counter('dxdb.seq', 
    %%                                    'subscription.nextkey', 1),
    write(#subscription{id = {User, Node, Sensor},
                        user = User,
                        node = Node,
                        sensor = Sensor,
                        mode = Mode}).

find_user_subscriptions(UserName) ->
    Q = qlc:q([S || S <- mnesia:table(subscription),
                    S#subscription.user == UserName]),
    transaction(fun() -> qlc:e(Q) end).

find_user_subscriptions_for_node(UserName, Node) ->
    Q = qlc:q([S || S <- mnesia:table(subscription),
                    S#subscription.user == UserName,
                    S#subscription.node == Node]),
    transaction(fun() -> qlc:e(Q) end).

find_all_subscriptions_for_node(Node) ->
    Q = qlc:q([S || S <- mnesia:table(subscription),
                    S#subscription.node == Node]),
    transaction(fun() -> qlc:e(Q) end).

%%
%% @doc Checks a user name and password against the database.
%%
check_user(#user{name=Name, password=Password}) ->
    check_user(Name, Password).

check_user(Name, Password) ->
    <<Digest:160>> = crypto:sha(Password),
    Q = qlc:q([X#user.name || X <- mnesia:table(user), 
                              X#user.name == Name, 
                              X#user.password == Digest]),
    {atomic, Val} = mnesia:transaction(fun() -> qlc:e(Q) end),
    length(Val) == 1.

write(Item) ->
    transaction(fun() -> mnesia:write(Item), Item end).

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Value} ->
            Value;
        {aborted, Reason} ->
            {error, Reason}
    end.
