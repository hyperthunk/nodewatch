%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: Common Type Definitions Header
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

-type(timestamp()       :: {integer(), integer(), integer()}). %% see erlang:now/0
-type(conn_time()       :: {number(), timestamp()}).
-type(interval()        :: integer()).
-type(unit_of_measure() :: seconds | minutes | hours | milliseconds).
-type(nodeinfo()        :: [{atom(), term()}]).
-type(nodestatus()      :: unknown | nodeup | nodedown).
-type(sample_rate()     :: integer() | {integer(), unit_of_measure()}).
-type(mode()            :: instrument | {sample, sample_rate()}).
-type(sensor()          :: atom()).
-type(username()        :: string()).

-record(connect_time, {
    elapsed  = 0 :: integer(),
    snapshot = 0 :: integer()
}).

-record(node_info, {
    node_name                       :: node(),
    status      = unknown           :: nodestatus(),
    info        = []                :: nodeinfo(),
    uptime      = #connect_time{}   :: #connect_time{},
    downtime    = #connect_time{}   :: #connect_time{}
}).

%% Represents a subscription for a specific user on a specific node
-record(subscription, {
    id                  :: {username(), node(), sensor()},
    user                :: {user, username()},
    node                :: node(),
    sensor  = undefined :: sensor(),
    mode                :: mode()
}).

%% User is an aggregate root in our domain model
-record(user, {
    name                :: username(),
    password            :: string()
}).

%% The named event handler registered by dxdb_sup on startup.
-define(DB_EVENT, dxdb_event_handler).

-define(TS(Start, End), {?DIFF_SEC(Start, End), End}).
-define(TS_EMPTY, {0,0,0}).
-define(DIFF_SEC(T1,T2), ((timer:now_diff(T2, T1) * 0.001) / 1000)).

