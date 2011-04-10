%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: Type Definitions Header
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
-type(nodestatus()      :: unknown | {nodeup | nodedown, nodeinfo()}).
-type(mode()            :: active | passive).
-type(sensor()          :: atom()).
-type(username()        :: string()).

-record(node_info, {
    node_name                   :: node(),
    nodestatus  = unknown       :: nodestatus(),
    uptime      = {0,{0,0,0}}   :: conn_time(),
    downtime    = {0,{0,0,0}}   :: conn_time()
}).

%% Represents a subscription - these are *only* stored against a specific user
-record(subscription, {
    id                  :: integer(),
    user                :: {user, username()},
    mode    = passive   :: mode(),
    sensor  = undefined :: sensor()
}).

%% User is an aggregate root in our domain model
-record(user, {
    name                :: username(),
    password            :: string()
}).

%% The named event handler registered by dxdb_sup on startup.
-define(DB_EVENT, dxdb_event_handler).
