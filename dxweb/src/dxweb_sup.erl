%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Top Level Supervisor
%%
%% Copyright (c) 2008-2010 Tim Watson (watson.timothy@gmail.com)
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
%%
%% Our supervision tree covers three main areas:
%%    1. HTTP Infrastructure
%%    2. Gathering Performance Counters from Eper
%%    3. SubSystem Event Notification Bridge
%%
%% The HTTP supervision tree is maintained by the virtual dxweb_http_server,
%% which is itself a supervisor. The Eper collector and associated processes
%% are hosted seperately by a tree of gen_servers. Finally, the notification
%% bridge handles the pub/sub between the collected performance counters and
%% the HTTP *sessions* maintained for each (browser or other type of ) client.
%%
%% -----------------------------------------------------------------------------

-module(dxweb_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StartArgs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
    Children = [
        {dxweb_http_server,
            {dxweb_http_server, start_link,
                [proplists:get_value(webconfig, StartArgs)]},
            permanent, 5000, supervisor, [supervisor]},
        {dxdb,
            {dxdb_sup, start_link, []},
            permanent, 5000, supervisor, [supervisor]}
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
