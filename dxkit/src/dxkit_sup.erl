%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring: Top Level Supervisor
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

-module(dxkit_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
    WorldArgs = proplists:get_value(world, StartArgs, []),
    Children = [
        {dxkit_event_subsystem, 
            {dxkit_event_sup, start_link, []},
             permanent, 5000, supervisor, [supervisor]},
        {dxkit_net_subsystem,
            {dxkit_net_sup, start_link, [WorldArgs]},
             permanent, infinity, supervisor, [supervisor]},
        {dxkit_monitor_subscription_subsystem,
            {dxkit_subscription_sup, start_link, []},
             permanent, infinity, supervisor, dynamic}
    ],
    {ok, {{one_for_one, 5, 5}, Children}}.
