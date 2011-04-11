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
    io:format("dxkit_sup: Start Args = ~p~n", [StartArgs]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
    WorldArgs = proplists:get_value(world, StartArgs, []),
    io:format("dxkit_sup: WorldArgs = ~p~n", [WorldArgs]),
    Children = [
        {dxkit_net_sup,
            {dxkit_net_sup, start_link, [WorldArgs]},
             permanent, infinity, supervisor, [supervisor]},
        {dxkit_event_handler, 
            {gen_event, start_link, [{local, dxkit_event_handler}]},
             permanent, 5000, worker, dynamic},
        {dxkit_pubsub,
            {dxkit_pubsub, start_link, []},
             permanent, 5000, supervisor, dynamic}
    ],
    {ok, {{one_for_one, 5, 5}, Children}}.
