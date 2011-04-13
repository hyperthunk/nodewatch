%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Supervisor for HTTP Services
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

-module(dxweb_http_server).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    io:format("~p StartArgs: ~p~n", [?MODULE, StartArgs]),    
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(StartArgs) ->
    io:format("~p init StartArgs: ~p~n", [?MODULE, StartArgs]),
    Children = [
        {dxweb_http_handler,
            {dxweb_http_handler, start_link, [StartArgs]},
            permanent, 5000, worker, [gen_server]},
        {dxweb_session,
            {dxweb_session, start_link, []},
            permanent, 5000, worker, [gen_server]}
    ],
    %% NB: these guys should go down together, if at all
    {ok, {{one_for_all, 10, 10}, Children}}.
