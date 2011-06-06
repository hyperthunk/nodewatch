%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Kit: Bridge between event manager and handlers
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
%% ------------------------------------------------------------------------------
%%
%% @doc 
%%
%% ------------------------------------------------------------------------------

-module(dxkit_event_bridge).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([permanent_subscriber/2, 
         poke_subscribers/0, publish_event/1]).

-include_lib("fastlog_parse_trans/include/fastlog.hrl").

%%
%% Public API
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

permanent_subscriber(Handler, Args) ->
    ?DEBUG("Starting Handler ~p~n", [Handler]),
    supervisor:start_child(?MODULE, [Handler, Args]).

poke_subscribers() ->
    gen_event:notify(dxkit_event_handler, poke).

publish_event(Event) ->
    gen_event:notify(dxkit_event_handler, Event).

%%
%% Supervisor callbacks
%%

init(_) ->
    ChildTemplate = [{'_',
        {dxkit_event_handler_bridge, start_link, []},
         transient, 5000, worker, [gen_server]}],
    {ok, {{simple_one_for_one, 10, 10}, ChildTemplate}}.
