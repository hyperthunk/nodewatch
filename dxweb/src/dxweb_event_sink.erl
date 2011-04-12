%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Event Sink
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
%% @doc Attaches to the dxkit event bridge, proxying events to the right session
%%
%% -----------------------------------------------------------------------------
-module(dxweb_event_sink).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/0, stop/0]).
-export([forward_event/1]).

%%
%% Public API
%%

%%
%% @doc Starts and registers the database subsystem
%%
start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _Pid}=Ok ->
            dxkit:add_event_sink(?MODULE, forward_event),
            Ok;
        Other ->
            Other
    end.

%%
%% @doc Explicitly stops the server.
%%
stop() ->
    gen_server:cast(?MODULE, stop).

%% @hidden
forward_event(Event) ->
    gen_server:call(?MODULE, Event).

%%
%% gen_server callbacks
%%

%% @hidden
init(_) ->
    {ok, []}.

%% @hidden
handle_call({world, {_NodeStatus, _NodeInfo}=Ev}, _From, State) ->
    %% TODO: fix this so that somewhere (?) we serialised the record properly
    dxweb_session:send_all([Ev]),
    {noreply, State};
handle_call({ID, _, Event}, _From, State) ->
    dxweb_session:send(ID, Event),
    {noreply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    terminated.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
