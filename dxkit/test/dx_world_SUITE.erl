%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Tools: World Server Test Suite
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

%% module annotations
-module(dx_world_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLAVE, esmt_test_slave).
-define(PORT_HINT, 10100).

%% public api exports

%% automatically registers all exported functions as test cases
all() ->
    [ FName || {FName, _} <- lists:filter(
    fun ({module_info,_}) -> false ;
        ({all,_}) -> false ;
        ({init_per_suite,1}) -> false ;
        ({end_per_suite,1}) -> false ;
        ({_,1}) -> true ;
        ({_,_}) -> false
    end,
    ?MODULE:module_info(exports))].

init_per_suite(Config) ->
    [Self] = dxkit_net:find_nodes(dxkit_net:hostname()),
    [A,B] = string:tokens(atom_to_list(Self), "@"),
    Host = list_to_atom(B),
    ct:pal("initializing with host=~p~n", [Host]),
    [{host, Host}|Config].

end_per_suite(_) ->
    ok.

%% sets up automated trace configuration (see test_config.ctc for details)
init_per_testcase(TestCase, Config) ->
    Host = ?config(host, Config),
    ct:pal("starting slave on ~p~n", [Host]),
    {ok, Node} = slave:start(Host, TestCase),
    TC = {TestCase,[{slave, dxkit_net:force_connect(Node)}]},
    [TC|Config].

end_per_testcase(TestCase, Config) ->
    Host = ?config(host, Config),
    Node = list_to_atom(atom_to_list(TestCase) ++ "@" ++ atom_to_list(Host)),
    slave:stop(Node).

nodedown_messages_should_get_through(Config) ->
    ct:pal("nodes = ~p~n", [dxkit:which_nodes()]),
    ok.
