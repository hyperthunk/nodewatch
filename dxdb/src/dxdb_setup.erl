%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Database: Top Level Supervisor
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

-module(dxdb_setup).
-author('Tim Watson <watson.timothy@gmail.com>').
-include("../include/types.hrl").

%% API
-export([create_db/0, create_schema/0]).

create_db() ->
    mnesia:create_schema([node()]),
    application:start(mnesia, permanent).

create_schema() ->
    Tabs = [{user, record_info(fields, user)}, 
            {subscription, record_info(fields, subscription)}],
    [create_table(T, Attrs) || {T, Attrs} <- Tabs].

create_table(Name, Attrs) ->
    Def = [{type, ordered_set},
           {disc_copies, [node()]},
           {attributes, Attrs}],
    mnesia:create_table(Name, Def),
    mnesia:wait_for_tables([Name], infinity).
