%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Database: Mnesia Setup (i.e. install)
%%
%% Copyright (c) 2008-2011 Tim Watson (watson.timothy@gmail.com)
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
-include_lib("dxcommon/include/dxcommon.hrl").
-include("dxdb.hrl").

%% API
-export([create_db/0, create_schema/0]).

create_db() ->
    mnesia:create_schema([node()]),
    application:start(mnesia, permanent).

create_schema() ->
    %% ok = create_seq(),
    Tabs = [{user, record_info(fields, user)},
            {subscription, record_info(fields, subscription)}],
    [create_table(T, Attrs) || {T, Attrs} <- Tabs],
    mnesia:wait_for_tables([user, subscription], infinity).

%%create_seq() ->
%%    create_table('dxdb.seq', set, record_info(fields, 'dxdb.seq')),
%%    mnesia:dirty_write(#'dxdb.seq'{ key='subscription.nextkey',
%%                                    nextkey=0 }).

create_table(Name, Attrs) ->
    create_table(Name, ordered_set, Attrs).

create_table(Name, Type, Attrs) ->
    Def = [{type, Type},
           {disc_copies, [node()]},
           {attributes, Attrs}],
    mnesia:create_table(Name, Def),
    mnesia:wait_for_tables([Name], infinity).
