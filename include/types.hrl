%% -------------------------------------------------------------------
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
%% -------------------------------------------------------------------

-type timestamp()  :: {integer(), integer(), integer()}. %% see erlang:now/0

-type conn_time()  :: {number(), timestamp()}.

-type nodeinfo()   :: [{atom(), term()}].

-type nodestatus() :: unknown | {nodeup | nodedown, nodeinfo()}.

-record(node_info, {
    node_name                   :: node(),
    nodestatus  = unknown       :: nodestatus(),
    uptime      = {0,{0,0,0}}   :: conn_time(),
    downtime    = {0,{0,0,0}}   :: conn_time()
}).

-define(TS(Start, End), {?DIFF_SEC(Start, End), End}).
-define(TS_EMPTY, {0,0,0}).
-define(DIFF_SEC(T1,T2), ((timer:now_diff(T2, T1) * 0.001) / 1000)).