%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Session (utilities) API
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
%% @doc Placeholder for a websocket session, yet to be established.
%% All functions crash the calling process.
%% ------------------------------------------------------------------------------

-module(dxweb_invalid_websocket).
-author('Tim Watson <watson.timothy@gmail.com>').
-export([raw/0, get/1, send/1]).

%% TODO: Use dynapi for this instead of doing the boilerplate by hand

raw() ->
    exit(no_websocket).

get(_) ->
    exit(no_websocket).

send(_) ->
    exit(no_websocket).
