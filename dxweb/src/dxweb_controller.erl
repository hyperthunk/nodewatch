%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Web Controller
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
-module(dxweb_controller).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([get/3, get/4]).

get(Req, _SID, "nodes") ->
    respond(Req, dxkit:which_nodes()).

get(Req, _SID, "nodes", NodeId) ->
    respond_with_data(Req, dxkit:find_node(NodeId)).

respond_with_data(Req, []) ->
    not_found(Req);
respond_with_data(Req, Data) ->
    respond(Req, Data).

respond(Req, Data) ->
    Req:ok([{"Content-Type", "application/json"}], 
            dxweb_util:marshal([Data])).

not_found(Req) ->
    Req:respond(404).
