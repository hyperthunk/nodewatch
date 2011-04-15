%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Commons
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
%% @doc
%%
%% -----------------------------------------------------------------------------
-module(dxcommon.data).
-author('Tim Watson <watson.timothy@gmail.com>').
-include("dxcommon.hrl").

-compile(export_all).

-import(lists).
-import(dxcommon).

jsonify(Data) when is_record(Data, user) orelse
                   is_record(Data, node_info) orelse
                   is_record(Data, subscription) orelse
                   is_record(Data, connect_time) orelse
                   is_record(Data, event) ->
    [dxcommon:record_to_proplist(Data)];
jsonify(Data=[Rec|_Rest])     
              when is_record(Rec, user) orelse
                   is_record(Rec, node_info) orelse
                   is_record(Rec, subscription) orelse
                   is_record(Data, connect_time) orelse
                   is_record(Data, event) ->
    lists:map(fun(E) -> [dxcommon:record_to_proplist(E)] end, Data);
jsonify([]=L) ->
    L;
jsonify({_K, []}=KV) ->
    KV;
jsonify([{K, V}]) when is_list(V) ->
    {K, lists:map(fun jsonify/1, V)};
jsonify([{_K, _V}|_]=List) ->
    lists:map(fun jsonify/1, List);    
jsonify({K, [H|_]=V}) when is_list(V) andalso is_integer(H) ->
    {K, list_to_binary(V)};
jsonify({K, {M, F, A}=V}) when is_atom(M) andalso is_atom(F) andalso 
                               is_integer(A) ->
    {K, jsonify(V)};
jsonify({K, V}) when is_tuple(V) ->
    {K, lists:map(fun jsonify/1, tuple_to_list(V))};
jsonify({K, V}) when is_atom(V) ->
    {K, atom_to_binary(V, utf8)};
jsonify({K, V}) when is_list(V) ->
    case is_tuple(hd(V)) of
        true ->
            {K, lists:map(fun jsonify/1, V)};
        false ->
            {K, list_to_binary(V)}
    end;
jsonify({M, F, A}) ->
    [{module, atom_to_binary(M, utf8)},
     {function, atom_to_binary(F, utf8)},
     {arity, A}];
jsonify({_K, _V}=Pair) ->
    Pair;
jsonify([H|_]=V) when is_list(V) andalso is_integer(H) ->
    list_to_binary(V);
jsonify(V) when is_atom(V) ->
    atom_to_binary(V, utf8).
