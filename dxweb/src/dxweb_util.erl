%% ------------------------------------------------------------------------------
%%
%% Erlang System Monitoring Dashboard: Utilities Module
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

-module(dxweb_util).
-author('Tim Watson <watson.timothy@gmail.com>').

-export([marshal/1, unmarshal/1, make_uuid/0,
         header/2, parse_cookie/1, cookie_item_fold/2]).

-include("dxweb.hrl").
%% FIXME: is there a way for us *not* to rely on misultin internals here?
-include_lib("misultin/include/misultin.hrl").

%%
%% @doc Serializes the supplied term/data.
%%
marshal([Data]) ->
    %% TODO: write an optimised encoding function for jsx, as it's quite slow.
    jsx:term_to_json(jsonify(Data)).

%%
%% @doc Deserializes the supplied json data.
%%
unmarshal(Data) ->
    jsx:json_to_term(Data).

%%
%% @doc Makes a (reasonably likely to be) unique session id. The ID is 
%% not guaranteed to be unqiue across emulator restarts.
%%
make_uuid() ->
    UUID = erlang:md5(term_to_binary({self(), erlang:make_ref()})),
    base64:encode_to_string(UUID).

header(Header, Req) ->
    misultin_utility:header_get_value(Header, Req:get(headers)).

%% FIXME: Consider moving this into an external library?
parse_cookie(Req) ->
    Raw = Req:raw(),
    case proplists:get_value('Cookie', Raw#req.headers) of
        undefined -> [];
        RawCookies ->
            CookieKeyValues = re:split(RawCookies, "; ", [{return, list}]),
            lists:foldl(fun cookie_item_fold/2, [], CookieKeyValues)
    end.

cookie_item_fold(CookieItem, Acc) ->
    [Key, Value] = re:split(CookieItem, "=", [{return, list}, {parts, 2}]),
    [{Key, Value} | Acc].

%%
%% Internal API
%%

jsonify([{K, V}]) when is_list(V) ->
    {K, lists:map(fun jsonify/1, V)};
jsonify({K, V}) when is_list(V) ->
    {K, list_to_binary(V)};
jsonify({K, {M, F, A}=V}) when is_atom(M), is_atom(F), is_integer(A) ->
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
     {arity, A}].
