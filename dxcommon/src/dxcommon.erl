%% -----------------------------------------------------------------------------
%%
%% Erlang System Monitoring Commons: Library API
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
-module(dxcommon).
-author('Tim Watson <watson.timothy@gmail.com>').
-include("dxcommon.hrl").

-compile(export_all).

rfc1123_datetime(Now={_,_,_}) ->
    rfc1123_datetime(calendar:now_to_local_time(Now));
rfc1123_datetime(DateTime={{_,_,_}, {_,_,_}}) ->
    httpd_util:rfc1123_date(DateTime).

iso_8601_time({{Year,Month,Day},{Hour,Min,Sec}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

jsonify({_K, []}=KV) ->
    KV;
jsonify([{K, V}]) when is_list(V) ->
    {K, lists:map(fun jsonify/1, V)};
jsonify([{_K, _V}|_]=List) ->
    lists:map(fun jsonify/1, List);    
jsonify({K, [H|_]=V}) when is_list(V) andalso is_integer(H) ->
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
     {arity, A}];
jsonify({_K, _V}=Pair) ->
    Pair.


record_to_proplist(R) ->
    case type(R) of
        {error, _Reason}=Err ->
            Err;
        {Type, Mod} ->
            record_to_proplist(Type, Mod, R)
    end.

record_to_proplist(Type, Mod, R) ->
    Members = Mod:'#info-'(Type, fields),
    PList = lists:zip(Members, [ Mod:'#get-'(M, R) || M <- Members ]),
    {Type, lists:map(fun enforce_fields/1, PList)}.

enforce_fields({Name, Value}=Pair) when is_tuple(Value) ->
    MaybeRecName = element(1, Value),
    case type(MaybeRecName) of
        {error, _} ->
            %% even if this isn't a record, we want a nested proplist
            case Value of
                {_, _}=KVP ->
                    {Name, [jsonify(KVP)]};
                _ ->
                    jsonify(Pair)
            end;
        {Type, Mod} ->
            case record_to_proplist(Type, Mod, Value) of
                {_, PList} ->
                    {Name, PList};
                Other ->
                    %% ????
                    {Name, Other}
            end
    end;
enforce_fields({_Name, _Value}=Pair) ->
    jsonify(Pair).

%% TODO: deprecate this....
new_r(Type, Values) ->
    case type(Type) of
        {error, Reason} ->
            throw(Reason);
        {Type, Mod} ->
            Properties = lists:zip(type_members(Type, Mod), Values),
            apply(Mod, list_to_atom("#new-" ++ atom_to_list(Type)), [Properties])
    end.

new(Type, Properties) ->
    case type(Type) of
        {error, Reason} ->
            throw(Reason);
        {_, Mod} ->
            apply(Mod, list_to_atom("#new-" ++ atom_to_list(Type)), [Properties])
    end.

new(Type) ->
    case type(Type) of
        {error, Reason} ->
            throw(Reason);
        {_, Mod} ->
            case erlang:function_exported(Mod, 'new', 0) of
                true ->
                    Mod:new();
                false ->
                    apply(Mod, list_to_atom("#new-" ++ atom_to_list(Type)), [])
            end
    end.

members(Type) ->
    case type(Type) of
        {error, Reason} ->
            throw(Reason);
        {Type, Mod} ->
            type_members(Type, Mod)
    end.

type_members(Type, Mod) ->
    Mod:'#info-'(Type, fields).

type(node_info) ->
    node_info();
type(subscription) ->
    subscription();
type(user) ->
    user();
type(connect_time) ->
    connect_time();
type(Other) ->
    case lists:filter(fun(Mod) -> Mod:'#is_record-'(Other) end,
                    [dxcommon.connect_time,
                     dxcommon.node_info,
                     dxcommon.subscription,
                     dxcommon.user]) of
        [TypeMod] ->
            [_,B] = string:tokens(atom_to_list(TypeMod), "."),
            {list_to_atom(B), TypeMod};
        [] ->
            {error, {unknown_type, Other}}
    end.

connect_time() ->
    {connect_time, 'dxcommon.connect_time'}.

node_info() ->
    {node_info, 'dxcommon.node_info'}.

subscription() ->
    {subscription, 'dxcommon.subscription'}.

user() ->
    {user, 'dxcommon.user'}.
