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
-module(dx_utils).
-author('Tim Watson <watson.timothy@gmail.com>').
-include("dxcommon.hrl").

-compile(export_all).

rejoin(Parts, With) ->
    lists:foldl(
        fun(E, Acc) ->
            case Acc of
                [] ->
                    E;
                _ ->
                    string:join([E, Acc], With)
            end
        end, "", lists:reverse(Parts)).

rfc1123_datetime(Now={_,_,_}) ->
    rfc1123_datetime(calendar:now_to_local_time(Now));
rfc1123_datetime(DateTime={{_,_,_}, {_,_,_}}) ->
    httpd_util:rfc1123_date(DateTime).

snapshot() ->
    Now = calendar:now_to_universal_time(erlang:now()),
    calendar:datetime_to_gregorian_seconds(Now).
