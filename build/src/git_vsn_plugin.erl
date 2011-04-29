%% -----------------------------------------------------------------------------
%%
%% Nodewatch: Git Version (Tagging) Plugin
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
%% @since: April 2010
%%
%% @doc Custom rebar build plugin - produces priv/vsn.git files with the current
%% git tag but minus the commit details. I do this because I don't like my OTP
%% applications with versions like `0.0.2-19-gc1f422f'.
%% -----------------------------------------------------------------------------
-module(git_vsn_plugin).

-export([tag/2, vsn/0]).

-include("build.hrl").

tag({_, Dir, _}, _) ->
    Path = filename:join([Dir, "priv"]),
    case filelib:is_dir(Path) of
        false ->
            ?DEBUG("Skipping tag command in non existent dir ~s~n", [Path]),
            ok;
        true ->
            Dest = filename:join([Path, "vsn.git"]),
            ?DEBUG("Templating tag file ~p~n", [Dest]),
            Vsn = vsn(),
            file:write_file(Dest, list_to_binary(Vsn))
    end.

vsn() ->
    Vsn = os:cmd("git describe --abbrev=0"),
    string:strip(Vsn, right, $\n).
