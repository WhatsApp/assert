%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% % @format
-module(wa_blame_SUITE).
-typing([eqwalizer]).
%% erlfmt:ignore
% @fb-only

-include("assert.hrl").

%% Test server callbacks
-export([
    all/0
]).

%% Test cases
-export([
    function_clause/1,
    function_clause_multiple_args/1,
    function_clause_binding/1,
    function_clause_with_guards/1,
    function_clause_mixed_guards/1,
    function_clause_mixed_guards_2/1,
    function_clause_map/1,
    function_clause_map_2/1,
    badmatch_map/1,
    badmatch_map_nested/1,
    badmatch_map_with_bindings/1,
    badmatch_tuple/1,
    badmatch_macro/1,
    badmatch_record_expr/1,
    badmatch_record_expr_non_exhaustive/1,
    format_error/1
]).

-define(BLAME(CALL),
    try
        CALL
    catch
        error:Reason:S ->
            {ok, Blame} = wa_blame:blame(Reason, S),
            wa_blame:pp_blame(Blame, <<"-">>, <<"-">>)
    end
).

-define(FORMAT_ERROR(CALL),
    attempt_format_error(fun() ->
        attempt_error_info(fun() ->
            CALL
        end)
    end)
).

all() ->
    [
        function_clause,
        function_clause_multiple_args,
        function_clause_binding,
        function_clause_with_guards,
        function_clause_mixed_guards,
        function_clause_mixed_guards_2,
        function_clause_map,
        function_clause_map_2,
        badmatch_map,
        badmatch_map_nested,
        badmatch_map_with_bindings,
        badmatch_tuple,
        badmatch_macro,
        badmatch_record_expr,
        badmatch_record_expr_non_exhaustive,
        format_error
    ].

%%--------------------------------------------------------------------
%% TEST CASES

function_clause(_Config) ->
    ?assertEqual(
        {ok, "(-1-)\n(-3-)"},
        ?BLAME(helper(42))
    ).

function_clause_multiple_args(_Config) ->
    ?assertEqual(
        {ok, "(-1-)\n(-3-)"},
        ?BLAME(helper(42))
    ).

function_clause_binding(_Config) ->
    ?assertEqual(
        {ok,
            "(X, -X-)\n"
            "(X, Y) when -is_list(X)-, is_list(Y)"},
        ?BLAME(helper_with_binding(24, 42))
    ).

function_clause_with_guards(_Config) ->
    ?assertEqual(
        {ok,
            "({X, Y}) when is_integer(X), -is_list(Y)-\n"
            "({X, Y}) when -is_list(X)-, is_integer(Y)\n"
            "(-{X, Y, Z}-) when is_integer(X), is_integer(Y), is_integer(Z)\n"
            "({X, Y}) when -is_list(X)-, is_list(Y)"},
        ?BLAME(helper_with_guards({1, 2}))
    ).

function_clause_mixed_guards(_Config) ->
    ?assertEqual(
        {ok,
            "({X, -X-})\n"
            "({X, Y}) when -is_integer(X)-, is_list(Y); -is_integer(Y)-, is_atom(X)\n"
            "({X, Y}) when -is_integer(X)-, X > 0; -is_integer(Y)-"},
        ?BLAME(helper_mixed_guards({"1", "2"}))
    ).

function_clause_mixed_guards_2(_Config) ->
    ?assertEqual(
        {ok,
            "({X, -X-})\n"
            "({X, Y}) when is_integer(X), -is_list(Y)-; -is_integer(Y)-, is_atom(X)\n"
            "({X, Y}) when is_integer(X), -X > 0-; -is_integer(Y)-"},
        ?BLAME(helper_mixed_guards({-1, foo}))
    ).

function_clause_map(_Config) ->
    ?assertEqual(
        {ok, "(#{-a := _-})"},
        ?BLAME(helper_map(#{b => 1}))
    ).

function_clause_map_2(_Config) ->
    ?assertEqual(
        {ok, "(#{a := _, b := -3-})"},
        ?BLAME(helper_map_2(#{a => 12, b => 1}))
    ).

badmatch_map(_Config) ->
    ?assertEqual(
        {ok, "#{a := _, b := -2-}"},
        ?BLAME(?assertMatch(#{a := _, b := 2}, #{a => 1, b => 3}))
    ).

badmatch_map_nested(_Config) ->
    ?assertEqual(
        {ok, "#{a := _, b := #{sub_a := _, sub_b := -2-}}"},
        ?BLAME(?assertMatch(#{a := _, b := #{sub_a := _, sub_b := 2}}, #{a => 1, b => #{sub_a => 2, sub_b => 3}}))
    ).

badmatch_map_with_bindings(_Config) ->
    X = 2,
    ?assertEqual(
        {ok, "#{a := _, b := #{sub_a := _, sub_b := -X-}}"},
        ?BLAME(?assertMatch(#{a := _, b := #{sub_a := _, sub_b := X}}, #{a => 1, b => #{sub_a => 2, sub_b => 3}}))
    ).

badmatch_tuple(_Config) ->
    ?assertEqual(
        {ok, "{tuple, #{a := _, b := -2-}}"},
        ?BLAME(?assertMatch({tuple, #{a := _, b := 2}}, {tuple, #{a => 1, b => 3}}))
    ).

badmatch_macro(_Config) ->
    ?assertEqual(
        {ok, "{-another_macro-, _, wa_blame_SUITE}"},
        ?BLAME(?assertMatch({another_macro, _, ?MODULE}, {some_macro, 42, ?MODULE}))
    ).

-record(person, {name :: string(), age :: integer()}).
badmatch_record_expr(_Config) ->
    P = #person{name = "John", age = 42},
    ?assertEqual(
        {ok, "{person, _, -38-}"},
        ?BLAME(?assertMatch(#person{name = _, age = 38}, P))
    ).

badmatch_record_expr_non_exhaustive(_Config) ->
    P = #person{name = "John", age = 42},
    ?assertEqual(
        {ok, "{person, _, -38-}"},
        ?BLAME(?assertMatch(#person{age = 38}, P))
    ).

format_error(_Config) ->
    ?assertEqual(
        "\n\n"
        "The following arguments were given to the wa_blame_SUITE:helper/2 function:\n\n"
        "\t* 2\n"
        "\t* three\n\n"
        "But they did not match the following patterns:\n\n"
        "(\e[41m1\e[0m, \e[41mone\e[0m)\n"
        "(2, \e[41mtwo\e[0m)\n"
        "(\e[41m3\e[0m, three)\n",
        ?FORMAT_ERROR(helper(2, three))
    ).

%%--------------------------------------------------------------------
%% Internal Helpers
%%--------------------------------------------------------------------
helper(1) -> ok;
helper(3) -> ok.

helper(1, one) -> ok;
helper(2, two) -> ok;
helper(3, three) -> ok.

helper_with_binding(X, X) -> ok;
helper_with_binding(X, Y) when is_list(X), is_list(Y) -> ok.

helper_with_guards({X, Y}) when is_integer(X), is_list(Y) -> ok;
helper_with_guards({X, Y}) when is_list(X), is_integer(Y) -> ok;
helper_with_guards({X, Y, Z}) when is_integer(X), is_integer(Y), is_integer(Z) -> ok;
helper_with_guards({X, Y}) when is_list(X), is_list(Y) -> ok.

helper_mixed_guards({X, X}) ->
    ok;
helper_mixed_guards({X, Y}) when
    is_integer(X), is_list(Y);
    is_integer(Y), is_atom(X)
->
    ok;
helper_mixed_guards({X, Y}) when is_integer(X), X > 0; is_integer(Y) -> ok.

helper_map(#{a := _}) -> ok.

helper_map_2(#{a := _, b := 3}) -> ok.

attempt_error_info(Fun) ->
    try
        Fun()
    catch
        error:Reason:S ->
            case wa_blame:error_info(Reason, S) of
                {ok, ErrorInfo} ->
                    erlang:error(Reason, none, [{error_info, ErrorInfo}]);
                {error, no_error_info} ->
                    erlang:error(Reason)
            end
    end.

attempt_format_error(Fun) ->
    try
        Fun()
    catch
        error:R:S ->
            #{reason := Reason} = wa_blame:format_error(R, S),
            lists:flatten(io_lib:format("~s", [Reason]))
    end.
