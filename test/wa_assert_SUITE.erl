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
-module(wa_assert_SUITE).
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
    assert_equal/1,
    assert_equal_sorted/1,
    assert_match/1,
    assert_comparison/1,
    assert_map_comprehension/1,
    assert_call_with_bindings/1
]).

all() ->
    [
        assert_equal,
        assert_equal_sorted,
        assert_match,
        assert_comparison,
        assert_map_comprehension,
        assert_call_with_bindings
    ].

%%--------------------------------------------------------------------
%% TEST CASES

assert_equal(_Config) ->
    Expected = expected(),
    ?assertException(error, {assertEqual, _}, ?assertEqual(Expected, actual())).

assert_equal_sorted(_Config) ->
    Expected = expected_list(),
    ok = ?assertEqualSorted(Expected, actual_list()).

assert_match(_Config) ->
    Expected = expected(),
    ?assertException(error, {assertMatch, _}, ?assertMatch(Expected, actual())).

assert_comparison(_Config) ->
    Expected = expected(),
    Actual = 42,
    ?assertException(error, {assert, _}, ?assert(Expected > actual(Actual))).

assert_map_comprehension(_Config) ->
    Y = 12,
    ?assertException(
        error,
        {assert, _},
        ?assert(
            #{xy => {X, Y} || X <- []} < 42
        )
    ).

assert_call_with_bindings(_Config) ->
    Expected = expected(),
    ?assertException(error, {assert, _}, ?assert(lists:member(Expected, [actual()]))).

%%--------------------------------------------------------------------
%% Internal Helpers
%%--------------------------------------------------------------------
actual() -> 42.
expected() -> 24.

actual(X) -> X.

actual_list() -> [1, 2, 3, 4, 5].
expected_list() -> [1, 5, 3, 2, 4].
