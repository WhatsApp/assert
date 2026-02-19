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
%% erlfmt:ignore
% @fb-only: -oncall("whatsapp_server_devx").

-include_lib("assert/include/assert.hrl").

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
    assert_call_with_bindings/1,
    assert_greater_than/1,
    assert_less_than/1,
    assert_greater_than_or_equal/1,
    assert_less_than_or_equal/1,
    assert_in_range_inclusive/1,
    assert_in_range_exclusive/1,
    assert_equal_with_delta/1,
    assert_comparison_macros_preserve_error_info/1,
    maybe_format_comment/1,
    format_comment_integration/1,
    format_where/1
]).

all() ->
    [
        assert_equal,
        assert_equal_sorted,
        assert_match,
        assert_comparison,
        assert_map_comprehension,
        assert_call_with_bindings,
        assert_greater_than,
        assert_less_than,
        assert_greater_than_or_equal,
        assert_less_than_or_equal,
        assert_in_range_inclusive,
        assert_in_range_exclusive,
        assert_equal_with_delta,
        assert_comparison_macros_preserve_error_info,
        maybe_format_comment,
        format_comment_integration,
        format_where
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
%% Comparison Assertion Tests
%%
%% Should be able to compare:
%% - Timestamps (large integers)
%% - Counts (small non-negative integers)
%% - Percentages and floats
%% - Negative numbers
%%--------------------------------------------------------------------

assert_greater_than(_Config) ->
    %% Basic integer comparisons
    ok = ?assertGreaterThan(10, 5),
    ok = ?assertGreaterThan(100, 0),
    %% Timestamp comparisons (large integers)
    Ts1 = 1737471235,
    Ts2 = 1737471234,
    ok = ?assertGreaterThan(Ts1, Ts2),
    %% Negative numbers
    ok = ?assertGreaterThan(-5, -10),
    %% Floats
    ok = ?assertGreaterThan(1.5, 1.0),
    ok = ?assertGreaterThan(0.001, 0.0),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertGreaterThan(5, 10)),
    ?assertException(error, {assert, _}, ?assertGreaterThan(10, 10)),
    %% With comment
    ok = ?assertGreaterThan(10, 5, "should be greater").

assert_less_than(_Config) ->
    %% Basic integer comparisons
    ok = ?assertLessThan(5, 10),
    ok = ?assertLessThan(0, 100),
    %% Queue sizes, error counts
    QueueSize = 50,
    MaxQueue = 100,
    ok = ?assertLessThan(QueueSize, MaxQueue),
    %% Negative numbers
    ok = ?assertLessThan(-10, -5),
    %% Floats
    ok = ?assertLessThan(1.0, 1.5),
    ok = ?assertLessThan(0.0, 0.001),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertLessThan(10, 5)),
    ?assertException(error, {assert, _}, ?assertLessThan(10, 10)),
    %% With comment
    ok = ?assertLessThan(5, 10, "should be less").

assert_greater_than_or_equal(_Config) ->
    %% Basic integer comparisons
    ok = ?assertGreaterThanOrEqual(10, 5),
    ok = ?assertGreaterThanOrEqual(10, 10),
    %% Zero checks (balance >= 0)
    Balance = 100,
    ok = ?assertGreaterThanOrEqual(Balance, 0),
    ok = ?assertGreaterThanOrEqual(0, 0),
    %% Negative numbers
    ok = ?assertGreaterThanOrEqual(-5, -10),
    ok = ?assertGreaterThanOrEqual(-5, -5),
    %% Floats
    ok = ?assertGreaterThanOrEqual(1.5, 1.5),
    ok = ?assertGreaterThanOrEqual(1.6, 1.5),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertGreaterThanOrEqual(5, 10)),
    ?assertException(error, {assert, _}, ?assertGreaterThanOrEqual(-1, 0)),
    %% With comment
    ok = ?assertGreaterThanOrEqual(10, 10, "should be at least threshold").

assert_less_than_or_equal(_Config) ->
    %% Basic integer comparisons
    ok = ?assertLessThanOrEqual(5, 10),
    ok = ?assertLessThanOrEqual(10, 10),
    %% Capacity checks (used =< max)
    Used = 80,
    Max = 100,
    ok = ?assertLessThanOrEqual(Used, Max),
    %% Negative numbers
    ok = ?assertLessThanOrEqual(-10, -5),
    ok = ?assertLessThanOrEqual(-5, -5),
    %% Floats
    ok = ?assertLessThanOrEqual(1.5, 1.5),
    ok = ?assertLessThanOrEqual(1.4, 1.5),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertLessThanOrEqual(10, 5)),
    ?assertException(error, {assert, _}, ?assertLessThanOrEqual(101, 100)),
    %% With comment
    ok = ?assertLessThanOrEqual(10, 10, "should not exceed threshold").

assert_in_range_inclusive(_Config) ->
    %% Percentage range (0-100)
    Percentage = 75,
    ok = ?assertInRangeInclusive(Percentage, 0, 100),
    ok = ?assertInRangeInclusive(0, 0, 100),
    ok = ?assertInRangeInclusive(100, 0, 100),
    %% HTTP status codes
    ok = ?assertInRangeInclusive(200, 200, 299),
    ok = ?assertInRangeInclusive(404, 400, 499),
    %% Negative ranges
    ok = ?assertInRangeInclusive(-5, -10, 0),
    ok = ?assertInRangeInclusive(-10, -10, -5),
    %% Floats
    ok = ?assertInRangeInclusive(1.5, 1.0, 2.0),
    ok = ?assertInRangeInclusive(0.5, 0.0, 1.0),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertInRangeInclusive(-1, 0, 100)),
    ?assertException(error, {assert, _}, ?assertInRangeInclusive(101, 0, 100)),
    ?assertException(error, {assert, _}, ?assertInRangeInclusive(199, 200, 299)),
    %% With comment
    ok = ?assertInRangeInclusive(50, 0, 100, "percentage must be valid").

assert_in_range_exclusive(_Config) ->
    ok = ?assertInRangeExclusive(2, 1, 3),
    ok = ?assertInRangeExclusive(50, 0, 100),
    %% Floats
    ok = ?assertInRangeExclusive(1.5, 1.0, 2.0),
    ok = ?assertInRangeExclusive(0.5, 0.0, 1.0),
    %% Computed bounds
    Size = 1000,
    Value = 250,
    ok = ?assertInRangeExclusive(Value, floor(Size * 0.15), ceil(Size * 0.35)),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertInRangeExclusive(1, 1, 3)),
    ?assertException(error, {assert, _}, ?assertInRangeExclusive(3, 1, 3)),
    ?assertException(error, {assert, _}, ?assertInRangeExclusive(0, 0, 100)),
    ?assertException(error, {assert, _}, ?assertInRangeExclusive(100, 0, 100)),
    %% With comment
    ok = ?assertInRangeExclusive(50, 0, 100, "value must be strictly between bounds").

assert_equal_with_delta(_Config) ->
    %% Float comparisons with tolerance
    ok = ?assertEqualWithDelta(3.14159, 3.14, 0.01),
    ok = ?assertEqualWithDelta(3.14159, 3.141, 0.001),
    %% Integer approximations
    ok = ?assertEqualWithDelta(100, 100.5, 1),
    ok = ?assertEqualWithDelta(100, 99, 1),
    %% Zero handling
    ok = ?assertEqualWithDelta(0.0, 0.0, 0.001),
    ok = ?assertEqualWithDelta(0.0, 0.0001, 0.001),
    %% Negative values
    ok = ?assertEqualWithDelta(-5.0, -5.1, 0.2),
    ok = ?assertEqualWithDelta(-100, -99, 2),
    %% Large values with delta
    ok = ?assertEqualWithDelta(1000000, 1000001, 10),
    %% Arithmetic expressions as arguments - parentheses in macro ensure
    %% correct evaluation, e.g. abs((10) - (15 - 3)) = abs(10 - 12) = 2, not abs(10 - 15 - 3) = 8
    ok = ?assertEqualWithDelta(10, 15 - 3, 3),
    ok = ?assertEqualWithDelta(20 + 5, 30 - 3, 5),
    %% Failure cases
    ?assertException(error, {assert, _}, ?assertEqualWithDelta(3.14159, 3.0, 0.01)),
    ?assertException(error, {assert, _}, ?assertEqualWithDelta(100, 102, 1)),
    ?assertException(error, {assert, _}, ?assertEqualWithDelta(0.0, 0.1, 0.01)),
    %% With comment
    ok = ?assertEqualWithDelta(3.14, 3.14159, 0.01, "pi approximation").

assert_comparison_macros_preserve_error_info(_Config) ->
    %% Verify that comparison macros preserve the parse transform's
    %% enhanced error output (the "Because:" clause).
    %% The error_info must use format_comparison_error which generates
    %% the "Because: X > Y" output showing evaluated values.
    X = 5,
    Y = 10,
    try
        ?assertGreaterThan(X, Y)
    catch
        error:{assert, _}:Stacktrace ->
            [{_M, _F, _Args, Info} | _] = Stacktrace,
            ErrorInfo = proplists:get_value(error_info, Info),
            %% Verify error_info exists and uses comparison format
            #{cause := Cause, module := wa_assert, function := format_comparison_error} = ErrorInfo,
            %% Verify comparison metadata is present (left, right, operator)
            #{left := 5, right := 10, operator := '>'} = Cause,
            ok
    end.

%%--------------------------------------------------------------------
%% Format Comment Tests
%%--------------------------------------------------------------------

maybe_format_comment(_Config) ->
    F = fun wa_assert:maybe_format_comment/1,
    %% Iolists (from io_lib:format) get flattened to strings
    ?assertEqual("value is 42", F(io_lib:format("value is ~p", [42]))),
    ?assertEqual("a and b", F(io_lib:format("~s and ~s", ["a", "b"]))),
    ?assertEqual("no args", F(io_lib:format("no args", []))),
    ?assertEqual("café", F(io_lib:format("caf~ts", ["é"]))),
    ?assertEqual("3.14", F(io_lib:format("~.2f", [3.14159]))),
    %% Plain strings pass through (already flat)
    ?assertEqual("plain string", F("plain string")),
    ?assertEqual("", F("")),
    %% Pass-through cases (non-stringy inputs)
    PassThrough = [
        {"label", some_atom},
        {"~p", [42]},
        an_atom,
        42,
        #{reason => test_failed},
        [atom, 123, "mixed"],
        self(),
        {"a", "b", "c"}
    ],
    lists:foreach(fun(Input) -> ?assertEqual(Input, F(Input)) end, PassThrough),
    Ref = make_ref(),
    ?assertEqual(Ref, F(Ref)).

format_comment_integration(_Config) ->
    %% assertEqual with io_lib:format
    try
        ?assertEqual(expected(), actual(), io_lib:format("got ~p", [actual()]))
    catch
        error:{assertEqual, P1} -> ?assertEqual("got 42", proplists:get_value(comment, P1))
    end,
    %% assertMatch with io_lib:format
    try
        ?assertMatch(#{key := _}, actual(#{}), io_lib:format("missing ~p", [key]))
    catch
        error:{assertMatch, P2} -> ?assertEqual("missing key", proplists:get_value(comment, P2))
    end,
    %% assert with io_lib:format
    try
        ?assert(actual(false), io_lib:format("failed ~p", [check]))
    catch
        error:{assert, P3} -> ?assertEqual("failed check", proplists:get_value(comment, P3))
    end.

%%--------------------------------------------------------------------
%% Formatting Output Tests
%%--------------------------------------------------------------------

format_where(_Config) ->
    ?assertEqual("", wa_assert:format_where(#{}, [])),
    PinsResult = wa_assert:format_where(#{'X' => 5, 'Y' => 10}, []),
    ?assert(is_list(string:find(PinsResult, "Where:"))),
    ?assert(is_list(string:find(PinsResult, "'X': 5"))),
    ?assert(is_list(string:find(PinsResult, "'Y': 10"))),
    IntResult = wa_assert:format_where(#{}, [{"length(X)", 3}, {"length(X) * 2", 6}]),
    ?assert(is_list(string:find(IntResult, "Where:"))),
    %% Intermediates should appear in the order they were passed
    Pos1 = string:str(IntResult, "'length(X)': 3"),
    Pos2 = string:str(IntResult, "'length(X) * 2': 6"),
    ?assert(Pos1 > 0),
    ?assert(Pos2 > Pos1),
    BothResult = wa_assert:format_where(#{'X' => [1, 2, 3]}, [{"length(X)", 3}]),
    ?assert(is_list(string:find(BothResult, "Where:"))),
    %% Pins come before intermediates
    PinPos = string:str(BothResult, "'X': [1,2,3]"),
    IntPos = string:str(BothResult, "'length(X)': 3"),
    ?assert(PinPos > 0),
    ?assert(IntPos > PinPos).

%%--------------------------------------------------------------------
%% Internal Helpers
%%--------------------------------------------------------------------
actual() -> 42.
expected() -> 24.

actual(X) -> X.

actual_list() -> [1, 2, 3, 4, 5].
expected_list() -> [1, 5, 3, 2, 4].
