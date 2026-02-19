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
    assert_intermediates_capture/1,
    assert_simple_comparison_no_intermediates/1,
    assert_excludes_local_bindings/1,
    assert_edge_cases_no_crash/1,
    assert_nested_calls_evaluated_once/1,
    assert_multiple_identical_calls/1,
    assert_counter_chaining_across_comparison/1,
    assert_deeply_nested_calls/1,
    assert_mixed_arithmetic_and_calls/1,
    assert_pure_function_repeated/1,
    format_where/1,
    format_error_output/1,
    format_repeated_calls_output/1,
    variable_collision_regression/1,
    format_intermediate_float_short_form/1
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
        assert_intermediates_capture,
        assert_simple_comparison_no_intermediates,
        assert_excludes_local_bindings,
        assert_edge_cases_no_crash,
        assert_nested_calls_evaluated_once,
        assert_multiple_identical_calls,
        assert_counter_chaining_across_comparison,
        assert_deeply_nested_calls,
        assert_mixed_arithmetic_and_calls,
        assert_pure_function_repeated,
        format_where,
        format_error_output,
        format_repeated_calls_output,
        variable_collision_regression,
        format_intermediate_float_short_form
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
%% Intermediate Values Tests
%%--------------------------------------------------------------------

%% Verifies intermediate values are captured for arithmetic, function calls,
%% and nested expressions like ceil(Size * 35 / 100).
assert_intermediates_capture(_Config) ->
    %% Arithmetic expressions
    Size1 = 1000,
    Value1 = 500,
    try
        ?assert(Value1 > Size1 * 0.6)
    catch
        error:{assert, _}:Stacktrace1 ->
            Intermediates1 = get_intermediates(Stacktrace1),
            ?assertEqual(1, length(Intermediates1)),
            ?assert(lists:member(600.0, intermediate_values(Intermediates1))),
            [{Key1, _}] = Intermediates1,
            ?assertEqual("Size1 * 0.6", Key1),
            %% Verify no scientific notation in expression key
            ?assertEqual(nomatch, string:find(Key1, "e-")),
            ?assertEqual(nomatch, string:find(Key1, "e+"))
    end,
    %% Function calls
    Items = [],
    try
        ?assert(length(Items) > 0)
    catch
        error:{assert, _}:Stacktrace2 ->
            Intermediates2 = get_intermediates(Stacktrace2),
            ?assert(has_intermediate("length(Items)", Intermediates2)),
            ?assertEqual(0, get_intermediate("length(Items)", Intermediates2))
    end,
    try
        ?assert(lists:nth(2, [a, b, c]) =:= x)
    catch
        error:{assert, _}:Stacktrace3 ->
            Intermediates3 = get_intermediates(Stacktrace3),
            ?assert(has_intermediate("lists:nth(2, [a, b, c])", Intermediates3)),
            ?assertEqual(b, get_intermediate("lists:nth(2, [a, b, c])", Intermediates3))
    end,
    %% Combined arithmetic and function calls
    Size2 = 1000,
    Items2 = [a, b, c],
    try
        ?assert(length(Items2) * 100 > Size2 div 2)
    catch
        error:{assert, _}:Stacktrace4 ->
            Intermediates4 = get_intermediates(Stacktrace4),
            ?assert(has_intermediate("length(Items2)", Intermediates4)),
            ?assertEqual(3, get_intermediate("length(Items2)", Intermediates4)),
            Values4 = intermediate_values(Intermediates4),
            ?assert(lists:member(3, Values4)),
            ?assert(lists:member(300, Values4)),
            ?assert(lists:member(500, Values4))
    end,
    %% Nested function calls with arithmetic: ceil(Size * 0.35)
    %% Also verifies float formatting in expression keys (no scientific notation)
    Size3 = 1000,
    Value3 = 200,
    try
        ?assert(Value3 > ceil(Size3 * 0.35))
    catch
        error:{assert, _}:Stacktrace5 ->
            Intermediates5 = get_intermediates(Stacktrace5),
            ?assert(has_intermediate("Size3 * 0.35", Intermediates5)),
            ?assertNot(has_intermediate("Size3 * 3.49999999999999977796e-1", Intermediates5)),
            Values5 = intermediate_values(Intermediates5),
            ?assert(lists:member(350.0, Values5)),
            ?assert(lists:member(350, Values5))
    end.

assert_simple_comparison_no_intermediates(_Config) ->
    X = 5,
    Y = 10,
    try
        ?assert(X > Y)
    catch
        error:{assert, _}:Stacktrace ->
            ?assertEqual([], get_intermediates(Stacktrace))
    end.

%% Expressions with local bindings (lambdas, list comprehensions) are excluded
%% from intermediate extraction, but enclosing function calls are still captured.
assert_excludes_local_bindings(_Config) ->
    %% Lambda bodies excluded
    Items1 = [1, 2, 3],
    try
        ?assert(lists:any(fun(X) -> X * 2 > 10 end, Items1))
    catch
        error:{assert, _}:Stacktrace1 ->
            Intermediates1 = get_intermediates(Stacktrace1),
            ?assertEqual(1, length(Intermediates1)),
            ?assert(has_intermediate("lists:any(fun (X) -> X * 2 > 10 end, Items1)", Intermediates1)),
            ?assertEqual(false, get_intermediate("lists:any(fun (X) -> X * 2 > 10 end, Items1)", Intermediates1))
    end,
    %% List comprehension bodies excluded
    Items2 = [1, 2, 3],
    try
        ?assert(lists:sum([X * 2 || X <- Items2]) > 100)
    catch
        error:{assert, _}:Stacktrace2 ->
            Intermediates2 = get_intermediates(Stacktrace2),
            ?assertEqual(1, length(Intermediates2)),
            ?assertEqual(12, get_intermediate("lists:sum([X * 2 || X <- Items2])", Intermediates2))
    end.

%% Edge cases that must not crash the parse transform.
assert_edge_cases_no_crash(_Config) ->
    ?assert(lists:member(a, [_X = a, b, c])),
    ?assert(true),
    ?assert(42 =:= 42),
    ?assert(foo =:= foo),
    X = 5,
    Y = 5,
    ?assert(X =:= Y).

%% Nested calls must be evaluated exactly once (children-first extraction order).
assert_nested_calls_evaluated_once(_Config) ->
    put(call_count, 0),
    ?assert(length(counted_val([1, 2, 3])) =:= 3),
    ?assertEqual(1, get(call_count)).

%% Multiple identical calls each get their own variable.
assert_multiple_identical_calls(_Config) ->
    put(call_count, 0),
    try
        ?assert({counted_val(1), counted_val(1)} =:= {1, 2})
    catch
        error:{assert, _}:Stacktrace ->
            ?assertEqual(2, get(call_count)),
            ?assertEqual(2, length(get_intermediates(Stacktrace)))
    end.

%% Intermediates on both sides of comparison get unique variables.
assert_counter_chaining_across_comparison(_Config) ->
    put(call_count, 0),
    try
        ?assert(counted_val(5) > counted_val(10))
    catch
        error:{assert, _}:Stacktrace ->
            Intermediates = get_intermediates(Stacktrace),
            ?assertEqual(2, get(call_count)),
            ?assertEqual(2, length(Intermediates)),
            Values = intermediate_values(Intermediates),
            ?assert(lists:member(5, Values)),
            ?assert(lists:member(10, Values))
    end.

%% All nesting levels extracted, inner first.
assert_deeply_nested_calls(_Config) ->
    try
        ?assert(outer(middle(inner(42))) =:= wrong)
    catch
        error:{assert, _}:Stacktrace ->
            Intermediates = get_intermediates(Stacktrace),
            ?assertEqual(3, length(Intermediates)),
            ?assert(has_intermediate("inner(42)", Intermediates)),
            ?assert(has_intermediate("middle(inner(42))", Intermediates)),
            ?assert(has_intermediate("outer(middle(inner(42)))", Intermediates))
    end.

%% Mixed arithmetic and function calls all extracted.
assert_mixed_arithmetic_and_calls(_Config) ->
    try
        ?assert(foo(1) + bar(2) * baz(3) > 1000)
    catch
        error:{assert, _}:Stacktrace ->
            Values = intermediate_values(get_intermediates(Stacktrace)),
            ?assert(lists:member(1, Values)),
            ?assert(lists:member(2, Values)),
            ?assert(lists:member(3, Values)),
            ?assert(lists:member(6, Values)),
            ?assert(lists:member(7, Values))
    end.

%% Pure function repeated: both extracted, display may dedupe same values.
assert_pure_function_repeated(_Config) ->
    X = [a, b, c],
    try
        ?assert(length(X) + length(X) > 100)
    catch
        error:{assert, _}:Stacktrace ->
            Intermediates = get_intermediates(Stacktrace),
            ?assertEqual(3, length(Intermediates)),
            ?assertEqual([3, 3, 6], intermediate_values(Intermediates))
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

%% Verifies error formatting for comparison errors (with pins and intermediates)
%% and generic errors.
format_error_output(_Config) ->
    %% Comparison error with pins
    X = 5,
    Y = 10,
    try
        ?assert(X > Y)
    catch
        error:{assert, _} = E1:Stacktrace1 ->
            #{reason := Reason1} = wa_assert:format_comparison_error(E1, Stacktrace1),
            ReasonStr1 = chardata_to_list(Reason1),
            ?assert(is_list(string:find(ReasonStr1, "X > Y"))),
            ?assert(is_list(string:find(ReasonStr1, "Where:"))),
            ?assert(is_list(string:find(ReasonStr1, "'X': 5"))),
            ?assert(is_list(string:find(ReasonStr1, "'Y': 10"))),
            ?assert(is_list(string:find(ReasonStr1, "Because:")))
    end,
    %% Comparison error with intermediates
    Size = 1000,
    Value = 500,
    try
        ?assert(Value > Size div 2)
    catch
        error:{assert, _} = E2:Stacktrace2 ->
            #{reason := Reason2} = wa_assert:format_comparison_error(E2, Stacktrace2),
            ReasonStr2 = chardata_to_list(Reason2),
            ?assert(is_list(string:find(ReasonStr2, "Where:"))),
            ?assert(is_list(string:find(ReasonStr2, "'Size': 1000"))),
            ?assert(is_list(string:find(ReasonStr2, "'Value': 500"))),
            ?assert(is_list(string:find(ReasonStr2, "500")))
    end,
    %% Generic error with intermediates
    Items = [a, b],
    try
        ?assert(length(Items) > 5)
    catch
        error:{assert, _} = E3:Stacktrace3 ->
            #{reason := Reason3} = wa_assert:format_generic_error(E3, Stacktrace3),
            ReasonStr3 = chardata_to_list(Reason3),
            ?assert(is_list(string:find(ReasonStr3, "Where:"))),
            ?assert(is_list(string:find(ReasonStr3, "'length(Items)': 2")))
    end.

format_repeated_calls_output(_Config) ->
    %% Impure function - each call produces different value, both appear
    put(call_count, 0),
    try
        ?assert({impure_counter(), impure_counter()} =:= {0, 0})
    catch
        error:{assert, _} = E1:Stacktrace1 ->
            ?assertEqual(2, get(call_count)),
            #{reason := Reason1} = wa_assert:format_comparison_error(E1, Stacktrace1),
            ReasonStr1 = chardata_to_list(Reason1),
            ?assert(is_list(string:find(ReasonStr1, "'impure_counter()': 1"))),
            ?assert(is_list(string:find(ReasonStr1, "'impure_counter()': 2")))
    end,
    %% Pure function - duplicate mappings (same key+value) deduplicated
    X = [a, b, c],
    try
        ?assert(length(X) + length(X) > 100)
    catch
        error:{assert, _} = E2:Stacktrace2 ->
            #{reason := Reason2} = wa_assert:format_generic_error(E2, Stacktrace2),
            ReasonStr2 = chardata_to_list(Reason2),
            Pattern = "'length(X)': 3",
            First = string:find(ReasonStr2, Pattern),
            ?assert(is_list(First)),
            %% No second occurrence
            Rest = string:slice(First, string:length(Pattern)),
            ?assertEqual(nomatch, string:find(Rest, Pattern))
    end.

%%--------------------------------------------------------------------
%% Variable Name Collision Regression Test
%%
%% The parse transform generates temporary variables (Intermediate__<line>_<N>)
%% for each assertion. If names collide, the Erlang compiler rejects the module,
%% so successful compilation of this test is itself the primary assertion.
%%--------------------------------------------------------------------

variable_collision_regression(_Config) ->
    %% Multiple comparison and generic assertions in same scope
    X = 5,
    Y = 10,
    Z = 15,
    List = [1, 2, 3],
    ?assert(X < Y),
    ?assert(Y < Z),
    ?assert(X < Z),
    ?assert(length(List) =:= 3),
    ?assert(X + Y =:= 15),
    ?assert(lists:sum(List) =:= 6),
    %% Assertions alongside list comprehensions
    Doubled = [V * 2 || V <- List],
    ?assert(length(Doubled) =:= 3),
    ?assert(lists:sum(Doubled) =:= 12),
    %% Assertions inside fun bodies and nested funs
    Outer = fun(A) ->
        ?assert(A > 0),
        Inner = fun(B) ->
            ?assert(B > A),
            ?assert(B < 100),
            ok
        end,
        ?assert(A < 50),
        Inner(A + 10)
    end,
    ok = Outer(5),
    ok = Outer(20).

format_intermediate_float_short_form(_Config) ->
    Intermediates = [{"ByteSize * 0.15", 1904 * 0.15}],
    Result = wa_assert:format_where(#{}, Intermediates),
    ?assert(is_list(string:find(Result, "285.6"))),
    ?assertEqual(nomatch, string:find(Result, "285.5999999")).

%%--------------------------------------------------------------------
%% Internal Helpers
%%--------------------------------------------------------------------
actual() -> 42.
expected() -> 24.

actual(X) -> X.

actual_list() -> [1, 2, 3, 4, 5].
expected_list() -> [1, 5, 3, 2, 4].

counted_val(X) ->
    put(call_count, get(call_count) + 1),
    X.

impure_counter() ->
    Count = get(call_count) + 1,
    put(call_count, Count),
    Count.

inner(X) -> X.
middle(X) -> X.
outer(X) -> X.

foo(X) -> X.
bar(X) -> X.
baz(X) -> X.

get_intermediates(Stacktrace) ->
    [{_M, _F, _Args, Info} | _] = Stacktrace,
    ErrorInfo = proplists:get_value(error_info, Info),
    #{cause := Cause} = ErrorInfo,
    maps:get(intermediates, Cause, []).

get_intermediate(Key, Intermediates) ->
    proplists:get_value(Key, Intermediates).

has_intermediate(Key, Intermediates) ->
    proplists:is_defined(Key, Intermediates).

intermediate_values(Intermediates) ->
    [V || {_, V} <- Intermediates].

chardata_to_list(CharData) ->
    case unicode:characters_to_list(CharData) of
        List when is_list(List) -> List;
        {error, _, _} -> [];
        {incomplete, _, _} -> []
    end.
