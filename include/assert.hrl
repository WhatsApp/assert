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
%% @oncall whatsapp_developer_tools
-ifndef(WADIFF_ASSERT_HRL).
-define(WADIFF_ASSERT_HRL, true).

-compile({parse_transform, wa_assert_parse_trans}).

-include_lib("stdlib/include/assert.hrl").

-undef(assertEqual).
-define(assertEqual(Expect, Expr), begin
    ((fun() ->
        X__X = (Expect),
        case (Expr) of
            X__X ->
                ok;
            X__V ->
                R__R =
                    {assertEqual, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {expression, (??Expr)},
                        {expected, X__X},
                        {value, X__V}
                    ]},
                case wa_diff:error_info(?MODULE, X__X, X__V) of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).

-define(assertEqual(Expect, Expr, Comment), begin
    ((fun() ->
        X__X = (Expect),
        case (Expr) of
            X__X ->
                ok;
            X__V ->
                R__R =
                    {assertEqual, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {comment, (Comment)},
                        {expression, (??Expr)},
                        {expected, X__X},
                        {value, X__V}
                    ]},
                case wa_diff:error_info(?MODULE, X__X, X__V) of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).

-undef(assertMatch).
-define(assertMatch(Pattern, Expr), begin
    ((fun() ->
        case (Expr) of
            Pattern ->
                ok;
            X__V ->
                R__R =
                    {assertMatch, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {expression, (??Expr)},
                        {value, X__V}
                    ]},
                case
                    wa_assert:'$assert_match_error_info$'(
                        case (Expr) of
                            Pattern -> ok
                        end
                    )
                of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).
-define(assertMatch(Pattern, Expr, Comment), begin
    ((fun() ->
        case (Expr) of
            Pattern ->
                ok;
            X__V ->
                R__R =
                    {assertMatch, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {comment, (Comment)},
                        {expression, (??Expr)},
                        {value, X__V}
                    ]},
                case
                    wa_assert:'$assert_match_error_info$'(
                        case (Expr) of
                            Pattern -> ok
                        end
                    )
                of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).

-undef(assert).
-ifdef(ELP_ERLANG_SERVICE).
-define(assert(BoolExpr), begin
    (fun() ->
        case (BoolExpr) of
            true -> ok;
            _ -> erlang:error(assertion_failed)
        end
    end)()
end).
-else.
-define(assert(BoolExpr), begin
    ((fun() ->
        % cheap source of truth
        X__T = is_process_alive(self()),
        #{bool_expr := BoolExpr__BoolExpr, meta := Meta__Meta} = wa_assert:'$expand_assert$'((BoolExpr)),
        case BoolExpr__BoolExpr of
            X__T ->
                ok;
            X__V ->
                R__R =
                    {assert, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {expression, (??BoolExpr)},
                        {expected, true},
                        case not X__T of
                            X__V -> {value, false};
                            _ -> {not_boolean, X__V}
                        end
                    ]},
                case wa_assert:assert_error_info(??BoolExpr, Meta__Meta) of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).
-endif.
-ifdef(ELP_ERLANG_SERVICE).
-define(assert(BoolExpr, Comment), begin
    (fun() ->
        case (BoolExpr) of
            true -> ok;
            _ -> erlang:error(Comment)
        end
    end)()
end).
-else.
-define(assert(BoolExpr, Comment), begin
    ((fun() ->
        % cheap source of truth
        X__T = is_process_alive(self()),
        #{bool_expr := BoolExpr__BoolExpr, meta := Meta__Meta} = wa_assert:'$expand_assert$'((BoolExpr)),
        case BoolExpr__BoolExpr of
            X__T ->
                ok;
            X__V ->
                R__R =
                    {assert, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {comment, (Comment)},
                        {expression, (??BoolExpr)},
                        {expected, true},
                        case not X__T of
                            X__V -> {value, false};
                            _ -> {not_boolean, X__V}
                        end
                    ]},
                case wa_assert:assert_error_info(??BoolExpr, Meta__Meta) of
                    {ok, EI__EI} ->
                        erlang:error(R__R, none, [{error_info, EI__EI}]);
                    {error, no_error_info} ->
                        erlang:error(R__R)
                end
        end
    end)())
end).
-endif.

-define(assertEqualSorted(ExpectedList, ActualList),
    ?assertEqual(lists:sort(ExpectedList), lists:sort(ActualList))
).

-define(assertEqualSorted(ExpectedList, ActualList, Comment),
    ?assertEqual(lists:sort(ExpectedList), lists:sort(ActualList), Comment)
).

%%--------------------------------------------------------------------
%% Numeric Comparison Assertion Macros
%%
%% These macros wrap common comparison patterns, for discoverability,
%% clear intent, and readability. They delegate to ?assert to preserve
%% its enhanced error messages.
%%--------------------------------------------------------------------

%% Asserts that Value is strictly greater than Threshold.
%% Example: ?assertGreaterThan(Count, 0)
-define(assertGreaterThan(Value, Threshold),
    ?assert(Value > Threshold)
).
-define(assertGreaterThan(Value, Threshold, Comment),
    ?assert(Value > Threshold, Comment)
).

%% Asserts that Value is strictly less than Threshold.
%% Example: ?assertLessThan(Age, 100)
-define(assertLessThan(Value, Threshold),
    ?assert(Value < Threshold)
).
-define(assertLessThan(Value, Threshold, Comment),
    ?assert(Value < Threshold, Comment)
).

%% Asserts that Value is greater than or equal to Threshold.
%% Example: ?assertGreaterThanOrEqual(Balance, 0)
-define(assertGreaterThanOrEqual(Value, Threshold),
    ?assert(Value >= Threshold)
).
-define(assertGreaterThanOrEqual(Value, Threshold, Comment),
    ?assert(Value >= Threshold, Comment)
).

%% Asserts that Value is less than or equal to Threshold.
%% Example: ?assertLessThanOrEqual(ErrorRate, 0.01)
-define(assertLessThanOrEqual(Value, Threshold),
    ?assert(Value =< Threshold)
).
-define(assertLessThanOrEqual(Value, Threshold, Comment),
    ?assert(Value =< Threshold, Comment)
).

%% Asserts that Value is within the inclusive range [Min, Max].
%%
%% Note: Value is evaluated twice due to the andalso expression.
%% For expensive computations or side-effecting expressions, bind to a
%% variable first: V = expensive_call(), ?assertInRangeInclusive(V, 0, 100)
%%
%% Example: ?assertInRangeInclusive(Port, 1, 65535)
-define(assertInRangeInclusive(Value, Min, Max),
    ?assert(Value >= Min andalso Value =< Max)
).
-define(assertInRangeInclusive(Value, Min, Max, Comment),
    ?assert(Value >= Min andalso Value =< Max, Comment)
).

%% Asserts that Value is within the exclusive range (Min, Max).
%%
%% Note: Value is evaluated twice due to the andalso expression.
%% For expensive computations or side-effecting expressions, bind to a
%% variable first: V = expensive_call(), ?assertInRangeExclusive(V, 0, 100)
%%
%% Example: ?assertInRangeExclusive(Value, floor(Size * 0.15), ceil(Size * 0.35))
-define(assertInRangeExclusive(Value, Min, Max),
    ?assert(Value > Min andalso Value < Max)
).
-define(assertInRangeExclusive(Value, Min, Max, Comment),
    ?assert(Value > Min andalso Value < Max, Comment)
).

%% Asserts that Expected and Actual are within Delta of each other.
%% Useful for floating-point comparisons or approximate equality checks.
%% Example: ?assertEqualWithDelta(3.141, calculate_pi(), 0.001)
%%
%% Note: Parentheses around Expected and Actual are required to handle
%% expressions like ?assertEqualWithDelta(A, B - C, D) correctly.
-define(assertEqualWithDelta(Expected, Actual, Delta),
    ?assert(erlang:abs((Expected) - (Actual)) =< Delta)
).
-define(assertEqualWithDelta(Expected, Actual, Delta, Comment),
    ?assert(erlang:abs((Expected) - (Actual)) =< Delta, Comment)
).

-endif.
