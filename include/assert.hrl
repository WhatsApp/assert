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

%%% -------------------------------------------------------------------
%%% Comparison Assertions
%%%
%%% These macros provide meaningful assertion failures for numeric comparisons.
%%% Instead of "expected true, got false", failures show the actual values
%%% and the comparison that failed.
%%% -------------------------------------------------------------------

%% @doc Assert that Value is less than Threshold.
%% Example: ?assertLessThan(ErrorCount, FailoverLimit)
-define(assertLessThan(Value, Threshold),
    ?assert(
        (Value) < (Threshold),
        lists:flatten(
            io_lib:format(
                "expected value < threshold, got value ~p, threshold ~p",
                [(Value), (Threshold)]
            )
        )
    )
).

-define(assertLessThan(Value, Threshold, Comment),
    ?assert(
        (Value) < (Threshold),
        lists:flatten(
            io_lib:format(
                "~s: expected value < threshold, got value ~p, threshold ~p",
                [(Comment), (Value), (Threshold)]
            )
        )
    )
).

%% @doc Assert that Value is less than or equal to Threshold.
%% Example: ?assertLessThanOrEqual(Size, MaxSize)
-define(assertLessThanOrEqual(Value, Threshold),
    ?assert(
        (Value) =< (Threshold),
        lists:flatten(
            io_lib:format(
                "expected value =< threshold, got value ~p, threshold ~p",
                [(Value), (Threshold)]
            )
        )
    )
).

-define(assertLessThanOrEqual(Value, Threshold, Comment),
    ?assert(
        (Value) =< (Threshold),
        lists:flatten(
            io_lib:format(
                "~s: expected value =< threshold, got value ~p, threshold ~p",
                [(Comment), (Value), (Threshold)]
            )
        )
    )
).

%% @doc Assert that Value is greater than Threshold.
%% Example: ?assertGreaterThan(length(List), 0)
-define(assertGreaterThan(Value, Threshold),
    ?assert(
        (Value) > (Threshold),
        lists:flatten(
            io_lib:format(
                "expected value > threshold, got value ~p, threshold ~p",
                [(Value), (Threshold)]
            )
        )
    )
).

-define(assertGreaterThan(Value, Threshold, Comment),
    ?assert(
        (Value) > (Threshold),
        lists:flatten(
            io_lib:format(
                "~s: expected value > threshold, got value ~p, threshold ~p",
                [(Comment), (Value), (Threshold)]
            )
        )
    )
).

%% @doc Assert that Value is greater than or equal to Threshold.
%% Example: ?assertGreaterThanOrEqual(Balance, 0)
-define(assertGreaterThanOrEqual(Value, Threshold),
    ?assert(
        (Value) >= (Threshold),
        lists:flatten(
            io_lib:format(
                "expected value >= threshold, got value ~p, threshold ~p",
                [(Value), (Threshold)]
            )
        )
    )
).

-define(assertGreaterThanOrEqual(Value, Threshold, Comment),
    ?assert(
        (Value) >= (Threshold),
        lists:flatten(
            io_lib:format(
                "~s: expected value >= threshold, got value ~p, threshold ~p",
                [(Comment), (Value), (Threshold)]
            )
        )
    )
).

%% @doc Assert that Value is within the range [Min, Max] (inclusive).
%% Example: ?assertInRange(Percentage, 0, 100)
-define(assertInRange(Value, Min, Max),
    ?assert(
        (Value) >= (Min) andalso (Value) =< (Max),
        lists:flatten(
            io_lib:format(
                "expected ~p =< value =< ~p, got value ~p",
                [(Min), (Max), (Value)]
            )
        )
    )
).

-define(assertInRange(Value, Min, Max, Comment),
    ?assert(
        (Value) >= (Min) andalso (Value) =< (Max),
        lists:flatten(
            io_lib:format(
                "~s: expected ~p =< value =< ~p, got value ~p",
                [(Comment), (Min), (Max), (Value)]
            )
        )
    )
).

%% @doc Assert that Actual is within Delta of Expected (for floating point comparisons).
%% Example: ?assertEqualWithDelta(3.14159, calculate_pi(), 0.001)
-define(assertEqualWithDelta(Expected, Actual, Delta),
    ?assert(
        abs((Actual) - (Expected)) =< (Delta),
        lists:flatten(
            io_lib:format(
                "expected |actual - expected| =< delta, got expected ~p, actual ~p, delta ~p",
                [(Expected), (Actual), (Delta)]
            )
        )
    )
).

-define(assertEqualWithDelta(Expected, Actual, Delta, Comment),
    ?assert(
        abs((Actual) - (Expected)) =< (Delta),
        lists:flatten(
            io_lib:format(
                "~s: expected |actual - expected| =< delta, got expected ~p, actual ~p, delta ~p",
                [(Comment), (Expected), (Actual), (Delta)]
            )
        )
    )
).

-endif.
