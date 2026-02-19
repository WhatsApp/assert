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
-module(wa_assert).
%% erlfmt:ignore
% @fb-only: -oncall("whatsapp_server_devx").
-compile(warn_missing_spec_all).

-export([
    error_info/2, assert_error_info/2, format_error/2, format_comparison_error/2, format_generic_error/2, format_where/2
]).
-export([maybe_format_comment/1]).

-export(['$assert_match_error_info$'/1, '$expand_assert$'/1]).

-type match_cause() :: #{pins := pins(), pattern := term()}.
-type assert_cause() :: comparison_cause() | generic_cause().
-type comparison_cause() :: #{
    type := comparison,
    pins := pins(),
    left := term(),
    right := term(),
    operator := atom(),
    expression := string(),
    intermediates => intermediates()
}.
-type generic_cause() :: #{
    type := generic, pins := pins(), expression := string(), intermediates => intermediates()
}.
-type error_info(Cause) :: #{cause => Cause, module => module(), function => atom()}.
-type error_description() :: #{general => unicode:chardata(), reason => unicode:chardata()}.
-type pins() :: #{atom() => term()}.
-type intermediates() :: [{string(), term()}].
-type comparison_meta() :: #{
    type := comparison,
    pins := pins(),
    left := term(),
    right := term(),
    operator := atom(),
    intermediates => intermediates()
}.
-type generic_meta() :: #{type := generic, pins := pins(), intermediates => intermediates()}.
-type meta() :: comparison_meta() | generic_meta().

-spec assert_error_info(string(), meta()) -> {ok, error_info(assert_cause())} | {error, no_error_info}.
assert_error_info(Expression, #{type := comparison} = Meta) ->
    {ok, #{module => ?MODULE, function => format_comparison_error, cause => Meta#{expression => Expression}}};
assert_error_info(Expression, #{type := generic} = Meta) ->
    {ok, #{module => ?MODULE, function => format_generic_error, cause => Meta#{expression => Expression}}};
assert_error_info(_Expression, _Meta) ->
    {error, no_error_info}.

-spec error_info(string(), pins()) -> {ok, error_info(match_cause())}.
error_info(Pattern, Pins) ->
    Cause = #{pins => Pins, pattern => Pattern},
    {ok, #{module => ?MODULE, function => format_error, cause => Cause}}.

-spec format_error(term(), erlang:stacktrace()) -> error_description().
format_error(Reason, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfo),
    Pins = maps:get(pins, Cause),
    Where = format_where(Pins, []),
    #{general => "Assert", reason => io_lib:format("~n~n~ts ~p", [Where, Reason])}.

-spec format_comparison_error(term(), erlang:stacktrace()) -> error_description().
format_comparison_error(Reason0, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info),
    #{cause := Cause} = ErrorInfo,
    #{left := Left, right := Right, expression := Expression, operator := Operator, pins := Pins} = Cause,
    Reason = io_lib:format(
        "~n~nThe following expression failed:~n~n~s~n~nBecause:~n~n~p ~s ~p~ts~n ~p", [
            Expression, Left, Operator, Right, format_where(Pins, []), Reason0
        ]
    ),
    #{general => "Assert", reason => Reason}.

-spec format_generic_error(term(), erlang:stacktrace()) -> error_description().
format_generic_error(Reason0, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info),
    #{cause := Cause} = ErrorInfo,
    #{expression := Expression, pins := Pins} = Cause,
    Reason = io_lib:format(
        "~n~nThe following expression failed:~n~n~s~ts~n ~p", [
            Expression, format_where(Pins, []), Reason0
        ]
    ),
    #{general => "Assert", reason => Reason}.

%% Format the "Where:" clause with both variable bindings and intermediate values.
%% Intermediates are kept as an ordered list to preserve expression order.
%% Duplicate mappings (same key and value) are deduplicated.
-spec format_where(pins(), intermediates()) -> string().
format_where(Pins, []) when map_size(Pins) =:= 0 ->
    "";
format_where(Pins, Intermediates) ->
    PinLines = [format_pin(Key, Value) || Key := Value <- Pins],
    DedupedIntermediates = dedup_intermediates(Intermediates),
    IntermediateLines = [format_intermediate(Key, Value) || {Key, Value} <- DedupedIntermediates],
    AllLines = PinLines ++ IntermediateLines,
    lists:flatten(
        io_lib:format("~n~nWhere:~n~n~ts~n", [string:join(AllLines, "\n")])
    ).

%% Remove duplicate {Key, Value} pairs while preserving order.
-spec dedup_intermediates(intermediates()) -> intermediates().
dedup_intermediates(Intermediates) ->
    {Deduped, _Seen} = lists:foldl(
        fun(Entry, {Acc, Seen}) ->
            case sets:is_element(Entry, Seen) of
                true -> {Acc, Seen};
                false -> {[Entry | Acc], sets:add_element(Entry, Seen)}
            end
        end,
        {[], sets:new()},
        Intermediates
    ),
    lists:reverse(Deduped).

-spec format_pin(term(), term()) -> string().
format_pin(Key, Value) ->
    lists:flatten(io_lib:format("  ~p: ~p", [Key, Value])).

-spec format_intermediate(string(), term()) -> string().
format_intermediate(ExprStr, Value) ->
    lists:flatten(io_lib:format("  '~s': ~p", [ExprStr, Value])).

-spec maybe_format_comment(Comment) -> Comment when Comment :: term().
maybe_format_comment(Comment) when is_list(Comment) ->
    try
        lists:flatten(io_lib:format("~s", [Comment]))
    catch
        _:_ -> Comment
    end;
maybe_format_comment(Comment) ->
    Comment.

%% These are only used as markers for the parse transform, but they are defined nonetheless to avoid
%% warnings about unused functions.
-spec '$assert_match_error_info$'(term()) -> {error, no_error_info}.
'$assert_match_error_info$'(_Expression) ->
    {error, no_error_info}.

-spec '$expand_assert$'(term()) -> #{bool_expr := term(), meta := meta()}.
'$expand_assert$'(Expression) ->
    #{bool_expr => Expression, meta => #{type => 'generic', pins => #{}}}.
