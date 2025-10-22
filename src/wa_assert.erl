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
% @fb-only
-compile(warn_missing_spec_all).

-export([error_info/2, assert_error_info/2, format_error/2, format_comparison_error/2, format_generic_error/2]).

-export(['$assert_match_error_info$'/1, '$expand_assert$'/1]).

-type match_cause() :: #{pins := pins(), pattern := term()}.
-type assert_cause() :: comparison_cause() | generic_cause().
-type comparison_cause() :: #{
    type := comparison, pins := pins(), left := term(), right := term(), operator := atom(), expression := string()
}.
-type generic_cause() :: #{
    type := generic, pins := pins(), expression := string()
}.
-type error_info(Cause) :: #{cause => Cause, module => module(), function => atom()}.
-type error_description() :: #{general => unicode:chardata(), reason => unicode:chardata()}.
-type pins() :: #{atom() => term()}.
-type comparison_meta() :: #{type := comparison, pins := pins(), left := term(), right := term(), operator := atom()}.
-type generic_meta() :: #{type := generic, pins := pins()}.
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
    Pins = format_pins(maps:get(pins, Cause)),
    #{general => "Assert", reason => io_lib:format("~n~n~ts ~p", [Pins, Reason])}.

-spec format_comparison_error(term(), erlang:stacktrace()) -> error_description().
format_comparison_error(Reason0, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info),
    #{cause := Cause} = ErrorInfo,
    #{left := Left, right := Right, expression := Expression, operator := Operator, pins := Pins} = Cause,
    Reason = io_lib:format(
        "~n~nThe following expression failed:~n~n~s~n~nBecause:~n~n~p ~s ~p~ts~n ~p", [
            Expression, Left, Operator, Right, format_pins(Pins), Reason0
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
            Expression, format_pins(Pins), Reason0
        ]
    ),
    #{general => "Assert", reason => Reason}.

-spec format_pins(map()) -> string().
format_pins(Pins) when map_size(Pins) =:= 0 ->
    "";
format_pins(Pins) ->
    lists:flatten(
        io_lib:format("~n~nWhere:~n~n~ts~n", [string:join([format_pin(Key, Value) || Key := Value <- Pins], "\n")])
    ).

-spec format_pin(term(), term()) -> string().
format_pin(Key, Value) ->
    lists:flatten(io_lib:format("  ~p: ~p~n", [Key, Value])).

%% These are only used as markers for the parse transform, but they are defined nonetheless to avoid
%% warnings about unused functions.
-spec '$assert_match_error_info$'(term()) -> {error, no_error_info}.
'$assert_match_error_info$'(_Expression) ->
    {error, no_error_info}.

-spec '$expand_assert$'(term()) -> #{bool_expr := term(), meta := meta()}.
'$expand_assert$'(Expression) ->
    #{bool_expr => Expression, meta => #{type => 'generic', pins => #{}}}.
