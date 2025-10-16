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
-module(wa_blame).
%% erlfmt:ignore
% @fb-only
-compile(warn_missing_spec_all).

-include_lib("kernel/include/logger.hrl").

-export([
    blame/2,
    pp_blame/3,
    error_info/2
]).

-export([format_blame/2, format_error/2]).

-type stacktrace() :: [dynamic()].
-type stacktrace_extrainfo() ::
    {error_info, #{module => module(), function => atom(), cause => term()}}
    | {atom(), term()}.
-type debug_info() :: dynamic().
-type tree() :: erl_syntax:syntaxTree().
-type bindings() :: orddict:orddict().
-type blame_info() :: {clauses, [clause_info()]} | {generic, [pattern_info()]}.
-type clause_info() :: {[pattern_info()], [conjunction_info()]}.
-type pattern_info() :: {badkey | boolean() | container_info(), tree()}.
-type container_type() :: map | tuple.
-type container_info() :: {container_type(), [container_field_info()]}.
-type container_field_info() :: {pattern_info(), tree()}.
-type guard_info() :: {boolean(), tree()}.
-type conjunction_info() :: [guard_info()].
-type doc() :: erlfmt_algebra:doc().
-type error_info() :: #{cause => term(), module => module(), function => atom()}.
-type error_description() :: #{
    pos_integer() => unicode:chardata(), general => unicode:chardata(), reason => unicode:chardata()
}.

-spec blame(term(), stacktrace()) -> {ok, blame_info()} | {error, no_blame_info}.
blame(function_clause, [{Module, Function, Args, _ExtraInfo} | _] = _StackTrace) ->
    {ok, blame_mfa(Module, Function, Args)};
blame({assertMatch, Reason}, [{Module, _Function, _Args, ExtraInfo} | _] = _StackTrace) when is_list(Reason) ->
    Value = proplists:get_value(value, Reason),
    case extract_pattern(ExtraInfo) of
        {ok, Pattern0} ->
            Bindings = extract_bindings(ExtraInfo),
            Pattern = expand_records(Module, Pattern0),
            {PatternInfo, _NewBindings} = blame_pattern(Pattern, Value, Bindings),
            {ok, {generic, [PatternInfo]}};
        {error, no_pattern} ->
            {error, no_blame_info}
    end;
blame(_Reason, _StackTrace) ->
    {error, no_blame_info}.

-spec expand_records(atom(), tree()) -> tree().
expand_records(Module, Expr) ->
    CompileOpts = [],
    Records = get_records(Module),
    Forms = Records ++ [wrap_in_function('dummy_function', Expr)],
    Expanded = erl_expand_records:module(Forms, CompileOpts),
    Function = lists:last(Expanded),
    extract_from_function(Function).

-spec wrap_in_function(atom(), tree()) -> erl_parse:abstract_form().
wrap_in_function(Name, Pattern) ->
    Clauses = [erl_syntax:clause([Pattern], none, [erl_syntax:atom('ok')])],
    erl_syntax:revert(erl_syntax:function(erl_syntax:atom(Name), Clauses)).

-spec extract_from_function(tree()) -> tree().
extract_from_function(Function) ->
    Clauses = erl_syntax:function_clauses(Function),
    [Clause] = Clauses,
    [Pattern] = erl_syntax:clause_patterns(Clause),
    Pattern.

-spec get_records(atom()) -> [erl_parse:abstract_form()].
get_records(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            case erl_prim_loader:get_file(File) of
                {ok, Beam, File} ->
                    {ok, {_Mod, [{abstract_code, {_Version, Forms}}]}} = beam_lib:chunks(Beam, [
                        abstract_code
                    ]),
                    [Record || {attribute, _, record, _} = Record <- Forms];
                _ ->
                    []
            end;
        _ ->
            []
    end.

-spec extract_pattern([stacktrace_extrainfo()]) -> {ok, tree()} | {error, no_pattern}.
extract_pattern(ExtraInfo) ->
    try
        ErrorInfo = proplists:get_value(error_info, ExtraInfo),
        #{pattern := PatternStr} = maps:get(cause, ErrorInfo),
        {ok, Tokens, _} = erl_scan:string(PatternStr ++ "."),
        {ok, [Pattern]} = erl_parse:parse_exprs(Tokens),
        {ok, Pattern}
    catch
        _:_:_ ->
            {error, no_pattern}
    end.

-spec extract_bindings([stacktrace_extrainfo()]) -> bindings().
extract_bindings(ExtraInfo) ->
    DefaultBindings = orddict:new(),
    try
        ErrorInfo = proplists:get_value(error_info, ExtraInfo),
        #{pins := Pins} = maps:get(cause, ErrorInfo),
        maps:fold(fun(Key, Value, Acc) -> orddict:store(Key, Value, Acc) end, DefaultBindings, Pins)
    catch
        _:_:_ ->
            DefaultBindings
    end.

-spec blame_mfa(module(), atom(), [term()]) -> blame_info().
blame_mfa(Module, Function, Args) ->
    {ok, DebugInfo} = debug_info(Module),
    Clauses = function_clauses({Module, Function, length(Args)}, DebugInfo),
    {clauses, [blame_clause(Clause, Args) || Clause <- Clauses]}.

-spec blame_clause(tree(), [term()]) -> clause_info().
blame_clause(Clause, Args) ->
    Patterns = erl_syntax:clause_patterns(Clause),
    Guards = erl_syntax:clause_guard(Clause),
    {PatternsInfo, Bindings} = blame_patterns(Patterns, Args),
    GuardsInfo = blame_guards(Guards, Bindings),
    {PatternsInfo, GuardsInfo}.

-spec blame_patterns([tree()], [term()]) -> {[pattern_info()], bindings()}.
blame_patterns(Patterns, Args) ->
    {Info, Binding} = lists:foldl(
        fun({Pattern, Arg}, {Info0, Bindings0}) ->
            {PatternInfo, Bindings} = blame_pattern(Pattern, Arg, Bindings0),
            {[PatternInfo | Info0], Bindings}
        end,
        {[], orddict:new()},
        lists:zip(Patterns, Args)
    ),
    {lists:reverse(Info), Binding}.

-spec blame_pattern(tree(), term(), bindings()) -> {pattern_info(), bindings()}.
blame_pattern(Pattern, Arg, Bindings0) ->
    Bindings1 = orddict:store('$VAR$', Arg, Bindings0),
    {IsMatch, NewBindings} =
        try
            case erl_eval:expr(match_expression(Pattern), Bindings1) of
                {value, _Value, Bindings2} when not is_map(Bindings2) ->
                    {true, Bindings2}
            end
        catch
            error:{badmatch, Map}:_S when is_map(Map) ->
                blame_map(Pattern, Map, Bindings0);
            error:{badmatch, Tuple}:_S when is_tuple(Tuple) ->
                blame_tuple(Pattern, Tuple, Bindings0);
            error:{badmatch, _}:_S ->
                {false, Bindings1}
        end,
    {{IsMatch, Pattern}, NewBindings}.

-spec blame_map(tree(), map(), bindings()) -> {container_info(), bindings()}.
blame_map({map, _Anno, Fields}, Map, Bindings0) ->
    Fun = fun(Field, {Acc, Bs}) ->
        {IsMatch, NewBs} = blame_map_field(Field, Map, Bs),
        {[{IsMatch, Field} | Acc], NewBs}
    end,
    {Info, Bindings} = lists:foldl(Fun, {[], Bindings0}, Fields),
    {{map, lists:reverse(Info)}, Bindings}.

-spec blame_map_field(tree(), map(), bindings()) -> {pattern_info(), bindings()}.
blame_map_field({map_field_exact, _Anno, Key, Value}, Map, Bindings0) ->
    try
        case erl_eval:expr(Key, Bindings0) of
            {value, EvaluatedKey, Bindings1} when not is_map(Bindings1) ->
                MapValue = maps:get(EvaluatedKey, Map),
                blame_pattern(Value, MapValue, Bindings1)
        end
    catch
        error:{badkey, _}:_S ->
            {{badkey, Value}, Bindings0};
        error:_:_S ->
            {{false, Value}, Bindings0}
    end.

-spec blame_tuple(tree(), tuple(), bindings()) -> {container_info() | boolean(), bindings()}.
blame_tuple({tuple, _Anno, Fields}, Tuple, Bindings0) when length(Fields) =:= tuple_size(Tuple) ->
    Fun = fun(Field, {Acc, Index, Bs}) ->
        {IsMatch, NewBs} = blame_pattern(Field, element(Index, Tuple), Bs),
        {[{IsMatch, Field} | Acc], Index + 1, NewBs}
    end,
    {Info, _Index, Bindings} = lists:foldl(Fun, {[], 1, Bindings0}, Fields),
    {{tuple, lists:reverse(Info)}, Bindings};
blame_tuple(_Tree, _Tuple, Bindings) ->
    {false, Bindings}.

-spec blame_guards(none | tree(), bindings()) -> [conjunction_info()].
blame_guards(none, _Bindings) ->
    [];
blame_guards(Guards, Bindings) ->
    case erl_syntax:type(Guards) of
        conjunction ->
            [blame_conjunction(erl_syntax:conjunction_body(Guards), Bindings)];
        disjunction ->
            DisjunctionBody = erl_syntax:disjunction_body(Guards),
            [
                blame_conjunction(erl_syntax:conjunction_body(Conjunction), Bindings)
             || Conjunction <- DisjunctionBody
            ]
    end.

-spec blame_conjunction([tree()], bindings()) -> conjunction_info().
blame_conjunction(Guards, Bindings) ->
    Fun = fun(Guard, {IsMatch0, Acc}) ->
        IsMatch = blame_guard(Guard, Bindings, IsMatch0),
        {IsMatch0 andalso IsMatch, [{IsMatch, Guard} | Acc]}
    end,
    {_Ignore, Acc} = lists:foldl(Fun, {true, []}, Guards),
    lists:reverse(Acc).

-spec blame_guard(tree(), bindings(), boolean()) -> boolean().
blame_guard(Guard, Bindings, true = _IsMatch) ->
    evaluate_guard(Guard, Bindings);
blame_guard(_Guard, _Bindings, false = _IsMatch) ->
    true.

-spec evaluate_guard(dynamic(), bindings()) -> boolean().
evaluate_guard(Guard, Bindings) ->
    try
        case erl_eval:expr(Guard, Bindings) of
            {value, Value, _Bindings} when is_boolean(Value) ->
                Value
        end
    catch
        error:{unbound_var, _}:_S ->
            true
    end.

-spec pp_blame(blame_info(), doc(), doc()) -> {ok, string()} | {error, term()}.
pp_blame({clauses, Clauses}, Pre, Post) ->
    Doc = pp_blame_clauses(Clauses, Pre, Post),
    pp_blame_doc(Doc);
pp_blame({generic, Patterns}, Pre, Post) ->
    Doc = pp_blame_generic(Patterns, Pre, Post),
    pp_blame_doc(Doc).

-spec pp_blame_generic([pattern_info()], doc(), doc()) -> doc().
pp_blame_generic(Patterns0, Pre, Post) ->
    Patterns = [pp_blame_pattern_or_guard(Pattern, Pre, Post) || Pattern <- Patterns0],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<", ">>, Doc2)
    end,
    erlfmt_algebra:fold_doc(Break, Patterns).

-spec pp_blame_doc(doc()) -> {ok, string()} | {error, term()}.
pp_blame_doc(Doc) ->
    case unicode:characters_to_list(erlfmt_algebra:format(Doc, 'infinity')) of
        Error when is_tuple(Error) ->
            {error, Error};
        Res ->
            {ok, Res}
    end.

-spec pp_blame_clauses([clause_info()], doc(), doc()) -> doc().
pp_blame_clauses(Clauses0, Pre, Post) ->
    Clauses = [pp_blame_clause(Clause, Pre, Post) || Clause <- Clauses0],
    erlfmt_algebra:fold_doc(fun erlfmt_algebra:line/2, Clauses).

-spec pp_blame_clause(clause_info(), doc(), doc()) -> doc().
pp_blame_clause({Patterns0, Conjunctions0}, Pre, Post) ->
    Patterns = pp_blame_clause_patterns(Patterns0, Pre, Post),
    Clause =
        case pp_blame_conjunctions(Conjunctions0, Pre, Post) of
            doc_nil ->
                Patterns;
            Conjunctions ->
                erlfmt_algebra:concat(Patterns, <<" when ">>, Conjunctions)
        end,
    erlfmt_algebra:group(Clause).

-spec pp_blame_clause_patterns([pattern_info()], doc(), doc()) -> doc().
pp_blame_clause_patterns(Patterns0, Pre, Post) ->
    Patterns = [pp_blame_pattern_or_guard(Pattern, Pre, Post) || Pattern <- Patterns0],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<", ">>, Doc2)
    end,
    erlfmt_algebra:concat([
        <<"(">>, erlfmt_algebra:fold_doc(Break, Patterns), <<")">>
    ]).

-spec pp_blame_conjunctions([conjunction_info()], doc(), doc()) -> doc().
pp_blame_conjunctions([], _Pre, _Post) ->
    erlfmt_algebra:empty();
pp_blame_conjunctions(Conjunctions0, Pre, Post) ->
    Conjunctions = [pp_blame_conjunction(Conjunction, Pre, Post) || Conjunction <- Conjunctions0],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<"; ">>, Doc2)
    end,
    erlfmt_algebra:fold_doc(Break, Conjunctions).

-spec pp_blame_conjunction(conjunction_info(), doc(), doc()) -> doc().
pp_blame_conjunction(Conjunction, Pre, Post) ->
    Guards = [pp_blame_pattern_or_guard(Guard, Pre, Post) || Guard <- Conjunction],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<", ">>, Doc2)
    end,
    erlfmt_algebra:fold_doc(Break, Guards).

-spec pp_blame_pattern_or_guard(pattern_info() | guard_info(), doc(), doc()) -> doc().
pp_blame_pattern_or_guard({true, Pattern}, _Pre, _Post) ->
    erlfmt_algebra:string(erl_prettypr:format(Pattern));
pp_blame_pattern_or_guard({false, Pattern}, Pre, Post) ->
    Doc = erlfmt_algebra:string(erl_prettypr:format(Pattern)),
    erlfmt_algebra:concat(Pre, Doc, Post);
pp_blame_pattern_or_guard({{map, ContainerFieldsInfo}, _Pattern}, Pre, Post) ->
    pp_blame_map(ContainerFieldsInfo, Pre, Post);
pp_blame_pattern_or_guard({{tuple, ContainerFieldsInfo}, _Pattern}, Pre, Post) ->
    pp_blame_tuple(ContainerFieldsInfo, Pre, Post).

-spec pp_blame_map([container_field_info()], doc(), doc()) -> doc().
pp_blame_map(Info, Pre, Post) ->
    Patterns = [pp_blame_map_field(I, Pre, Post) || I <- Info],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<", ">>, Doc2)
    end,
    erlfmt_algebra:concat([
        <<"#{">>, erlfmt_algebra:fold_doc(Break, Patterns), <<"}">>
    ]).

-spec pp_blame_map_field(container_field_info(), doc(), doc()) -> doc().
pp_blame_map_field({{badkey, _Value}, Pattern}, Pre, Post) ->
    pp_blame_pattern_or_guard({false, Pattern}, Pre, Post);
pp_blame_map_field({Info, Pattern}, Pre, Post) ->
    Name = erl_syntax:map_field_exact_name(Pattern),
    erlfmt_algebra:concat([
        erlfmt_algebra:string(erl_prettypr:format(Name)),
        <<" := ">>,
        pp_blame_pattern_or_guard(Info, Pre, Post)
    ]).

-spec pp_blame_tuple([container_field_info()], doc(), doc()) -> doc().
pp_blame_tuple(Info, Pre, Post) ->
    Patterns = [pp_blame_pattern_or_guard(I, Pre, Post) || {I, _} <- Info],
    Break = fun(Doc1, Doc2) ->
        erlfmt_algebra:break(Doc1, <<", ">>, Doc2)
    end,
    erlfmt_algebra:concat([
        <<"{">>, erlfmt_algebra:fold_doc(Break, Patterns), <<"}">>
    ]).

-spec error_info(term(), stacktrace()) -> {ok, error_info()} | {error, no_error_info}.
error_info(Reason, [{M, F, Args, _Meta} | _] = StackTrace) ->
    try
        case blame(Reason, StackTrace) of
            {ok, Blame} ->
                MFA = {M, F, length(Args)},
                {ok, #{
                    module => ?MODULE,
                    function => format_error,
                    cause => #{blame => Blame, mfa => MFA, args => Args}
                }};
            {error, no_blame_info} ->
                {error, no_error_info}
        end
    catch
        C:R:S ->
            ?LOG_INFO(
                "[wa_diff] Error while blaming. Please report this bug at https://github.com/whatsapp/assert.~n~ts",
                [erl_error:format_exception(C, R, S)],
                #{domain => wa_diff}
            ),
            {error, no_error_info}
    end.

-spec match_expression(tree()) -> tuple().
match_expression(Pattern) ->
    Anno = erl_anno:new(0),
    {match, Anno, Pattern, {var, Anno, '$VAR$'}}.

-spec debug_info(module()) -> {ok, debug_info()} | {error, term()}.
debug_info(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            case erl_prim_loader:get_file(File) of
                {ok, Beam, File} ->
                    case beam_lib:chunks(Beam, [debug_info]) of
                        {ok, {_Mod, [{debug_info, DebugInfo}]}} ->
                            {ok, DebugInfo};
                        E ->
                            {error, {no_debug_info, E}}
                    end;
                E ->
                    {error, {file_not_found, E}}
            end;
        E ->
            {error, {module_not_found, E}}
    end.

-spec function_clauses(mfa(), debug_info()) -> [tree()].
function_clauses(MFA, DebugInfo) ->
    {debug_info_v1 = _Version, erl_abstract_code = _Backend, Data} = DebugInfo,
    {Forms, _Options} = Data,
    lists:flatten([extract_function_clauses(Form, MFA) || Form <- Forms]).

-spec extract_function_clauses(tree(), mfa()) -> [tree()].
extract_function_clauses(Form, {_Module, Function, Arity} = _MFA) ->
    case erl_syntax:type(Form) of
        function ->
            case
                {
                    erl_syntax:concrete(erl_syntax:function_name(Form)),
                    erl_syntax:function_arity(Form)
                }
            of
                {Function, Arity} ->
                    erl_syntax:function_clauses(Form);
                _ ->
                    []
            end;
        _ ->
            []
    end.

-spec format_blame(term(), erlang:stacktrace()) -> ok.
format_blame(Reason, [{M, F, Args, _Meta} | _] = StackTrace) ->
    try
        case blame(Reason, StackTrace) of
            {ok, BlameInfo} ->
                {ok, PrettyReason} = pp_blame(BlameInfo, wa_diff:red(), wa_diff:reset()),
                io:format("~ts~n", [format_reason(Reason, PrettyReason, M, F, Args)]);
            {error, no_blame_info} ->
                ok
        end
    catch
        C:R:S ->
            ?LOG_INFO(
                "[wa_diff] Error while formatting blame info. Please report this bug at https://github.com/whatsapp/assert.~n~ts",
                [erl_error:format_exception(C, R, S)],
                #{domain => wa_diff}
            )
    end;
format_blame(_Reason, _StackTrace) ->
    ok.

-spec format_error(term(), erlang:stacktrace()) -> error_description().
format_error(R, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfo),
    Blame = maps:get(blame, Cause),
    {M, F, _A} = maps:get(mfa, Cause),
    Args = maps:get(args, Cause),
    case pp_blame(Blame, wa_diff:red(), wa_diff:reset()) of
        {ok, Pretty} ->
            Reason = format_reason(R, Pretty, M, F, Args),
            Cause#{
                general => "Diff", reason => Reason
            };
        {error, Error} ->
            ?LOG_INFO(
                "[wa_diff] Error while pretty-printing blame info. Please report this bug at https://github.com/whatsapp/assert.~n~ts",
                [Error],
                #{domain => wa_diff}
            ),
            Cause
    end.

-spec format_reason(term(), string(), atom(), atom(), number() | [term()]) ->
    string().
format_reason(function_clause, BlameInfo, M, F, Args) when is_list(Args) ->
    format_function_clause_reason(BlameInfo, M, F, length(Args), Args);
format_reason({assertMatch, Reason}, BlameInfo, _M, _F, _Args) when is_list(Reason) ->
    Value = proplists:get_value(value, Reason),
    format_badmatch_reason(BlameInfo, Value).

-spec format_function_clause_reason(string(), atom(), atom(), non_neg_integer(), [term()]) -> string().
format_function_clause_reason(BlameInfo, M, F, A, Args) ->
    Format =
        "~n~nThe following arguments were given to the ~p:~p/~p function:~n"
        "~n"
        "~ts~n~n"
        "But they did not match the following patterns:~n"
        "~n"
        "~ts~n",
    lists:flatten(io_lib:format(Format, [M, F, A, format_args(Args), BlameInfo])).

-spec format_badmatch_reason(string(), term()) -> string().
format_badmatch_reason(BlameInfo, Value) ->
    Format =
        "~n~nMatch failed:~n~n"
        "~ts~n~n"
        "For the provided input:~n~n"
        "~p~n~n",
    lists:flatten(io_lib:format(Format, [BlameInfo, Value])).

-spec format_args([term()]) -> string().
format_args(Args) ->
    string:join([format_arg(Arg) || Arg <- Args], "\n").

-spec format_arg(term()) -> string().
format_arg(Arg) ->
    lists:flatten(io_lib:format("\t* ~p", [Arg])).
