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
-module(wa_assert_parse_trans).
%% erlfmt:ignore
% @fb-only: -oncall("whatsapp_server_devx").
-compile(warn_missing_spec_all).

%% Public API
-export([parse_transform/2, format_error/1]).

-define(WA_ASSERT, wa_assert).

-define(call(A, M, F, As), {call, A, {remote, A, {atom, A, M}, {atom, A, F}}, As}).
-define(map_key(A, K, V), {map_field_assoc, A, {atom, A, K}, V}).
% List from https://www.erlang.org/doc/system/expressions.html#term-comparisons
-define(COMPARISON_OPERATORS, ['==', '/=', '=<', '<', '>=', '>', '=:=', '=/=']).
% Arithmetic operators that produce intermediate values to show in errors
-define(ARITHMETIC_OPERATORS, ['+', '-', '*', '/', 'div', 'rem']).
% Short-circuit operators where we skip intermediate extraction to avoid
% breaking short-circuit semantics (RHS may not be evaluated at runtime)
-define(SHORT_CIRCUIT_OPERATORS, ['orelse', 'andalso']).
% Performance limits for intermediate extraction
-define(MAX_INTERMEDIATE_DEPTH, 3).
-define(MAX_INTERMEDIATES, 10).

-type tree() :: erl_syntax:syntaxTree().

-spec parse_transform([tree()], [term()]) -> [tree()].
parse_transform(Forms, _Options) ->
    [transform_form(Form) || Form <- Forms].

-spec transform_form(tree()) -> tree().
transform_form(Form) ->
    case erl_syntax:type(Form) of
        function ->
            Annotated = erl_syntax_lib:annotate_bindings(Form, ordsets:new()),
            try
                erl_syntax:revert(erl_syntax_lib:map(fun transform_expr/1, Annotated))
            catch
                {error_marker, Line, Reason} -> {error, {erl_anno:location(Line), ?MODULE, Reason}}
            end;
        _ ->
            Form
    end.

-spec transform_expr(tree()) -> tree().
transform_expr(Expr) ->
    case erl_syntax:type(Expr) of
        application ->
            case erl_syntax_lib:analyze_application(Expr) of
                {?WA_ASSERT, {'$assert_match_error_info$', 1}} ->
                    Anno = erl_syntax:get_pos(Expr),
                    case erl_anno:generated(Anno) of
                        % Do nothing if this code is annoted by generated
                        % Likely a result of another parse transform
                        true ->
                            Expr;
                        _ ->
                            process_assert_match_error_info(
                                erl_syntax:get_pos(Expr), erl_syntax:application_arguments(Expr)
                            )
                    end;
                {?WA_ASSERT, {'$expand_assert$', 1}} ->
                    Anno = erl_syntax:get_pos(Expr),
                    case erl_anno:generated(Anno) of
                        % Do nothing if this code is annoted by generated
                        % Likely a result of another parse transform
                        true ->
                            Expr;
                        _ ->
                            process_expand_assert(erl_syntax:application_arguments(Expr))
                    end;
                _ ->
                    Expr
            end;
        _ ->
            Expr
    end.

-spec process_assert_match_error_info(erl_anno:anno() | erl_anno:location(), [tree()]) -> tuple().
process_assert_match_error_info(Anno, [Expr0]) ->
    Pattern0 = extract_pattern(Expr0),
    PatternStr = {string, Anno, format_expr(Pattern0)},
    Pins = extract_pins(Anno, Pattern0),
    PinsVar = {var, Anno, 'Pins'},
    {block, Anno, [
        {match, Anno, PinsVar, {map, Anno, Pins}},
        ?call(Anno, ?WA_ASSERT, error_info, [PatternStr, PinsVar])
    ]}.

-spec process_expand_assert([tree()]) -> tree().
process_expand_assert([Expr]) ->
    Pins = extract_pins(0, Expr),
    Line = get_line(Expr),
    case erl_syntax:type(Expr) of
        infix_expr ->
            Operator = erl_syntax:infix_expr_operator(Expr),
            case lists:member(erl_syntax:operator_name(Operator), ?COMPARISON_OPERATORS) of
                true ->
                    expand_comparison(Expr, Operator, Pins, Line);
                false ->
                    expand_generic(Expr, Pins, Line)
            end;
        _ ->
            expand_generic(Expr, Pins, Line)
    end.

-spec get_line(tree()) -> non_neg_integer().
get_line(Expr) ->
    Anno = erl_syntax:get_pos(Expr),
    case Anno of
        N when is_integer(N) -> N;
        _ ->
            case erl_anno:is_anno(Anno) of
                true -> erl_anno:line(Anno);
                false -> 0
            end
    end.

-spec expand_comparison(tree(), tree(), [tree()], non_neg_integer()) -> tree().
expand_comparison(Expr, Operator, Pins, Line) ->
    Left0 = erl_syntax:infix_expr_left(Expr),
    Right0 = erl_syntax:infix_expr_right(Expr),
    LeftVar = erl_syntax:variable('Left__Left'),
    RightVar = erl_syntax:variable('Right__Right'),
    {Left, LeftBindings, LeftEntries, N} = transform_intermediates(Left0, Line, 0),
    {Right, RightBindings, RightEntries, _} = transform_intermediates(Right0, Line, N),
    IntermediateBindings = LeftBindings ++ RightBindings,
    IntermediateFields =
        case LeftEntries ++ RightEntries of
            [] ->
                [];
            Entries ->
                [
                    erl_syntax:map_field_assoc(
                        erl_syntax:atom(intermediates),
                        erl_syntax:list(Entries)
                    )
                ]
        end,
    ComparisonFields = [
        erl_syntax:map_field_assoc(erl_syntax:atom(left), LeftVar),
        erl_syntax:map_field_assoc(erl_syntax:atom(right), RightVar),
        erl_syntax:map_field_assoc(
            erl_syntax:atom(operator), erl_syntax:atom(erl_syntax:operator_name(Operator))
        )
    ],
    BoolExpr = erl_syntax:infix_expr(LeftVar, Operator, RightVar),
    build_result_block(
        IntermediateBindings ++
            [erl_syntax:match_expr(LeftVar, Left), erl_syntax:match_expr(RightVar, Right)],
        BoolExpr,
        comparison,
        Pins,
        ComparisonFields ++ IntermediateFields
    ).

-spec expand_generic(tree(), [tree()], non_neg_integer()) -> tree().
expand_generic(Expr, Pins, Line) ->
    {TransformedExpr, IntermediateBindings, IntermediateEntries} = transform_intermediates(Expr, Line),
    IntermediateFields =
        case IntermediateEntries of
            [] ->
                [];
            Entries ->
                [
                    erl_syntax:map_field_assoc(
                        erl_syntax:atom(intermediates),
                        erl_syntax:list(Entries)
                    )
                ]
        end,
    build_result_block(IntermediateBindings, TransformedExpr, generic, Pins, IntermediateFields).

-spec build_result_block([tree()], tree(), atom(), [tree()], [tree()]) -> tree().
build_result_block(Bindings, BoolExpr, Type, Pins, ExtraFields) ->
    MetaFields =
        [
            erl_syntax:map_field_assoc(erl_syntax:atom(type), erl_syntax:atom(Type)),
            erl_syntax:map_field_assoc(erl_syntax:atom(pins), erl_syntax:map_expr(Pins))
        ] ++ ExtraFields,
    ResultMap = erl_syntax:map_expr([
        erl_syntax:map_field_assoc(erl_syntax:atom(bool_expr), BoolExpr),
        erl_syntax:map_field_assoc(erl_syntax:atom(meta), erl_syntax:map_expr(MetaFields))
    ]),
    erl_syntax:block_expr(Bindings ++ [ResultMap]).

-doc """
Single-pass intermediate extraction and substitution.
Walks the AST once, replacing each intermediate with a fresh variable.
Returns {TransformedExpr, Bindings, Entries} where each intermediate
occurrence gets its own unique variable at the moment of discovery.
Line is used to make variable names unique per assertion.
Depth and count limits prevent pathological cases.
""".
-spec transform_intermediates(tree(), non_neg_integer()) -> {tree(), [tree()], [tree()]}.
transform_intermediates(Expr, Line) ->
    {TransformedExpr, Bindings, Entries, _N} = transform_intermediates(Expr, Line, 0),
    {TransformedExpr, Bindings, Entries}.

%% Version that accepts a starting counter, returns final counter for chaining.
-spec transform_intermediates(tree(), non_neg_integer(), non_neg_integer()) ->
    {tree(), [tree()], [tree()], non_neg_integer()}.
transform_intermediates(Expr, Line, StartN) ->
    case contains_short_circuit(Expr) of
        % skip expressions that can short-circuit
        true ->
            {Expr, [], [], StartN};
        false ->
            {TransformedExpr, Bindings, Entries, N} =
                transform_intermediates_impl(Expr, [], [], Line, StartN, 0),
            {TransformedExpr, lists:reverse(Bindings), lists:reverse(Entries), N}
    end.

%% Stop extracting intermediates beyond max depth or count to prevent pathological cases.
-spec transform_intermediates_impl(tree(), [tree()], [tree()], non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {tree(), [tree()], [tree()], non_neg_integer()}.
transform_intermediates_impl(Expr, Bindings, Entries, _Line, N, Depth) when
    Depth > ?MAX_INTERMEDIATE_DEPTH; N >= ?MAX_INTERMEDIATES
->
    {Expr, Bindings, Entries, N};
transform_intermediates_impl(Expr, Bindings, Entries, Line, N, Depth) ->
    case erl_syntax:type(Expr) of
        T when
            % skip expressions with local bindings
            T =:= fun_expr;
            T =:= implicit_fun;
            T =:= named_fun_expr;
            T =:= list_comp;
            T =:= binary_comp;
            T =:= map_comp;
            T =:= block_expr;
            T =:= case_expr;
            T =:= if_expr;
            T =:= receive_expr;
            T =:= try_expr;
            T =:= catch_expr;
            T =:= maybe_expr;
            T =:= match_expr
        ->
            {Expr, Bindings, Entries, N};
        _ ->
            case erl_syntax:subtrees(Expr) of
                [] ->
                    {Expr, Bindings, Entries, N};
                Groups ->
                    case is_intermediate(Expr) of
                        true ->
                            ExprStr = format_expr(Expr),
                            {NewGroups, Bindings1, Entries1, N1} =
                                transform_subtrees(Groups, Bindings, Entries, Line, N, Depth + 1),
                            TransformedExpr = erl_syntax:update_tree(Expr, NewGroups),
                            VarName = list_to_atom(
                                "Intermediate__" ++ integer_to_list(Line) ++ "_" ++ integer_to_list(N1)
                            ),
                            Var = erl_syntax:variable(VarName),
                            Binding = erl_syntax:match_expr(Var, TransformedExpr),
                            Entry = erl_syntax:tuple([erl_syntax:string(ExprStr), Var]),
                            {Var, [Binding | Bindings1], [Entry | Entries1], N1 + 1};
                        false ->
                            {NewGroups, Bindings1, Entries1, N1} =
                                transform_subtrees(Groups, Bindings, Entries, Line, N, Depth),
                            TransformedExpr = erl_syntax:update_tree(Expr, NewGroups),
                            {TransformedExpr, Bindings1, Entries1, N1}
                    end
            end
    end.

-spec transform_subtrees([[tree()]], [tree()], [tree()], non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {[[tree()]], [tree()], [tree()], non_neg_integer()}.
transform_subtrees(Groups, Bindings, Entries, Line, N, Depth) ->
    {RevGroups, FinalBindings, FinalEntries, FinalN} = lists:foldl(
        fun(Group, {AccGroups, AccBindings, AccEntries, AccN}) ->
            {TransformedGroup, NewBindings, NewEntries, NewN} =
                transform_group(Group, AccBindings, AccEntries, Line, AccN, Depth),
            {[TransformedGroup | AccGroups], NewBindings, NewEntries, NewN}
        end,
        {[], Bindings, Entries, N},
        Groups
    ),
    {lists:reverse(RevGroups), FinalBindings, FinalEntries, FinalN}.

-spec transform_group([tree()], [tree()], [tree()], non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {[tree()], [tree()], [tree()], non_neg_integer()}.
transform_group(Group, Bindings, Entries, Line, N, Depth) ->
    {RevChildren, FinalBindings, FinalEntries, FinalN} = lists:foldl(
        fun(Child, {AccChildren, AccBindings, AccEntries, AccN}) ->
            {Transformed, NewBindings, NewEntries, NewN} =
                transform_intermediates_impl(Child, AccBindings, AccEntries, Line, AccN, Depth),
            {[Transformed | AccChildren], NewBindings, NewEntries, NewN}
        end,
        {[], Bindings, Entries, N},
        Group
    ),
    {lists:reverse(RevChildren), FinalBindings, FinalEntries, FinalN}.

-spec contains_short_circuit(tree()) -> boolean().
contains_short_circuit(Expr) ->
    case erl_syntax:type(Expr) of
        infix_expr ->
            Op = erl_syntax:operator_name(erl_syntax:infix_expr_operator(Expr)),
            lists:member(Op, ?SHORT_CIRCUIT_OPERATORS) orelse
                contains_short_circuit(erl_syntax:infix_expr_left(Expr)) orelse
                contains_short_circuit(erl_syntax:infix_expr_right(Expr));
        _ ->
            lists:any(
                fun(Group) -> lists:any(fun contains_short_circuit/1, Group) end,
                erl_syntax:subtrees(Expr)
            )
    end.

-spec is_intermediate(tree()) -> boolean().
is_intermediate(Node) ->
    case erl_syntax:type(Node) of
        application ->
            true;
        infix_expr ->
            lists:member(
                erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)),
                ?ARITHMETIC_OPERATORS
            );
        _ ->
            false
    end.

-spec extract_pins(erl_anno:anno() | erl_anno:location(), tree()) -> [tree()].
extract_pins(Anno, Expr) ->
    Attrs = erl_syntax:get_ann(Expr),
    pins(Anno, Attrs).

-spec extract_pattern(tree()) -> tree().
extract_pattern(Expr) ->
    case erl_syntax:type(Expr) of
        case_expr ->
            [Clause] = erl_syntax:case_expr_clauses(Expr),
            [Pattern] = erl_syntax:clause_patterns(Clause),
            Pattern;
        _ ->
            throw(erl_syntax:error_marker({assert_match, Expr}))
    end.

-spec pins(erl_anno:anno() | erl_anno:location(), dynamic()) -> [tuple()].
pins(Anno, Attrs) ->
    {free, Free} = lists:keyfind(free, 1, Attrs),
    {env, Env} = lists:keyfind(env, 1, Attrs),
    [?map_key(Anno, Name, {var, Anno, Name}) || Name <- Free, lists:member(Name, Env)].

%% Format expression to string, with special handling for floats to avoid
%% erl_prettypr's scientific notation (e.g., "5.99999999999999977796e-1" instead of "0.6")
%% Short circuit if there are no floats in the expression.
-spec format_expr(tree()) -> string().
format_expr(Expr) ->
    case has_float(Expr) of
        true ->
            Transformed = erl_syntax_lib:map(fun transform_floats/1, Expr),
            erl_prettypr:format(Transformed);
        false ->
            erl_prettypr:format(Expr)
    end.

-spec has_float(tree()) -> boolean().
has_float(Expr) ->
    erl_syntax_lib:fold(
        fun(Node, Acc) ->
            Acc orelse erl_syntax:type(Node) =:= float
        end,
        false,
        Expr
    ).

-spec transform_floats(tree()) -> tree().
transform_floats(Node) ->
    case erl_syntax:type(Node) of
        float ->
            erl_syntax:text(float_to_list(erl_syntax:float_value(Node), [short]));
        _ ->
            Node
    end.

-spec format_error(erl_lint:error_info()) -> io_lib:chars().
format_error(E) ->
    io_lib:format("~p", [E]).
