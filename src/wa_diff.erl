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
-module(wa_diff).
%% erlfmt:ignore
% @fb-only: -oncall("whatsapp_server_devx").
-compile(warn_missing_spec_all).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    error_info/3,
    compute/2, compute/3,
    to_algebra/2,
    to_diff/3
]).

-export([red/0, green/0, reset/0]).

-export([format_error/2]).

-export([glue/3, join_docs/2]).

-type diff() :: #{equivalent := boolean(), left := side(), right := side()}.
-type side() ::
    contents()
    | literal()
    | {container_type(), meta(), [side()]}
    | block()
    | {struct_type(), meta(), [side()]}
    | struct_item().
-type contents() :: #{contents := [content()]}.
-type content() :: {boolean(), binary()}.
-type literal() :: number() | atom() | binary() | string().
-type block() :: {'__block__', meta(), [supported_input()]}.
-type meta() :: [{diff, boolean()}].
-type script() :: diffy:diffs().
-type doc() :: erlfmt_algebra:doc().
-type break() :: binary().
-type delimiter() :: binary().
-type error_description() :: #{
    pos_integer() => unicode:chardata(), general => unicode:chardata(), reason => unicode:chardata()
}.
-type wrapper_fun() :: fun((doc()) -> doc()).
-type error_info() :: #{cause => term(), module => module(), function => atom()}.
-type supported_input() :: tuple() | number() | map() | atom() | string() | binary().
-type container_type() :: tuple | list.
-type struct_type() :: map | {record, atom()}.
-type struct_item() :: {struct_item, side(), delimiter(), side()}.
-type context() :: #{records => #{{atom(), pos_integer()} => [atom()]}}.

-define(TIMEOUT, 1000).

-spec error_info(module(), dynamic(), dynamic()) -> {ok, error_info()} | {error, no_error_info}.
error_info(Module, Left0, Right0) ->
    try
        Self = self(),
        Ref = make_ref(),
        spawn(
            fun() ->
                Diff = compute(Left0, Right0, Module),
                {ok, Left} = to_diff(maps:get(left, Diff), red(), reset()),
                {ok, Right} = to_diff(maps:get(right, Diff), green(), reset()),
                Cause = #{left => Left, right => Right},
                Self ! {Ref, #{module => ?MODULE, function => format_error, cause => Cause}}
            end
        ),
        receive
            {Ref, ErrorInfo} ->
                {ok, ErrorInfo}
        after ?TIMEOUT ->
            ?LOG_INFO(
                "[wa_diff] Timeout while extracting diff. Please report this bug at https://github.com/whatsapp/assert.~n~p",
                [{Left0, Right0}],
                #{domain => wa_diff}
            ),
            {error, no_error_info}
        end
    catch
        Class:Reason:StackTrace ->
            ?LOG_INFO(
                "[wa_diff] Error extracting diff. Please report this bug at https://github.com/whatsapp/assert.~n~ts",
                [erl_error:format_exception(Class, Reason, StackTrace)],
                #{domain => wa_diff}
            ),
            {error, no_error_info}
    end.

-spec records(atom()) -> #{{atom(), pos_integer()} => [atom()]}.
records(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            case erl_prim_loader:get_file(File) of
                {ok, Beam, File} ->
                    {ok, {_Mod, [{abstract_code, {_Version, Forms}}]}} = beam_lib:chunks(Beam, [
                        abstract_code
                    ]),
                    Defs = [D || {attribute, _, record, D} <- Forms],
                    lists:foldl(fun parse_def/2, #{}, Defs);
                _ ->
                    #{}
            end;
        _ ->
            #{}
    end.

-spec parse_def(dynamic(), #{{atom(), pos_integer()} => [atom()]}) ->
    #{{atom(), pos_integer()} => [atom()]}.
parse_def({Name, Fields}, Acc) ->
    Acc#{{Name, length(Fields)} => [parse_def_field(F) || F <- Fields]}.

-spec parse_def_field(term()) -> atom().
parse_def_field({record_field, _, {atom, _, Name}}) when is_atom(Name) ->
    Name;
parse_def_field({record_field, _, {atom, _, Name}, _Expr}) when is_atom(Name) ->
    Name;
parse_def_field({typed_record_field, {record_field, _, {atom, _, Name}}, _Type}) when
    is_atom(Name)
->
    Name;
parse_def_field({typed_record_field, {record_field, _, {atom, _, Name}, _Expr}, _Type}) when
    is_atom(Name)
->
    Name.

-spec compute(supported_input(), supported_input()) -> diff().
compute(Left, Right) ->
    Context = #{records => #{}},
    diff_value(Left, Right, Context).

-spec compute(supported_input(), supported_input(), atom()) -> diff().
compute(Left, Right, Module) ->
    Context = #{records => records(Module)},
    diff_value(Left, Right, Context).

-spec to_algebra(side(), wrapper_fun()) -> doc().
to_algebra(Side, DiffWrapper) ->
    wrap_on_diff(Side, fun safe_to_algebra/2, DiffWrapper).

-spec format_error(term(), erlang:stacktrace()) -> error_description().
format_error(Reason, [{_M, _F, _Args, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfo),
    Left = maps:get(left, Cause),
    Right = maps:get(right, Cause),
    Cause#{
        general => "Diff",
        reason => io_lib:format("~nLeft: ~ts~nRight: ~ts~n ~p", [Left, Right, Reason])
    }.

-spec diff_value(supported_input(), supported_input(), context()) -> diff().
diff_value(Same, Same, _Context) ->
    #{equivalent => true, left => Same, right => Same};
diff_value(Left, Right, _Context) when is_number(Left), is_number(Right) ->
    diff_literal(Left, Right);
diff_value(Left, Right, _Context) when is_atom(Left), is_atom(Right) ->
    diff_literal(Left, Right);
diff_value(Left, Right, Context) when is_tuple(Left), is_tuple(Right) ->
    diff_tuple(tuple_to_list(Left), tuple_to_list(Right), Context);
diff_value(Left, Right, Context) when is_map(Left), is_map(Right) ->
    diff_struct(map, Left, Right, Context);
diff_value(Left, Right, Context) when is_list(Left), is_list(Right) ->
    case io_lib:printable_unicode_list(Left) andalso io_lib:printable_unicode_list(Right) of
        true ->
            diff_string(inspect_string(Left), inspect_string(Right));
        false ->
            diff_container(list, Left, Right, Context)
    end;
diff_value(Left, Right, _Context) when is_binary(Left), is_binary(Right) ->
    case is_printable(Left) andalso is_printable(Right) of
        true ->
            diff_string(inspect_binary(Left), inspect_binary(Right));
        false ->
            diff_literal(Left, Right)
    end;
diff_value(Left, Right, _Context) ->
    non_recursive_diff_value(Left, Right).

-spec is_printable(binary()) -> boolean().
is_printable(Binary) ->
    case unicode:characters_to_list(Binary) of
        List when is_list(List) ->
            io_lib:printable_unicode_list(List);
        _ ->
            false
    end.

-spec non_recursive_diff_value(supported_input(), supported_input()) -> diff().
non_recursive_diff_value(Left0, Right0) ->
    Left = update_diff_meta(Left0, true),
    Right = update_diff_meta(Right0, true),
    #{equivalent => false, left => Left, right => Right}.

-spec diff_literal(literal(), literal()) -> diff().
diff_literal(Left, Right) ->
    diff_string(inspect(Left), inspect(Right)).

-spec diff_string(binary(), binary()) -> diff().
diff_string(Left, Right) ->
    Script = diffy:diff(Left, Right),
    string_script_to_diff(Script, true, [], []).

-spec diff_container(container_type(), [supported_input()], [supported_input()], context()) ->
    diff().
diff_container(Type, Left, Right, Context) ->
    diff_container(Type, Left, Right, true, [], [], Context).

-spec diff_container(
    container_type(),
    [supported_input()],
    [supported_input()],
    boolean(),
    [side()],
    [side()],
    context()
) ->
    diff().
diff_container(Type, [HLeft | TLeft], [HRight | TRight], Equivalent0, AccLeft, AccRight, Context) ->
    #{equivalent := Equivalent, left := Left, right := Right} = diff_value(HLeft, HRight, Context),
    diff_container(
        Type,
        TLeft,
        TRight,
        Equivalent0 andalso Equivalent,
        [Left | AccLeft],
        [Right | AccRight],
        Context
    );
diff_container(Type, Left0, Right0, Equivalent, AccLeft, AccRight, _Context) ->
    Left = [update_diff_meta(L, true) || L <- Left0],
    Right = [update_diff_meta(L, true) || L <- Right0],
    #{
        equivalent => Equivalent andalso Left =:= [] andalso Right =:= [],
        left => {Type, [], lists:reverse(AccLeft, Left)},
        right => {Type, [], lists:reverse(AccRight, Right)}
    }.

-spec diff_struct(
    struct_type(),
    #{supported_input() => supported_input()},
    #{supported_input() => supported_input()},
    context()
) -> diff().
diff_struct(Type, Left0, Right0, Context) ->
    Acc0 = {
        _Equivalent = true,
        _AccLeft = [],
        _AccRight = [],
        _PendingLeft = [],
        _PendingRight = Right0
    },
    Fun = fun(LeftKey, LeftValue, {Equivalent0, AccLeft, AccRight, PendingLeft0, PendingRight0}) ->
        case maps:take(LeftKey, PendingRight0) of
            error ->
                PendingLeft = [
                    update_diff_meta({struct_item, LeftKey, divider(Type), LeftValue}, true)
                    | PendingLeft0
                ],
                {false, AccLeft, AccRight, PendingLeft, PendingRight0};
            {RightValue, PendingRight} ->
                Diff = diff_value(LeftValue, RightValue, Context),
                #{equivalent := Equivalent, left := Left, right := Right} = Diff,
                {
                    Equivalent0 andalso Equivalent,
                    [{LeftKey, Left} | AccLeft],
                    [{LeftKey, Right} | AccRight],
                    PendingLeft0,
                    PendingRight
                }
        end
    end,
    {AccEquivalent, AccLeft, AccRight, PendingLeft, PendingRight0} = maps:fold(Fun, Acc0, Left0),
    PendingRight = [update_diff_meta({struct_item, K, divider(Type), V}, true) || K := V <- PendingRight0],
    Equivalent = AccEquivalent andalso (maps:size(PendingRight0) =:= 0),
    Left = lists:sort(AccLeft) ++ lists:sort(PendingLeft),
    Right = lists:sort(AccRight) ++ lists:sort(PendingRight),
    #{equivalent => Equivalent, left => {Type, [], Left}, right => {Type, [], Right}}.

-spec divider(struct_type()) -> delimiter().
divider(map) -> <<" => ">>;
divider({record, _}) -> <<" = ">>.

-spec diff_tuple([supported_input()], [supported_input()], context()) -> diff().
diff_tuple(Left, Right, Context) ->
    case {expand_tuple(Left, Context), expand_tuple(Right, Context)} of
        {{Name, MapLeft}, {Name, MapRight}} ->
            diff_struct({record, Name}, MapLeft, MapRight, Context);
        _ ->
            diff_container(tuple, Left, Right, Context)
    end.

-spec expand_tuple([supported_input()], context()) ->
    {atom(), #{supported_input() => supported_input()}} | not_found.
expand_tuple([Name | Values], #{records := Records} = _Context) when is_atom(Name) ->
    case maps:get({Name, length(Values)}, Records, not_found) of
        not_found ->
            not_found;
        Fields ->
            Pairs = lists:zip(Fields, Values),
            {Name, maps:from_list(Pairs)}
    end;
expand_tuple(_, _) ->
    not_found.

-spec update_diff_meta(supported_input(), boolean()) -> block().
update_diff_meta(Value, true) ->
    {'__block__', [{diff, true}], [Value]}.

-spec inspect(term()) -> binary().
inspect(Term) ->
    inspect(Term, "~0tp").

-spec inspect_string(term()) -> binary().
inspect_string(Term) ->
    inspect(Term, "\"~ts\"").

-spec inspect_binary(term()) -> binary().
inspect_binary(Term) ->
    inspect(Term, "<<\"~ts\">>").

-spec inspect(term(), string()) -> binary().
inspect(Term, Format) ->
    case unicode:characters_to_binary(io_lib:format(Format, [Term])) of
        Result when is_binary(Result) -> Result;
        Error -> error({conversion_to_binary_failed, Error})
    end.

-spec safe_to_algebra(side(), wrapper_fun()) -> doc().
safe_to_algebra({tuple, [], Args}, DiffWrapper) ->
    container_to_algebra(<<"{">>, Args, <<"}">>, DiffWrapper, fun to_algebra/2);
safe_to_algebra({list, [], Args}, DiffWrapper) ->
    container_to_algebra(<<"[">>, Args, <<"]">>, DiffWrapper, fun to_algebra/2);
safe_to_algebra({map = Type, [], Args0}, DiffWrapper) ->
    Args = [with_struct_divider(Arg, divider(Type)) || Arg <- Args0],
    container_to_algebra(<<"#{">>, Args, <<"}">>, DiffWrapper, fun struct_item_to_algebra/2);
safe_to_algebra({{record, Record} = Type, [], Args0}, DiffWrapper) ->
    RecordName = atom_to_binary(Record, utf8),
    Prefix = <<"#", RecordName/binary, "{">>,
    Args = [with_struct_divider(Arg, divider(Type)) || Arg <- Args0],
    container_to_algebra(Prefix, Args, <<"}">>, DiffWrapper, fun struct_item_to_algebra/2);
safe_to_algebra(#{contents := Contents}, DiffWrapper) ->
    Fun = fun
        ({false, Content}) ->
            Content;
        ({true, Content}) ->
            DiffWrapper(Content)
    end,
    erlfmt_algebra:concat(lists:map(Fun, Contents));
safe_to_algebra(Literal, _DiffWrapper) ->
    erlfmt_algebra:string(inspect(Literal)).

-spec with_struct_divider(side() | {side(), side()}, delimiter()) -> side().
with_struct_divider({K, V}, Divider) ->
    {struct_item, K, Divider, V};
with_struct_divider(Side, _Divider) ->
    Side.

-spec container_to_algebra(delimiter(), [side()], delimiter(), wrapper_fun(), fun((side(), wrapper_fun()) -> doc())) ->
    doc().
container_to_algebra(Open, Args, Close, DiffWrapper, ItemToAlgebra) ->
    Items = [ItemToAlgebra(Item, DiffWrapper) || Item <- Args],
    Docs = erlfmt_algebra:fold_doc(fun join_docs/2, Items),
    erlfmt_algebra:group(glue(erlfmt_algebra:nest(glue(Open, <<"">>, Docs), 2), <<"">>, Close)).

-spec struct_item_to_algebra(side(), wrapper_fun()) -> doc().
struct_item_to_algebra(Item, DiffWrapper) ->
    wrap_on_diff(Item, fun safe_struct_item_to_algebra/2, DiffWrapper).

-spec safe_struct_item_to_algebra(side(), wrapper_fun()) -> doc().
safe_struct_item_to_algebra({struct_item, Key, Delimiter, Value}, DiffWrapper) ->
    erlfmt_algebra:concat([
        to_algebra(Key, DiffWrapper),
        erlfmt_algebra:string(Delimiter),
        to_algebra(Value, DiffWrapper)
    ]).

-spec glue(doc(), break(), doc()) -> doc().
glue(Doc1, BreakString, Doc2) ->
    erlfmt_algebra:concat(Doc1, erlfmt_algebra:concat(erlfmt_algebra:break(BreakString), Doc2)).

-spec join_docs(doc(), doc()) -> doc().
join_docs(Doc1, Doc2) ->
    glue(erlfmt_algebra:concat(Doc1, erlfmt_algebra:string(",")), <<" ">>, Doc2).

-spec wrap_on_diff(side(), fun((side(), wrapper_fun()) -> doc()), wrapper_fun()) -> doc().
wrap_on_diff(Side, Fun, DiffWrapper) ->
    case extract_diff_meta(Side) of
        {Expr, true} ->
            DiffWrapper(Fun(Expr, fun(X) -> X end));
        {Expr, false} ->
            Fun(Expr, DiffWrapper)
    end.

-spec extract_diff_meta(side()) -> {side(), boolean()}.
extract_diff_meta({'__block__', [{diff, true}], [Literal]}) -> {Literal, true};
extract_diff_meta(Other) -> {Other, false}.

-spec string_script_to_diff(script(), boolean(), [{boolean(), binary()}], [{boolean(), binary()}]) ->
    diff().
string_script_to_diff([], Equivalent, Left, Right) ->
    #{
        equivalent => Equivalent,
        left => #{contents => lists:reverse(Left)},
        right => #{contents => lists:reverse(Right)}
    };
string_script_to_diff([{equal, String} | Tail], Equivalent, Left0, Right0) ->
    string_script_to_diff(Tail, Equivalent, [{false, String} | Left0], [{false, String} | Right0]);
string_script_to_diff([{delete, String} | Tail], _Equivalent, Left0, Right0) ->
    string_script_to_diff(Tail, false, [{true, String} | Left0], Right0);
string_script_to_diff([{insert, String} | Tail], _Equivalent, Left0, Right0) ->
    string_script_to_diff(Tail, false, Left0, [{true, String} | Right0]).

-spec to_diff(side(), doc(), doc()) -> {ok, string()} | {error, term()}.
to_diff(Side, Pre, Post) ->
    Doc = to_algebra(Side, diff_wrapper(Pre, Post)),
    case unicode:characters_to_list(erlfmt_algebra:format(Doc, 'infinity')) of
        Error when is_tuple(Error) ->
            {error, Error};
        Res ->
            {ok, Res}
    end.

-spec diff_wrapper(doc(), doc()) -> wrapper_fun().
diff_wrapper(Pre, Post) ->
    fun(Doc) -> erlfmt_algebra:concat(Pre, Doc, Post) end.

-spec red() -> binary().
red() ->
    <<"\e[41m">>.

-spec green() -> binary().
green() ->
    <<"\e[42m">>.

-spec reset() -> binary().
reset() ->
    <<"\e[0m">>.
