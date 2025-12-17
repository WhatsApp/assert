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

-module(wa_assert_parse_trans_SUITE).
%% erlfmt:ignore
% @fb-only: -oncall("whatsapp_server_devx").

-include_lib("common_test/include/ct.hrl").
-include("assert.hrl").

%% Test server callbacks
-export([
    all/0
]).

%% Test cases
-export([
    parse_transform_assert_match/1,
    parse_transform_expand_assert/1
]).

all() ->
    [
        parse_transform_assert_match,
        parse_transform_expand_assert
    ].

%%--------------------------------------------------------------------
%% TEST CASES

parse_transform_assert_match(Config) ->
    Source = file_path("assert_match.erl", Config),
    Expected = file_path("assert_match.expected", Config),
    assert_parse_transform(Source, Expected).

parse_transform_expand_assert(Config) ->
    Source = file_path("expand_assert.erl", Config),
    Expected = file_path("expand_assert.expected", Config),
    assert_parse_transform(Source, Expected).

%%--------------------------------------------------------------------
%% Internal Helpers
%%--------------------------------------------------------------------

file_path(Name, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Name]).

assert_parse_transform(Source, Expected0) ->
    AST = compile(Source),
    Pretty = string:join([erl_prettypr:format(Tree) || Tree <- AST], "\n"),
    {ok, Expected} = file:read_file(Expected0),
    ?assertEqual(Expected, unicode:characters_to_binary(Pretty)).

compile(File) ->
    {ok, [], AST} = compile:file(File, [binary, deterministic, 'P']),
    AST.
