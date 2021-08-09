%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%%% @doc
%%%
%%% @end
%%% Created : 2021-08-09T03:06:30+00:00
%%%-------------------------------------------------------------------
-module(rebar3_ez_clean).

-author("yangcancai").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER},            % The 'user friendly' name of the task
                          {module, ?MODULE},            % The module implementation of the task
                          {bare,
                           true},                 % The task can be run by the user, always true
                          {deps, ?DEPS},                % The list of dependencies
                          {example, "rebar3 ez clean"}, % How to use the plugin
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc, "clean *.ez"},
                          {namespace, ez},
                          {desc, "A rebar3 plugin for dist app to  *.ez"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Cleaning compiled rebar3_ez files...", []),
    PluginsDir = rebar3_ez_prv:plugins_dir(State),
    clean(PluginsDir),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

clean(Dir) ->
    case filelib:is_dir(Dir) of
        false ->
            ok;
        true ->
            rebar_api:info("Deleteing rebar3_ez files ...", []),
            Files = [filename:join(Dir, File) || File <- filelib:wildcard("*", Dir)],
            delete_file(Files)
    end.

delete_file([]) ->
    ok;
delete_file([File | Rest]) ->
    rebar_api:info("Deleteing rebar3_ez file: ~p", [File]),
    case filelib:is_dir(File) of
        true ->
            case prim_file:list_dir(File) of
                {ok, List} ->
                    delete_file([filename:join(File, File1) || File1 <- List]),
                    prim_file:del_dir(File),
                    delete_file(Rest);
                {error, Why} ->
                    rebar_api:error("List dir ~p ~p", [File, Why]),
                    delete_file(Rest)
            end;
        false ->
            case prim_file:delete(File) of
                ok ->
                    delete_file(Rest);
                {error, enoent} ->
                    delete_file(Rest);
                {error, Why} ->
                    rebar_api:error("Detele file ~p ~p", [File, Why]),
                    delete_file(Rest)
            end
    end.
