-module(rebar3_ez_prv).

-export([init/1, do/1, format_error/1, plugins_dir/1]).

-define(PROVIDER, compile).
-define(DEPS, [default, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, NewS} = rebar3_ez_clean:init(State),
    Provider =
        providers:create([{name, ?PROVIDER},            % The 'user friendly' name of the task
                          {module, ?MODULE},            % The module implementation of the task
                          {bare,
                           true},                 % The task can be run by the user, always true
                          {deps, ?DEPS},                % The list of dependencies
                          {example, "rebar3 ez compile"}, % How to use the plugin
                          {opts, []},                   % list of options understood by the plugin
                          {short_desc, "A rebar3 plugin for dist app to  *.ez"},
                          {namespace, ez},
                          {desc, "A rebar3 plugin for dist app to  *.ez"}]),
    {ok, rebar_state:add_provider(NewS, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% all deps
    DepsL = rebar_state:all_deps(State),
    %% proj apps
    ProjL = rebar_state:project_apps(State),
    % rebar_api:debug("all_deps: ~p~n", [[rebar_app_info:app_to_map(App) || App <- DepsL]]),
    % rebar_api:debug("project_apps: ~p~n", [[rebar_app_info:app_to_map(App) || App <- ProjL]]),
    % ===> project_apps: [#{applications => [kernel,stdlib],
    %                          dir =>
    %                              "/Users/admin/proj/erlang/rebar3_diameter_compiler/_build/test/lib/rebar3_diameter_compiler/test/compile/_build/prod/lib/compile",
    %                          ebin_dir =>
    %                              "/Users/admin/proj/erlang/rebar3_diameter_compiler/_build/test/lib/rebar3_diameter_compiler/test/compile/_build/prod/lib/compile/ebin",
    %                          included_applications => [],link => false,
    %                          name => compile,
    %                          out_dir =>
    %                              "/Users/admin/proj/erlang/rebar3_diameter_compiler/_build/test/lib/rebar3_diameter_compiler/test/compile/_build/prod/lib/compile",
    %                          vsn => "1.0"}]
    PluginsDir = plugins_dir(State),
    tar_ez([rebar_app_info:app_to_map(App) || App <- DepsL ++ ProjL], PluginsDir, State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% 获取plugins_dir的配置
%% 主要存放*.ez文件
plugins_dir(State) ->
    BaseDir = rebar_dir:base_dir(State),
    EzOpts = rebar_state:get(State, ez_opts, [{plugins_dir, BaseDir}]),
    proplists:get_value(plugins_dir, EzOpts).

tar_ez([], _, _) ->
    ok;
tar_ez([#{ebin_dir := EbinDir,
          out_dir := OutDir,
          name := Name,
          vsn := Vsn} =
            _App
        | Rest],
       PluginsDir,
       State) ->
    %%
    TarName = make_tar_name(PluginsDir, Name, Vsn),
    Fs = [EbinDir, filename:join(OutDir, "include"), filename:join(OutDir, "priv")],
    Fs1 = exits_fs(Fs),
    rebar_api:debug("rebar3_ez tar_ez ==> ez_file_name: ~p, list_file: ~p, list_file1: ~p",
                    [TarName, Fs, Fs1]),
    {ok, _} = zip:create(TarName, Fs1),
    tar_ez(Rest, PluginsDir, State).

make_tar_name(PluginsDir, Name, Vsn) ->
    filename:join(PluginsDir, lists:concat([Name, "-", Vsn, ".ez"])).

exits_fs(Fs) ->
    lists:flatten([case filelib:is_dir(F) of
                       true ->
                           F;
                       false ->
                           rebar_api:debug("File = ~p, ~p", [F, filelib:wildcard("*", F)]),
                           []
                   end
                   || F <- Fs]).
