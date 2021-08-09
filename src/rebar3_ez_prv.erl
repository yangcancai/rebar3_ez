-module(rebar3_ez_prv).

-export([init/1, do/1, format_error/1, plugins_dir/1]).

-define(PROVIDER, compile).
-define(DEPS, []).

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
    rebar_api:info("Compiling rebar3_ez files...", []),
    {ok, State1} = rebar3_ez_clean:do(State),
    PluginsDir = plugins_dir(State1),
    ok = tar_ez([rebar_app_info:app_to_map(App) || App <- apps(State)], PluginsDir, State1),
    case rebar3_ez:find_missing_apps(plugins_dir(State)) of
        [] ->
            ok;
        {MissApps, Plugins} ->
            rebar_api:debug("Missing deps ~p, plugins ~p", [MissApps, Plugins]),
            ok = tar_ez(find_std_apps(MissApps), PluginsDir, State1)
    end,
    {ok, State1}.

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
tar_ez([#{out_dir := OutDir,
          name := Name,
          vsn := Vsn} =
            _App
        | Rest],
       PluginsDir,
       State) ->
    %%
    TarName = make_tar_name(Name, Vsn),
    Fs = ["ebin", "include", "priv"],
    Fs1 = exits_fs(OutDir, Fs),
    rebar_api:debug("rebar3_ez tar_ez ==> ez_file_name: ~p, list_file: ~p, list_file1: ~p",
                    [TarName, Fs, Fs1]),
    {ok, Repo} = file:get_cwd(),
    From = filename:join(OutDir, TarName),
    To = filename:join(Repo, filename:join(PluginsDir, TarName)),
    ok = filelib:ensure_dir(To),
    ok = file:set_cwd(OutDir),
    {ok, _} = zip:create(TarName, Fs1),
    ok = file:set_cwd(Repo),
    rebar_api:info("Generating ez file: ~p", [To]),
    ok = file:rename(From, To),
    tar_ez(Rest, PluginsDir, State).

make_tar_name(Name, Vsn) ->
    lists:concat([Name, "-", Vsn, ".ez"]).

exits_fs(OutDir, Fs) ->
    [F
     || F <- Fs,
        filelib:is_dir(
            filename:join(OutDir, F))].

apps(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    Apps ++ rebar_state:all_deps(State).

find_std_apps(MissApps) ->
    lists:foldl(fun find_std_app/2, [], MissApps).

find_std_apps(App, Acc) ->
    [AppDir] = filelib:wildcard(lists:concat([code:lib_dir(), App, "*"]),
    {ok, [{application, 
    App, Opts}]} = file:consult(filename:join(AppDir, filename:join("ebin",lists:concat([App,".app"])))),
    Vsn = proplists:get_value(vsn, Opts),
    [#{out_dir => AppDir, name => App, vsn => Vsn} | Acc].

