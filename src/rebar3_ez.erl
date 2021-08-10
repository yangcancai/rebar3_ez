-module(rebar3_ez).

-export([init/1, find_missing_apps/2, lib_dir/1]).

-include_lib("stdlib/include/zip.hrl").
-include_lib("kernel/include/file.hrl").

-record(plugin,
        {name = undefined :: atom(),
         vsn = <<"0.1.0">> :: binary(),
         desc = <<"">> :: binary(),
         location = <<>> :: binary(),
         applications = [] :: list(), %% 依赖
         extra_applications = [] :: list()}). %% 系统未加载的依赖

-define(EXINCLUDE_STD_LIBS, [kernel, stdlib]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_ez_prv:init(State),
    {ok, State1}.

find_missing_apps(PluginsDir, State) ->
    Plugins = list_plugins(PluginsDir),
    {StdDeps, _} = lists:foldl(fun do_find_missing_apps/2, {[], Plugins}, Plugins),
    %% find deps for std lib
    {find_std_deps(StdDeps, State), Plugins}.

do_find_missing_apps(#plugin{extra_applications = ExtraApps}, {Acc, Plugins}) ->
    {[App
      || App <- ExtraApps,
         not lists:keymember(App, #plugin.name, Plugins) andalso not lists:member(App, Acc)]
         ++ Acc,
     Plugins}.

find_std_deps(StdDeps, State) ->
    find_std_deps(StdDeps, StdDeps, State).

find_std_deps([], Acc, _State) ->
    Acc;
find_std_deps([App | Rest], Acc, State) ->
    [AppDir] =
        filelib:wildcard(
            filename:join(lib_dir(State), lists:concat([App, "*"]))),
    {ok, [{application, App, Opts}]} =
        file:consult(
            filename:join(AppDir, filename:join("ebin", lists:concat([App, ".app"])))),

    Deps =
        [Dep
         || Dep <- proplists:get_value(applications, Opts, []),
            not lists:member(Dep, Acc) andalso not lists:member(Dep, ?EXINCLUDE_STD_LIBS)],
    find_std_deps(Rest ++ Deps, Acc ++ Deps).

list_plugins(PluginsDir) ->
    Files = [filename:join(PluginsDir, File) || File <- filelib:wildcard("*.ez", PluginsDir)],
    lists:foldl(fun(F, Acc) ->
                   {ok, [_ | L]} = zip:list_dir(F),
                   case app_deps_info(L, F) of
                       #plugin{} = Plugin -> [Plugin | Acc];
                       _ -> Acc
                   end
                end,
                [],
                Files).

app_deps_info(L, PluginDesc) when is_list(L) ->
    [File] = [F || #zip_file{name = F} <- L, lists:suffix(".app", F)],
    {ok, [{_, B}]} = zip:extract(PluginDesc, [{file_list, [File]}, memory]),
    case parse_binary(B) of
        {application, Name, Props} ->
            app_deps_info(Name, Props, PluginDesc);
        Err ->
            rebar_api:error("Plugin app file ~p format ~p", [File, Err]),
            []
    end.

app_deps_info(Name, Props, PluginDesc) ->
    Version = proplists:get_value(vsn, Props, "0"),
    Description = proplists:get_value(description, Props, ""),
    Dependencies = proplists:get_value(applications, Props, []),
    #plugin{name = Name,
            vsn = Version,
            desc = Description,
            location = PluginDesc,
            applications = Dependencies,
            extra_applications =
                [App || App <- Dependencies, not lists:member(App, ?EXINCLUDE_STD_LIBS)]}.

parse_binary(Bin) ->
    try
        {ok, Ts, _} = erl_scan:string(binary_to_list(Bin)),
        {ok, Term} = erl_parse:parse_term(Ts),
        Term
    catch
        Err ->
            {error, {invalid_app, Err}}
    end.

lib_dir(State) ->
    CurrentProfiles = rebar_state:current_profiles(State),
    Rs = case lists:member(prod, CurrentProfiles) of
             true ->
                 Profiles = rebar_state:get(State, profiles, []),
                 ProdOpts = proplists:get_value(prod, Profiles, []),
                 Relx = proplists:get_value(relx, ProdOpts, []),
                 proplists:get_value(system_libs, Relx, code:lib_dir());
             false ->
                 code:lib_dir()
         end,
    rebar_api:debug("Rebar3_ez System_libs ~p", [Rs]),
    Rs.
