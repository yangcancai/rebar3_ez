-module(rebar3_ez).

-export([init/1, find_missing_apps/1]).

-include_lib("stdlib/include/zip.hrl").
-include_lib("kernel/include/file.hrl").

-record(plugin,
        {name = undefined :: atom(),
         vsn = <<"0.1.0">> :: binary(),
         desc = <<"">> :: binary(),
         location = <<>> :: binary(),
         applications = [] :: list(), %% 依赖
         extra_applications = [] :: list()}). %% 系统未加载的依赖

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_ez_prv:init(State),
    {ok, State1}.

find_missing_apps(PluginsDir) ->
    Plugins = list_plugins(PluginsDir),
    lists:foldl(fun find_missing_apps/2, {[], Plugins}, Plugins).

find_missing_apps(#plugin{extra_applications = ExtraApps}, {Acc, Plugins}) ->
    {[App || App <- ExtraApps, not lists:keymember(App, #plugin.name, Plugins)] ++ Acc,
     Plugins}.

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
            extra_applications = Dependencies}.

parse_binary(Bin) ->
    try
        {ok, Ts, _} = erl_scan:string(binary_to_list(Bin)),
        {ok, Term} = erl_parse:parse_term(Ts),
        Term
    catch
        Err ->
            {error, {invalid_app, Err}}
    end.
