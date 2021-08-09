%%%-------------------------------------------------------------------
%%% @author admin

%%% @doc
%%%
%%% @end
%%% Created : 2021-06-16T10:14:55+00:00
%%%-------------------------------------------------------------------

-module(rebar3_ez_test).

-include_lib("eunit/include/eunit.hrl").

compile_test_() ->
    {setup, fun() -> setup() end, {timeout, 60, [fun() -> test_compile() end]}}.

%% Internal

setup() ->
    {ok, Repo} = file:get_cwd(),
    Branch = setup_git_branch(),
    Test_target = test_target(Repo),
    setup_delete(Test_target),
    setup_create(Test_target, Repo, Branch).

setup_create(Test_target, Repo, Branch) ->
    ok = setup_create_src(Test_target),
    ok = setup_create_rebar_config(Test_target, Repo, Branch).

setup_create_src(Test_target) ->
    App = "ez_example",
    File = filename:join([Test_target, "src", App ++ ".app.src"]),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_create_src_content(App)).

setup_create_src_content(App) ->
    "\n{application, " ++
        App ++
            ", [\n\t{description, \"" ++
                App ++ "\"},\n\t{vsn, \"1.0\"},\n\t{applications, [kernel,stdlib]}\n]}.\n".

setup_create_rebar_config(Test_target, Repo, Branch) ->
    File = filename:join(Test_target, "rebar.config"),
    ok = filelib:ensure_dir(File),
    file:write_file(File, setup_rebar_config_content(Repo, Branch)).

setup_delete(Directory) ->
    Paths =
        filelib:wildcard(
            filename:join(Directory, "*")),
    {Directories, Files} = lists:partition(fun filelib:is_dir/1, Paths),
    [file:delete(X) || X <- Files],
    [setup_delete(X) || X <- Directories],
    file:del_dir(Directory).

setup_git_branch() ->
    string:trim(
        os:cmd("git branch --show-current")).

setup_rebar_config_content(Repo, Branch) ->
    "\n{plugins, [\n\t{rebar3_ez, {git, \"file://" ++
        Repo ++
            "\", {branch, \"" ++
                Branch ++
                    "\"}}}\n]}.\n{provider_hooks, [\n\t{post, [\n\t\t{clean, {ez, "
                    "clean}},\n\t\t{compile, {ez, compile}}\n\t]}\n]}.\n{ez_opts,[{plugin"
                    "s_dir, \"plugins\"}]}.\n{deps,[jiffy]}.".

                    % "s_dir, \"plugins\"}]}.\n{deps,[{cowboy,\"2.9.0\"}]}.".

test_compile() ->
    {ok, Repo} = file:get_cwd(),
    Test_target = test_target(Repo),
    ok = file:set_cwd(Test_target),
    % Result = os:cmd("DIAGNOSTIC=1 rebar3 eunit"),
    % ?debugMsg(Result),
    ?assertCmd("rebar3 eunit"),
    true =
        filelib:is_regular(
            filename:join("plugins", "cowboy-2.9.0.ez")),
    true =
        filelib:is_regular(
            filename:join("plugins", "cowlib-2.11.0.ez")),
    true =
        filelib:is_regular(
            filename:join("plugins", "ranch-1.8.0.ez")),
    true =
        filelib:is_regular(
            filename:join("plugins", "ez_example-1.0.ez")),

    file:set_cwd(Repo).

test_target(Repo) ->
    filename:join([Repo, "_build", "test", "lib", "rebar3_ez", "test", "ez_example"]).
