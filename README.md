rebar3_ez
=====

A rebar3 plugin for dist app to  *.ez

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:
```erlang
    {plugins, [
	    {rebar3_ez, {git, "https://github.com/yangcancai/rebar3_ez", {branch, "main"}}}
    ]}.
{provider_hooks, [
	{post, [
		{clean, {ez, clean}},
		{compile, {ez, compile}}
	]}
]}.
{ez_opts,[{plugins_dir, "plugins"}]}.
```

Then just call your plugin directly in an existing application:

```shell
$ rebar3 compile
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ez_example
===> Compiling rebar3_ez files...
===> Generating ez file: "/Users/admin/proj/erlang/rebar3_ez/_build/test/lib/rebar3_ez/test/ez_example/plugins/ez_example-1.0.ez"
===> Generating ez file: "/Users/admin/proj/erlang/rebar3_ez/_build/test/lib/rebar3_ez/test/ez_example/plugins/cowboy-2.9.0.ez"
===> Generating ez file: "/Users/admin/proj/erlang/rebar3_ez/_build/test/lib/rebar3_ez/test/ez_example/plugins/cowlib-2.11.0.ez"
===> Generating ez file: "/Users/admin/proj/erlang/rebar3_ez/_build/test/lib/rebar3_ez/test/ez_example/plugins/ranch-1.8.0.ez"

# admin @ admindeMacBook-Pro in ~/proj/erlang/rebar3_ez/_build/test/lib/rebar3_ez/test/ez_example on git:main x [16:09:42] 
$ rebar3 clean  
===> Verifying dependencies...
===> Cleaning out ez_example...
===> Cleaning compiled rebar3_ez files...
===> Deleteing rebar3_ez files ...
===> Deleteing rebar3_ez file: "plugins/cowboy-2.9.0.ez"
===> Deleteing rebar3_ez file: "plugins/cowlib-2.11.0.ez"
===> Deleteing rebar3_ez file: "plugins/ez_example-1.0.ez"
===> Deleteing rebar3_ez file: "plugins/ranch-1.8.0.ez"
===> Cleaning compiled rebar3_ez files...
===> Deleteing rebar3_ez files ...
```
