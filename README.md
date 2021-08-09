rebar3_ez
=====

A rebar3 plugin for dist app to  *.ez

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_ez, {git, "https://host/user/rebar3_ez.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 ez
    ===> Fetching rebar3_ez
    ===> Compiling rebar3_ez
    <Plugin Output>
# rebar3_ez
