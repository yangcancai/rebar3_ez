-module(rebar3_ez).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_ez_prv:init(State),
    {ok, State1}.
