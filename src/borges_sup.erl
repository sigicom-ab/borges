%%%-------------------------------------------------------------------
%% @doc borges top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(borges_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    HandlerSpec =
        #{id => borges_handler_sup,
          modules => [borges_handler_sup],
          restart => transient,
          shutdown => infinity,
          start => {borges_handler_sup, start_link, []},
          type => worker},

    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 10},

    ChildSpecs = [HandlerSpec],
    {ok, {SupFlags, ChildSpecs}}.
