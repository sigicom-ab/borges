%%%-------------------------------------------------------------------
%% @doc borges public API
%% @end
%%%-------------------------------------------------------------------

-module(borges_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) -> borges_sup:start_link().

stop(_State) -> ok.

%% internal functions
