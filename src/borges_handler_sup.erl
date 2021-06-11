%%%-------------------------------------------------------------------
%% @doc borges top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(borges_handler_sup).

-behaviour(supervisor).

-export([start_link/0,
         maybe_start_handler/1,
         maybe_stop_handler/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 0,
          period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

maybe_start_handler(Name) ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid ->
            ChildSpec =
                #{id => {borges_handler, Name},
                  modules => [borges_handler],
                  restart => transient,
                  shutdown => infinity,
                  start => {borges_handler, start_link, [Name]},
                  type => worker},
            {ok, _Pid} = supervisor:start_child(Pid, ChildSpec),
            ok
    end.

maybe_stop_handler(Name) ->
    case whereis(?SERVER) of
        undefined -> ok;
        _Pid -> stop_handler(Name)
    end.

stop_handler(Name) ->
    case supervisor:terminate_child(?SERVER, {borges_handler, Name}) of
        ok -> supervisor:delete_child(?SERVER, {borges_handler, Name});
        Err ->
            logger:debug("error when terminating child: ~p", [Err]),
            ok
    end.
