-module(borges_adapter_behaviour).

% These depend on
-type key() :: term().
-type value() :: term().
-type storage_config() :: borges_spec_behaviour:storage_config().

-callback name() -> atom().
-callback store(key(), value(), storage_config()) -> ok | {error, term()}.
-callback fetch(key(), storage_config()) -> {ok, term()} | {error, term()}.
