-module(borges_adapter).

-export([get/2,
         get_subset/3,
         store/3,
         store_subset/4]).

% These depend on
-type key() :: term().
-type value() :: term().
%-type storage_adapter_config() :: term().
-type storage_config() :: borges_spec_behaviour:storage_config().

-callback name() -> atom().
-callback store(key(), value(), storage_config()) -> ok | {error, term()}.
-callback fetch(key(), storage_config()) -> {ok, term()} | {error, term()}.

store(ModelName, Input, Term) ->
    KeyFun = borges_spec:get_key_fun(ModelName),
    StoreIdentifier = KeyFun(Input),
    Config = borges_spec:get_storage_config(ModelName),
    internal_store(StoreIdentifier, Term, Config).

store_subset(ModelName, SubsetName, Input, Term) ->
    KeyFun = borges_spec:get_key_fun(ModelName, SubsetName),
    StoreIdentifier = KeyFun(Input),
    Config = borges_spec:get_subset_storage_config(ModelName, SubsetName),
    internal_store(StoreIdentifier, Term, Config).

internal_store(StoreIdentifier, Term, #{storage_adapter := StorageAdapter} = Config) ->
    StorageAdapter:store(StoreIdentifier, Term, Config).

get(ModelName, Input) ->
    KeyFun = borges_spec:get_key_fun(ModelName),
    StoreIdentifier = KeyFun(Input),
    Config = borges_spec:get_storage_config(ModelName),
    internal_get(StoreIdentifier, Config).

get_subset(ModelName, SubsetName, Input) ->
    KeyFun = borges_spec:get_key_fun(ModelName, SubsetName),
    StoreIdentifier = KeyFun(Input),
    Config = borges_spec:get_subset_storage_config(ModelName, SubsetName),
    internal_get(StoreIdentifier, Config).

internal_get(StoreIdentifier, #{storage_adapter := StorageAdapter} = Config) ->
    StorageAdapter:fetch(StoreIdentifier, Config).
