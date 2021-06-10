-module(borges_ets_adapter).

-export([name/0,
         store/3,
         fetch/2,
         remove/2]).

-behaviour(borges_adapter).

-type storage_adapter_config() ::
    #{storage_adapter_config := #{table => atom()},
      _ => _}.
-type key() :: term().
-type data() :: term().

name() -> borges_ets_adapter.

-spec store(key(), data(), storage_adapter_config()) -> ok.
store(Key, Data, #{storage_adapter_config := StorageAdapterConfig} = _Config) ->
    TableName = maps:get(name, StorageAdapterConfig),
    ets:insert(TableName, {Key, Data}),
    ok.

%-spec fetch(key(), storage_adapter_config()) -> {ok, term()}.
fetch(Key, #{storage_adapter_config := StorageAdapterConfig} = _Config) ->
    TableName = maps:get(name, StorageAdapterConfig),
    case ets:lookup(TableName, Key) of
        [{_, Data}] -> {ok, Data};
        _ -> {ok, []}
    end.

remove(Key, #{storage_adapter_config := StorageAdapterConfig} = _Config) ->
    TableName = maps:get(name, StorageAdapterConfig),
    ets:delete(TableName, Key),
    ok.
