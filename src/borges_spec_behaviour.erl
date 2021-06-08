-module(borges_spec_behaviour).

-callback init() -> ok.
-callback terminate() -> ok.  % call at unregister, chance to undo things started by init

-optional_callbacks([init/0,
                     terminate/0]).

-export_type([storage_config/0]).

-callback name() -> atom().
-callback main_obj() -> storage_config(). % TODO: main_storage
-callback subsets() -> [subset_config()].
-callback storage_identifier(main_obj_data()) -> ident().

-type subset_data() :: term().
-type subset_obj_data() :: term() | [subset_data()].
-type main_obj_data() :: term().
-type key() :: term().
-type ident() :: term().
-type subset_config() ::
    #{name => atom(),
      is_related := fun((main_obj_data()) -> boolean()),
      data_prep := fun((main_obj_data()) -> subset_data()),
      subset_keys_fun := fun((main_obj_data()) -> [ident()]),
      extend := fun((atom(), subset_data(), ident()) -> subset_obj_data()),
      reduce := fun((atom(), subset_data(), ident()) -> subset_obj_data()),
      storage_config := storage_config()}.
-type storage_config() ::
    #{key_fun := fun((ident()) -> key()),
      storage_adapter := atom(),
      storage_adapter_config => term()}.
