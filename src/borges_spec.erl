-module(borges_spec).

% internal
-export([get_spec/1,
         validate/1,
         create/2,
         get_handler_pid/1,
         set_handler_pid/2,
         get_key_fun/1,
         get_identifier_fun/1,
         get_key_fun/2,
         get_subsets/1,
         make_internal_specification_structure/1,
         get_storage_config/1,
         get_subset_storage_config/2]).

-callback init() -> ok.
-callback terminate() -> ok.  % call at unregister, chance to undo things started by init

-optional_callbacks([init/0,
                     terminate/0]).

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
      storage_config := storage_config()}.    % Name of subset

    % true if input is related to subset, else false

    % Function modifying instance before storage

    % Function generating a set of valid keys given subset instance

    % Extend function to add new data to existing subset object

-type storage_config() ::
    #{key_fun := fun((ident()) -> key()),
      storage_adapter := atom(),
      storage_adapter_config => borges_adapter_behaviour:storage_adapter_config()}.

    % Additional config information required for storage, e.g. url, password etc
 % Not sure what this would be.

make_internal_specification_structure(Module) ->
    StorageName = Module:name(),
    Subsets = Module:subsets(),
    MainStorage = Module:main_obj(),
    SubsetsMap = maps:from_list([{Name, Def} || #{name := Name} = Def <- Subsets]),
    #{name => StorageName,
      main_storage => MainStorage,
      handler_pid => undefined,
      subsets => SubsetsMap,
      module => Module}.

%%%%%%%%%%%%% ACCESS FUNCTIONS %%%%%%%%%%%%
validate(_StorageSpec) -> ok.

create(Name, Spec) ->
    PTKey = {?MODULE, Name},
    undefined = persistent_term:get(PTKey, undefined),
    ok = persistent_term:put(PTKey, Spec).

get_spec(Name) ->
    PTKey = {?MODULE, Name},
    persistent_term:get(PTKey).

get_subset_spec(Name, SubsetName) ->
    Spec = get_spec(Name),
    #{module := Module} = Spec,
    AllSubsets = Module:subsets(),
    find_subset(AllSubsets, SubsetName).

% TODO: rewrite as map
find_subset([], _SubsetName) -> undefined;
find_subset([Subset | Rest], SubsetName) ->
    SubsetName2 = maps:get(name, Subset),
    case SubsetName == SubsetName2 of
        true -> Subset;
        false -> find_subset(Rest, SubsetName)
    end.

get_subsets(Name) ->
    #{subsets := Subsets} = get_spec(Name),
    Subsets.

get_storage_config(Name) ->
    #{main_storage := StorageConfig} = get_spec(Name),
    StorageConfig.

get_subset_storage_config(Name, SubsetName) ->
    #{storage_config := SubsetConfig} = get_subset_spec(Name, SubsetName),
    SubsetConfig.

get_handler_pid(Name) ->
    #{handler_pid := Handler} = get_spec(Name),
    Handler.

set_handler_pid(Name, Pid) ->
    PTKey = {?MODULE, Name},
    Spec0 = persistent_term:get(PTKey),
    Spec1 = Spec0#{handler_pid => Pid},
    ok = persistent_term:put(PTKey, Spec1).

get_key_fun(Name) ->
    #{key_fun := KeyFun} = get_storage_config(Name),
    KeyFun.

get_key_fun(Name, SubsetName) ->
    #{key_fun := KeyFun} = get_subset_storage_config(Name, SubsetName),
    KeyFun.

get_identifier_fun(Name) ->
    #{module := Module} = get_spec(Name),
    fun Module:storage_identifier/1.
