-module(borges_spec).

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

% Name of module containing spec
-type spec_module_name() :: atom().
% Name of spec, i.e. what is returned by running name/0 in the spec module
-type spec_name() :: atom().
% Name of a subset, i.e. what is returned by running name/0 from a subset_config
-type subset_name() :: atom().
-type storage_config() :: borges_spec_behaviour:storage_config().
-type subset_map() ::
    map().  % Format is #{ subset_name => borges_spec_behaviour:subset_config()},
-type spec() ::
    #{name => atom(),
      main_storage := borges_spec_behaviour:storage_config(),
      handler_pid => undefined | pid(),
      subsets => subset_map(),
      module => spec_module_name()}.

-spec make_internal_specification_structure(spec_module_name()) -> spec().
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

-spec create(spec_name(), spec()) -> ok.
create(Name, Spec) ->
    PTKey = {?MODULE, Name},
    undefined = persistent_term:get(PTKey, undefined),
    ok = persistent_term:put(PTKey, Spec).

-spec get_spec(spec_name()) -> spec().
get_spec(Name) ->
    PTKey = {?MODULE, Name},
    persistent_term:get(PTKey).

-spec get_subset_spec(spec_name(), subset_name()) ->
                         borges_spec_behaviour:subset_config().
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

-spec get_subsets(spec_name()) -> subset_map().
get_subsets(Name) ->
    #{subsets := Subsets} = get_spec(Name),
    Subsets.

-spec get_storage_config(spec_name()) -> storage_config().
get_storage_config(Name) ->
    #{main_storage := StorageConfig} = get_spec(Name),
    StorageConfig.

-spec get_subset_storage_config(spec_name(), subset_name()) -> storage_config().
get_subset_storage_config(Name, SubsetName) ->
    #{storage_config := SubsetConfig} = get_subset_spec(Name, SubsetName),
    SubsetConfig.

-spec get_handler_pid(spec_name()) -> pid() | undefined.
get_handler_pid(Name) ->
    #{handler_pid := Handler} = get_spec(Name),
    Handler.

-spec set_handler_pid(spec_name(), pid() | undefined) -> ok.
set_handler_pid(Name, Pid) ->
    PTKey = {?MODULE, Name},
    Spec0 = persistent_term:get(PTKey),
    Spec1 = Spec0#{handler_pid => Pid},
    ok = persistent_term:put(PTKey, Spec1).

-spec get_key_fun(spec_name()) -> term(). % TODO
get_key_fun(Name) ->
    #{key_fun := KeyFun} = get_storage_config(Name),
    KeyFun.

-spec get_key_fun(spec_name(), subset_name()) -> term(). % TODO
get_key_fun(Name, SubsetName) ->
    #{key_fun := KeyFun} = get_subset_storage_config(Name, SubsetName),
    KeyFun.

-spec get_identifier_fun(spec_name()) -> term(). % TODO
get_identifier_fun(Name) ->
    #{module := Module} = get_spec(Name),
    fun Module:storage_identifier/1.
