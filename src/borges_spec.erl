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
