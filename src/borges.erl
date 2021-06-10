-module(borges).

-export([register/1,
         unregister/1,
         get/2,
         get_subset/2,
         get_subset/3,
         store/2,
         put/2,
         post/2,
         delete/2,
         add_id/2]).

% Name of module containing spec
-type spec_module_name() :: atom().
% Name of spec, i.e. what is returned by running name/0 in the spec module
-type spec_name() :: atom().
% Name of a subset, i.e. what is returned by running name/0 from a subset_config
-type subset_name() :: atom().
% Object identifier
-type ident() :: borges_spec_behaviour:ident().
-type main_obj_data() :: borges_spec_behaviour:main_obj_data().

-spec register(spec_module_name()) -> ok.
register(ModelSpecModule) ->
    ok = borges_spec:validate(ModelSpecModule),
    Name = ModelSpecModule:name(),
    Spec = borges_spec:make_internal_specification_structure(ModelSpecModule),
    borges_spec:create(Name, Spec),
    maybe_init(ModelSpecModule),
    ok = borges_handler_sup:maybe_start_handler(Name),
    ok.

%% Note: If you change name and hot-reload this would break badly
-spec unregister(spec_module_name()) -> ok.
unregister(StorageSpec) ->
    Name = StorageSpec:name(),
    ok = borges_handler_sup:maybe_stop_handler(Name),
    persistent_term:erase({?MODULE, Name}),
    ok.

-spec get(spec_name(), ident()) -> {ok, main_obj_data()} | {ok, []}.
get(Name, Id) -> borges_adapter:get(Name, Id).

get_subset(Name, SubsetName) ->
    % get data belonging to subset
    get_subset(Name, SubsetName, []).

-spec get_subset(spec_name(), subset_name(), ident()) -> {ok, main_obj_data()} | {ok, []}.
get_subset(Name, SubsetName, Id) -> borges_adapter:get_subset(Name, SubsetName, Id).

-spec store(spec_name(), main_obj_data()) -> ok.
store(Name, Term) ->
    borges_handler:handle_event(Name, Term),
    IdentifierFun = borges_spec:get_identifier_fun(Name),
    Input = IdentifierFun(Term),
    borges_adapter:store(Name, Input, Term).

-spec put(spec_name(), main_obj_data()) -> ok.
put(Name, Term) -> store(Name, Term).

-spec post(spec_name(), main_obj_data()) -> ok.
post(Name, Term) ->
    TermWithId = add_id(Name, Term),
    store(Name, TermWithId).

delete(Name, Term) -> ok.

add_id(Name, Term) -> ok.

maybe_init(StorageSpec) ->
    case erlang:function_exported(StorageSpec, init, 0) of
        true -> StorageSpec:init();
        false -> ok
    end.
