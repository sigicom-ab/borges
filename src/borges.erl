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

register(ModelSpecModule) ->
    ok = borges_spec:validate(ModelSpecModule),
    Name = ModelSpecModule:name(),
    Spec = borges_spec:make_internal_specification_structure(ModelSpecModule),
    borges_spec:create(Name, Spec),
    maybe_init(ModelSpecModule),
    ok = borges_handler_sup:maybe_start_handler(Name),
    ok.

%% if you change name and hot reload this would break badly
unregister(StorageSpec) ->
    Name = StorageSpec:name(),
    ok = borges_handler_sup:maybe_stop_handler(Name),
    persistent_term:erase({?MODULE, Name}),
    ok.

get(Name, Id) -> borges_adapter:get(Name, Id).

get_subset(Name, SubsetName) ->
    % get data belonging to subset
    get_subset(Name, SubsetName, []).

get_subset(Name, SubsetName, Input) -> borges_adapter:get_subset(Name, SubsetName, Input).

store(Name, Term) ->
    borges_handler:handle_event(Name, Term),
    IdentifierFun = borges_spec:get_identifier_fun(Name),
    Input = IdentifierFun(Term),
    borges_adapter:store(Name, Input, Term).

put(Name, Term) -> store(Name, Term).

post(Name, Term) ->
    TermWithId = add_id(Name, Term),
    store(Name, TermWithId).

delete(Name, Id) ->
    case borges_adapter:get(Name, Id) of
        {ok, Obj} ->
            borges_adapter:remove(Name, Id),
            borges_handler:remove(Name, Obj);
        _ ->
            logger:info("Attempt to remove non-existing object with id ~p. Doing nothing", [Id])
    end.

add_id(Name, Term) -> ok.

maybe_init(StorageSpec) ->
    case erlang:function_exported(StorageSpec, init, 0) of
        true -> StorageSpec:init();
        false -> ok
    end.
