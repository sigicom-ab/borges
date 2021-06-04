-module(borges_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    borges:unregister(example_spec),
    application:ensure_all_started(borges),
    borges:register(example_spec),
    Projects = example_data(),
    timer:sleep(10),

    [borges:put(project_storage, Project) || Project <- Projects],
    timer:sleep(10),

    {ok, CompanySubset} = borges:get_subset(project_storage, company_projects, 1),
    {ok, UserSubset} = borges:get_subset(project_storage, user_projects, 2),
    {ok, Project1} = borges:get(project_storage, 1),
    ?assertMatch(#{project_id := 1}, Project1),
    ?assertEqual(length(CompanySubset), 2),
    ?assertEqual(length(UserSubset), 2).

example_data() ->
    [#{project_id => N,
       user_ids => [N, N + 1],
       company => N rem 2,
       some => <<"some">>,
       other => <<"other">>,
       stuff => <<"stuff">>}
     || N <- lists:seq(1, 3)].
