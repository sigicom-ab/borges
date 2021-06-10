-module(borges_tests).

-export([]).

-define(SLEEPTIME, 30).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    % Setup and register
    application:ensure_all_started(borges),
    borges:unregister(example_spec),
    borges:register(example_spec),
    Projects = example_data(),
    timer:sleep(?SLEEPTIME),

    % Add data to storage
    [borges:put(project_storage, Project) || Project <- Projects],
    timer:sleep(?SLEEPTIME),

    % Fetch project and check that it exists
    {ok, Project1} = borges:get(project_storage, 1),
    ?assertMatch(#{project_id := 1}, Project1),

    % Fetch company and user subsets and check that they have the right number of objects
    {ok, CompanySubset} = borges:get_subset(project_storage, company_projects, 1),
    {ok, UserSubset} = borges:get_subset(project_storage, user_projects, 2),
    ?assertEqual(length(CompanySubset), 2),
    ?assertEqual(length(UserSubset), 2),

    % Delete project 1
    borges:delete(project_storage, 1),
    timer:sleep(?SLEEPTIME),

    % Check that project 1 is indeed removed
    {ok, Project1Again} = borges:get(project_storage, 1),
    ?assertMatch([], Project1Again),

    % Check that project 1 is removed from user and company subsets
    {ok, User1Subset} = borges:get_subset(project_storage, user_projects, 1),
    ?assertEqual(length(User1Subset), 0),
    {ok, User2Subset} = borges:get_subset(project_storage, user_projects, 2),
    ?assertEqual(length(User2Subset), 1),
    {ok, CompanySubsetAgain} = borges:get_subset(project_storage, company_projects, 1),
    ?assertEqual(length(CompanySubsetAgain), 1).

example_data() ->
    [#{project_id => N,
       user_ids => [N, N + 1],
       company => N rem 2,
       some => <<"some">>,
       other => <<"other">>,
       stuff => <<"stuff">>}
     || N <- lists:seq(1, 3)].
