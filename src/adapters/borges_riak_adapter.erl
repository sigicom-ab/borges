-module(borges_riak_adapter).

-export([store/3,
         fetch/2,
         remove/2]).

-behaviour(borges_adapter).

-type config() ::
    #{storage_adapter_config := storage_adapter_config(),
      _ => _}.
-type storage_adapter_config() ::
    #{bucket := binary(),
      host := iolist(),
      port => integer()}.
-type key() :: binary().
-type data() :: term().

-spec store(key(), data(), config()) -> ok | {error, term()}.
store(RiakKey, Data, #{storage_adapter_config := Config}) ->
    BucketType = maps:get(bucket_type, Config, "default"),
    Bucket = maps:get(bucket, Config),
    Path = get_riak_path(Config),

    GetRes = get_request(Path, BucketType, Bucket, RiakKey),
    case handle_get_res_for_store(GetRes) of
        {ok, ExistingHeaders} ->
            Headers = make_headers(Data, Config, ExistingHeaders),
            Res = put_request(Path, BucketType, Bucket, RiakKey, Data, Headers),
            case handle_result(Res) of
                {ok, undefined} -> ok;
                _ -> {error, {unexpected_response, Res}}
            end;
        {error, _} = Err -> Err
    end.

fetch(Key, #{storage_adapter_config := Config}) ->
    Path = get_riak_path(Config),
    BucketType = maps:get(bucket_type, Config, "default"),
    Bucket = maps:get(bucket, Config),
    RiakKey = Key,
    Res = get_request(Path, BucketType, Bucket, RiakKey),
    handle_result(Res).

remove(Key, #{storage_adapter_config := Config}) ->
    Path = get_riak_path(Config),
    BucketType = maps:get(bucket_type, Config, "default"),
    Bucket = maps:get(bucket, Config),
    RiakKey = Key,

    %% TODO: we need to test this with toombstone and siblings, in the doc it looks like it's
    %%       valid to skip vclock in request, but I'm hesitant.
    Res = delete_request(Path, BucketType, Bucket, RiakKey),
    case handle_result(Res) of
        {ok, undefined} -> ok;
        {ok, not_found} = Err -> Err;
        _ -> {error, {unexpected_response, Res}}
    end.

get_riak_path(Config) ->
    Host = maps:get(host, Config),
    Port = maps:get(port, Config, 8098),  % Default http
    [Host, ":", Port].

get_request(Host, BucketType, Bucket, Key) ->
    Url = build_url(Host, BucketType, Bucket, Key),
    httpc:request(Url).

put_request(Host, BucketType, Bucket, Key, Data, Headers) ->
    Url = build_url(Host, BucketType, Bucket, Key),
    RawData = term_to_binary(Data),
    Req = {Url, Headers, "application/x-erlang-term", RawData},
    httpc:request(put, Req, [], []).

delete_request(Host, BucketType, Bucket, Key) ->
    Url = build_url(Host, BucketType, Bucket, Key),
    httpc:request(delete, {Url, []}, [], []).

build_url(Host, BucketType, Bucket, Key) ->
    [Host, "/types/", BucketType, "/buckets/", Bucket, "/keys/", Key].

handle_get_res_for_store({ok, {{_Version, 404, _StatusText}, _, _}}) -> {ok, []};
handle_get_res_for_store({ok, {{_Version, 200, _StatusText}, Headers, _}}) ->
    VClock = proplists:get_value("x-riak-vclock", Headers),
    {ok, [{"x-riak-vclock", VClock}]};
%% TODO: we probably don't want to be this hard in just removing other possible headers
%%       such as user-meta. We need to think about if we can / should trust the storage
%%       adapter to always do the right thing
%% this is a mock if we want to make something that keeps the headers that are already set
%% {ok, remove_standard_headers(Headers)};
handle_get_res_for_store(_) -> {error, cant_parse_response}.

%% remove_standard_headers(Headers) ->
    %% lists:filter(fun not_std_header/1, Headers).

%% not_std_header({"date", _}) -> false;
%% not_std_header({"server", _}) -> false;

handle_result({ok, {{_Version, Status, _StatusText}, Headers, RawData}})
    when Status < 300 ->
    CT = proplists:get_value("content-type", Headers),
    decode_raw_data(RawData, CT);
handle_result({ok, {{_Version, Status, _StatusText}, _Headers, _RawData}})
    when Status == 404 ->
    {ok, not_found};
handle_result(R) ->
    io:format("jso: ~p~n", [R]),
    {error, cant_handle_the_pressure}.

decode_raw_data([], _ContentType) -> {ok, undefined};
decode_raw_data(Data, ContentType) when is_list(Data) ->
    decode_raw_data(list_to_binary(Data), ContentType);
decode_raw_data(RawData, "application/x-erlang-term") -> {ok, binary_to_term(RawData)};
decode_raw_data(_RawData, CT) ->
    io:format("cant parse ~p~n", [CT]),
    {error, cant_parse_content_type}.

make_headers(_Data, _Config, ExistingHeaders) -> ExistingHeaders.
