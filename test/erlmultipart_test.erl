-module(erlmultipart_test).

-include_lib("eunit/include/eunit.hrl").
-define(SIMPLE_TEST_DATA, <<"--abc\r\nContent-Disposition: form-data; name=\"message\"\r\n\r\nHello\r\n--abc--\r\n">>).
-define(SIMPLE_TEST_BOUNDARY, <<"abc">>).
-define(SAFARI_TEST_BOUNDARY, <<"----WebKitFormBoundaryVXJ8WdsksxSvukhv">>).

chunk_binary(<<>>, _Size, Chunks) ->
    Chunks;
    
chunk_binary(Bin, Size, Chunks) when byte_size(Bin) =< Size ->
    Chunks ++ [Bin];

chunk_binary(Bin, Size, Chunks) ->
    Chunk = binary:part(Bin, {0,Size}),
    Bin2 = binary:part(Bin, {Size, byte_size(Bin) - Size}),
    chunk_binary(Bin2, Size, Chunks ++ [Chunk] ).

verify_simple_result({ok,[[{type,field},{name,<<"message">>},{value,<<"Hello">>}]]}) ->
    ok.

verify_safari_result({ok,[[{type,field},{name,<<"field1">>},{value,<<"Markus">>}],
             [{filename,_},
              {type,file},
              {name,<<"field2">>},
              {content_type,<<"image/jpeg">>},
              {original_filename,<<"testbild-sendepause.jpg">>}],
             [{type,field},{name,<<"submit">>},{value,<<"submit">>}]]}) ->
    ok.

safari_test_data() ->
    {ok, Data} = file:read_file("../test/safari-single-upload.http"), 
    Data.

test_chunked(Bin,Boundary,Size) ->
    Parser = erlmultipart:new(Boundary, fun erlmultipart:file_handler/3, 12345),
    Chunked = chunk_binary(Bin, Size, []),
    lists:foldl(fun(Chunk, {_, P}) -> P(Chunk) end, {more, Parser}, Chunked).

simple_test() ->
    Parser = erlmultipart:new(?SIMPLE_TEST_BOUNDARY, fun erlmultipart:file_handler/3, 12345),
    verify_simple_result(Parser(?SIMPLE_TEST_DATA)).

safari_test() ->
    Parser = erlmultipart:new(?SAFARI_TEST_BOUNDARY, fun erlmultipart:file_handler/3, 12345),
    verify_safari_result(Parser(safari_test_data())).

simple_chunked_1_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 1)).

simple_chunked_2_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 2)).

simple_chunked_3_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 3)).

simple_chunked_4_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 4)).

simple_chunked_5_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 5)).
    
simple_chunked_6_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 6)).

simple_chunked_7_test() ->
    verify_simple_result(test_chunked(?SIMPLE_TEST_DATA, ?SIMPLE_TEST_BOUNDARY, 7)).
    
safari_chunked_3_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 3)).

safari_chunked_4_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 4)).

safari_chunked_5_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 5)).

safari_chunked_10_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 10)).

safari_chunked_20_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 20)).

safari_chunked_30_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 30)).

safari_chunked_100_test() ->
    verify_safari_result(test_chunked(safari_test_data(), ?SAFARI_TEST_BOUNDARY, 100)).