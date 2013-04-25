-module(erlmultipart).

-version(0.1).

-export([new/3,file_handler/4]).
-include_lib("eunit/include/eunit.hrl").

new(Boundary, FileHandler, MaxSize) when is_binary(Boundary) ->
    NextBoundary = <<"\r\n--", Boundary/binary>>,
    FirstBoundary = <<"--", Boundary/binary, "\r\n">>,
    Parser = 
    fun 
        (P, next, Buffer, Result) when byte_size(Buffer) < 4 ->
            grow(P,next,Buffer,Result, MaxSize);
            
        (P, State = next, Buffer = <<"--\r\n">>, Result) ->
            result(P,State,Buffer,Result);
            
        (P, next, Buffer, Result) ->
            parse(P,headers,Buffer,Result);
            
        (P, S, Buffer, Result) when byte_size(Buffer) < byte_size(Boundary) + 6 -> 
            % autogrow when Buffer is too small
            grow(P,S,Buffer,Result, MaxSize);
        
        (P, initial, Buffer, Result) ->
            % skip until first boundary
            case binary:split(Buffer,FirstBoundary) of
                [_] ->
                    skip(P, initial, Buffer, Result, FirstBoundary);
                [_,Rest] ->
                    parse(P, headers, Rest, Result)
            end;
            
        (P, headers, Buffer, Result) ->
            % look for \r\n\r\n - if not found request more to parse the whole header

            case binary:split(Buffer,<<"\r\n\r\n">>) of
                [_] ->
                    grow(P,headers,Buffer,Result, MaxSize);
                [HeaderBuffer,Rest] ->
                    case parse_headers(HeaderBuffer) of
                        {ok, Headers} ->
                            % parse Content Disposition Header
                            CD = proplists:get_value(<<"Content-Disposition">>, Headers, <<"undefined">>),
                            CDParsed = binary:split(CD, <<";">>,[trim, global]),
                            case CDParsed of        
                                [<<"form-data">>| KVStrings] ->
                                    KVs = parse_kv(KVStrings),
                                    NameVal = proplists:get_value(<<"name">>, KVs),
                                    FilenameVal = proplists:get_value(<<"filename">>, KVs),
                                    case {NameVal, FilenameVal} of
                                        {undefined, _} -> 
                                            {error, <<"Invalid content disposition: ", CD/binary>>};
                                        {Name, undefined} ->
                                            Result1 = {field, Name, undefined},
                                            parse(P, field, Rest, [Result1|Result]);
                                        {Name, _Filename} ->
                                            ContentType = proplists:get_value(<<"Content-Type">>, Headers),
                                            State = FileHandler(start, <<>>, {Name, ContentType}, undefined),
                                            Result1 = {file, Name, ContentType, State},
                                            parse(P, file, Rest, [Result1|Result])
                                    end;
                                _ -> 
                                    {error, <<"Invalid content disposition: ", CD/binary>>}
                            end;
                        Error ->
                            Error
                    end
            end;
        
        (P, field, Buffer, Result=[{field,Name,_}|Results]) ->
            % look for the next boundary, otherwise grow
            case binary:split(Buffer,NextBoundary) of
                [_] ->
                    grow(P,field,Buffer,Result,MaxSize);
                [Value,Rest] ->
                    %% form data done - either complete or next section
                    parse(P,next,Rest,[{field,Name,Value}|Results])
            end;
        
        (P, file, Buffer, [{file,Filename,ContentType,State1}|Results]) ->
            % look for the next boundary, otherwise grow
            case binary:split(Buffer,NextBoundary) of
                [_] ->
                    {Data, Rest} = consume(Buffer, NextBoundary),
                    State2 = FileHandler(data, Data, {Filename, ContentType}, State1),
                    Result = {file,Filename,ContentType,State2},
                    grow(P,file,Rest,[Result|Results],MaxSize);
                [Data,Rest] ->
                    State2 = FileHandler(data, Data, {Filename, ContentType}, State1),
                    State3 = FileHandler(done, <<>>, {Filename, ContentType}, State2),
                    Result = {file,Filename,ContentType,State3},
                    parse(P,next,Rest,[Result|Results])
            end
    end,
    
    fun (Buffer) ->
        Parser(Parser, initial, Buffer, [])
    end.

% consume the buffer looking for token
consume(Buffer, Token) ->
    { binary:part(Buffer, 0, byte_size(Buffer) - byte_size(Token)), 
      binary:part(Buffer, byte_size(Buffer), - (byte_size(Token)-1)) }.

% skip the buffer looking for token
skip(P, S, Buffer, Result, Token) ->
    {_, Rest} = consume(Buffer, Token),
    more(P, S, Rest, Result, 0).

% grow the buffer, watch max size
grow(P, S, Buffer, Result, MaxSize) ->
    more(P, S, Buffer, Result, MaxSize).
    
% continue parsing
parse(P, S, Buffer, Result) ->
    P(P, S, Buffer, Result).

% done
result(_P, _S, _Buffer, Result) ->
    {ok, lists:reverse(Result)}.
    
% request more data
more(P, S, Buffer1, Result, MaxSize) when MaxSize > 0, byte_size(Buffer1) =< MaxSize ->
    {more, fun (Buffer2) -> P(P, S, <<Buffer1/binary, Buffer2/binary>>, Result) end };

more(_P, _S, _Buffer1, _Result, _MaxSize) ->
    {error, <<"Maximum buffer size exceeded">> }.

% ltrim
trim(<<>>) -> <<>>;
trim(<<" ",Rest/binary>>) -> trim(Rest);
trim(<<"\t",Rest/binary>>) -> trim(Rest);
trim(Other) when is_binary(Other) -> Other;
trim(L) when is_list(L) -> lists:map(fun(B) -> trim(B) end, L).

parse_headers(Buffer) ->
    HeadersBuffer = binary:split(Buffer, <<"\r\n">>, [global]),
    HeadersList = lists:map(fun(H) -> trim(binary:split(H, <<":">>, [trim])) end, HeadersBuffer),
    Result = lists:mapfoldl(
        fun
            ([K,V], ok) -> 
                {{K, V}, ok};
            ([H], ok) -> 
                {{error}, {error, <<"Invalid Header: ", H/binary>>}} ;
            (_, Error) -> 
                {{error}, Error}
        end, ok, HeadersList),
    case Result of
        {R, ok} -> {ok, R};
        {_, Error} -> Error
    end.

parse_kv(KV) when is_binary(KV) ->
    case trim(binary:split(KV, <<"=">>)) of
        [K,QV] when byte_size(K) >= 1, byte_size(QV) >= 3 ->
            Sz = byte_size(QV),
            case {binary:at(QV,0), binary:part(QV,{1,Sz-2}), binary:at(QV,Sz-1)} of
                {$", V, $"} -> {K,V};
                _ -> {error, <<"Invalid key value pair: ", KV>>}
            end;
        _ ->
            {error, <<"Invalid key value pair: ", KV>>}
    end;

parse_kv(L) when is_list(L) ->
    lists:map(fun(KV) -> parse_kv(KV) end, L).

tmp_filename() ->
    {A,B,C} = now(),
    N = node(),
    "/tmp/" ++ lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])).
    
file_handler(start, _Buffer, _Info, _State) ->
    Tmp = tmp_filename(),
    {ok, File} = file:open(Tmp, [write]),
    {Tmp,File};
    
file_handler(data, Buffer, _Info, {Tmp,File}) ->
    ok = file:write(File, Buffer),
    {Tmp,File};

file_handler(done, _Buffer, _Info, {Tmp,File}) ->
    ok = file:close(File),
    Tmp.