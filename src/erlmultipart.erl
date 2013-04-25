-module(erlmultipart).

-version(0.1).

-export([new/3,file_handler/3]).
-include_lib("eunit/include/eunit.hrl").

new(Boundary, FileHandler, MaxSize) when is_binary(Boundary) ->
    NextBoundary = <<"\r\n--", Boundary/binary>>,
    FirstBoundary = <<"--", Boundary/binary, "\r\n">>,
    Parser = 
    fun 
        % autogrow when the buffer is too small
        (P, S, Buffer, Result) when S /= next, byte_size(Buffer) < byte_size(Boundary) + 6 -> 
            grow(P,S,Buffer,Result, MaxSize);
            
        % need 4 bytes to figure out the end
        (P, next, Buffer, Result) when byte_size(Buffer) < 4 ->
            grow(P,next,Buffer,Result, MaxSize);
        
        % just matched a boundary, got end character sequence - return result
        (P, State = next, Buffer = <<"--\r\n">>, Result) ->
            result(P,State,Buffer,Result);
        
        % no end sequence, parse next headers
        (P, next, Buffer, Result) ->
            parse(P,headers,Buffer,Result);
        
        % skip until first boundary
        (P, initial, Buffer, Result) ->
            
            case binary:split(Buffer,FirstBoundary) of
                [_] ->
                    skip(P, initial, Buffer, Result, FirstBoundary);
                [_,Rest] ->
                    parse(P, headers, Rest, Result)
            end;
        
        % look for the end of the header block, if not found grow the header buffer
        (P, headers, Buffer, Result) ->
            
            case binary:split(Buffer,<<"\r\n\r\n">>) of
                [_] ->
                    grow(P,headers,Buffer,Result, MaxSize);
                    
                [HeaderBuffer,Rest] ->
                    case parse_headers(HeaderBuffer) of
                        
                        {ok, Headers} ->
                           
                            % parse Content Disposition Header
                            CD = proplists:get_value(<<"Content-Disposition">>, Headers, <<"undefined">>),
                            case binary:split(CD, <<";">>,[trim, global]) of        
                                
                                [<<"form-data">>| KVStrings] ->
                                    
                                    KVs = parse_kv(KVStrings),
                                    NameVal = proplists:get_value(<<"name">>, KVs),
                                    FilenameVal = proplists:get_value(<<"filename">>, KVs),
                                    
                                    case {NameVal, FilenameVal} of
                                        {undefined, _} -> 
                                            perror(<<"Invalid content disposition:">>, CD);
                                            
                                        {Name, undefined} ->
                                            Result1 = [{type, field}, {name, Name}, {value, undefined}],
                                            parse(P, field, Rest, [Result1|Result]);
                                            
                                        {Name, Filename} ->
                                            ContentType = proplists:get_value(<<"Content-Type">>, Headers),
                                            Result1 = [{type, file}, {name, Name}, {content_type, ContentType}, {original_filename, Filename}],
                                            Result2 = FileHandler(start, <<>>, Result1),
                                            parse(P, file, Rest, [Result2|Result])
                                    end;
                                    
                                _ -> 
                                    perror(<<"Invalid content disposition:">>, CD)
                            end;
                        Error ->
                            Error
                    end
            end;
        
        % parse a field - look for the next boundary, otherwise grow
        (P, field, Buffer, R=[Result|Results]) ->
            case binary:split(Buffer,NextBoundary) of
                [_] ->
                    grow(P,field,Buffer,R,MaxSize);
                [Value,Rest] ->
                    %% form field complete
                    Result1 = orddict:store(value, Value, Result),
                    parse(P,next,Rest,[Result1|Results])
            end;
        
        % parse a file
        (P, file, Buffer, [Result|Results]) ->
            
            case binary:split(Buffer,NextBoundary) of
                [_] ->
                    {Data, Rest} = consume(Buffer, NextBoundary),
                    Result1 = FileHandler(data, Data, Result),
                    grow(P,file,Rest,[Result1|Results],MaxSize);
                [Data,Rest] ->
                    Result1 = FileHandler(data, Data, Result),
                    Result2 = FileHandler(done, <<>>, Result1),
                    parse(P,next,Rest,[Result2|Results])
            end
    end,
    
    fun (Buffer) ->
        Parser(Parser, initial, Buffer, [])
    end.

% Parser functions

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
    perror(<<"Maximum buffer size exceeded">>).

% parser error
perror(Reason) ->
    {error, Reason}.

perror(Reason, S) ->
    {error, <<Reason/binary, " ", S/binary>>}.

% Utilities

ltrim(<<>>) -> <<>>;
ltrim(<<" ",Rest/binary>>) -> ltrim(Rest);
ltrim(<<"\t",Rest/binary>>) -> ltrim(Rest);
ltrim(Other) when is_binary(Other) -> Other;
ltrim(L) when is_list(L) -> lists:map(fun(B) -> ltrim(B) end, L).

parse_headers(Buffer) ->
    HeadersBuffer = binary:split(Buffer, <<"\r\n">>, [global]),
    HeadersList = lists:map(fun(H) -> ltrim(binary:split(H, <<":">>, [trim])) end, HeadersBuffer),
    Result = lists:mapfoldl(
        fun
            ([K,V], ok) -> 
                {{K, V}, ok};
            ([H], ok) -> 
                {{error}, perror(<<"Invalid Header:">>, H)} ;
            (_, Error) -> 
                {{error}, Error}
        end, ok, HeadersList),
    case Result of
        {R, ok} -> {ok, R};
        {_, Error} -> Error
    end.

parse_kv(KV) when is_binary(KV) ->
    case ltrim(binary:split(KV, <<"=">>)) of
        [K,QV] when byte_size(K) >= 1, byte_size(QV) >= 3 ->
            Sz = byte_size(QV),
            case {binary:at(QV,0), binary:part(QV,{1,Sz-2}), binary:at(QV,Sz-1)} of
                {$", V, $"} -> {K,V};
                _ -> perror(<<"Invalid key value pair:">>, KV)
            end;
        _ ->
            perror(<<"Invalid key value pair:">>, KV)
    end;

parse_kv(L) when is_list(L) ->
    lists:map(fun(KV) -> parse_kv(KV) end, L).

% file handling

tmp_filename() ->
    {A,B,C} = now(),
    N = node(),
    "/tmp/" ++ lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])).
    
file_handler(start, _, Result) ->
    Filename = tmp_filename(),
    {ok, File} = file:open(Filename, [write]),
    Result1 = orddict:store(filename, Filename, Result),
    Result2 = orddict:store(file, File, Result1),
    Result2;
    
file_handler(data, Buffer, Result) ->
    File = orddict:fetch(file, Result),
    ok = file:write(File, Buffer),
    Result;

file_handler(done, _, Result) ->
    File = orddict:fetch(file, Result),
    ok = file:close(File),
    orddict:erase(file, Result).
    