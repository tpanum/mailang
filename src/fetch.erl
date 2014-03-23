-module(fetch).
-export([pass/1]).

-record(mail,{header, contents, attachments}).

pass(MimeData) ->
    {_,_,RawHeader,_,Body}=MimeData,
    PassedHeader=pass_header(RawHeader),
    TextContent=find_by_mime_type(<<"text">>, Body),
    Attachments=find_by_mime_parameter({<<"disposition">>,<<"attachment">>}, Body),
    PassedAttachments=pass_attachments(Attachments),
    PassedContent=pass_texts(TextContent),
    #mail{header=PassedHeader, contents=PassedContent, attachments=PassedAttachments}.

pass_header(RawHeader) ->
    lists:foldl(fun(I, Acc) ->
                       case pass_header_info(I) of
                           KV={_,_} -> [KV|Acc];
                           _ -> Acc
                       end
              end, [], RawHeader).

pass_header_info({<<"Return-Path">>, V}) ->
    {<<"return_path">>, V};
pass_header_info({<<"To">>, V}) ->
    {<<"to">>, V};
pass_header_info({<<"From">>, V}) ->
    {<<"from">>, V};
pass_header_info({<<"Subject">>, V}) ->
    {<<"subject">>, V};
pass_header_info(_) ->
    false.


pass_attachments(ListofAttachments) ->
    lists:map(fun(A) ->
                      pass_attachment(A)
              end, ListofAttachments).

pass_attachment({_,_,_,Params,Body})->
    {_, DisParams}=lists:keyfind(<<"disposition-params">>, 1, Params),
    {_, Filename}=lists:keyfind(<<"filename">>, 1, DisParams),
    {[{filename,Filename}], Body}.

pass_texts(ListofTexts)->
    lists:map(fun(A) ->
                      pass_text(A)
              end, ListofTexts).

pass_text({_,Type,_,Params,Body}) ->
    {_, ConParams}=lists:keyfind(<<"content-type-params">>, 1, Params),
    {_, Encoding}=lists:keyfind(<<"charset">>, 1, ConParams),
    {Type,[{<<"encoding">>, Encoding}], Body}.


texttype(Type) ->
    case Type of
        <<"plain">> ->
            plain;
        <<"html">> ->
            html
    end.

find_by_mime_type(Type, MimeData) when is_list(MimeData) ->
    lists:foldl(fun(I, Acc) ->
                    {I_Type,_,_,_,I_Content}=I,
                        case I_Type of
                            Type -> lists:append([find_by_mime_type(Type, I_Content), [I], Acc]);
                            _ -> lists:append([find_by_mime_type(Type, I_Content), Acc])
                        end
                end, [], MimeData);
find_by_mime_type(_, _) ->
    [].

find_by_mime_parameter(Parameter, MimeData) when is_list(MimeData) ->
    lists:foldl(fun(I, Acc) ->
                    {_,_,_,I_Params,I_Content}=I,
                        case lists:member(Parameter,I_Params) of
                            true -> lists:append([find_by_mime_parameter(Parameter, I_Content), [I], Acc]);
                            _ -> lists:append([find_by_mime_parameter(Parameter, I_Content), Acc])
                        end
                end, [], MimeData);
find_by_mime_parameter(_, _) ->
    [].

decode_contents(Contents) ->
    lists:map(fun({Info, RawContent}) ->
                      {Info, rfc2047:decode(RawContent)}
              end, Contents).
