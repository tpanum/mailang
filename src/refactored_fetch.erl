 -module(refactored_fetch).
-compile([export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

pass(Lines) when is_list(Lines) ->
    pass_lines(Lines, []).
pass_lines([],Mails) ->
    lists:reverse(Mails);
pass_lines(Lines,Mails) ->
    try
    {mail, RemaindingLines, Mail}=pass_header(Lines),
    pass_lines(RemaindingLines, [Mail|Mails])
    catch
        error:unexpected_end -> Mails
    end.
pass_header(Lines) ->
    pass_header(Lines,[]).

pass_header([],_) ->
    erlang:error(unexpected_end);
pass_header([H|T],Header) ->
    case pass_header_line(H) of
        {delimiter,Delimiter} -> pass_content_info(T, Delimiter, Header, [], []);
        {K,V} -> pass_header(T, [{K,list_to_binary(V)}|Header]);
        false -> pass_header(T,Header)
    end.
pass_content_info([],_,_,_,_) ->
    erlang:error(unexpected_end);
pass_content_info([H|T],Delimiter,Header,ContentInfo, CurrentContents) ->
    io:format("PassContentInfo : ~p~n", [H]),
    case pass_content_info_line(H) of
        Values when is_list(Values) -> pass_content_info(T, Delimiter, Header, Values++ContentInfo, CurrentContents);
        {attachment, Filename} -> {attachments, RemainingLines, FoundAttachments}=pass_attachment(T, Delimiter, [{filename,Filename}|ContentInfo]), {partial, RemainingLines, CurrentContents, FoundAttachments};
        multipart -> {partial, PRemainingLines, PCurrentContents, PAttachment}=pass_header(T,[]), {mail, PRemainingLines, {Header,PCurrentContents++CurrentContents, PAttachment}};
        end_content_info -> pass_content(T,Delimiter,Header,ContentInfo,[],CurrentContents);
        false -> pass_content_info(T,Delimiter, Header, ContentInfo, CurrentContents)
    end.
pass_content([],_,_,_,_,_) ->
    erlang:error(unexpected_end);
pass_content([H|T],Delimiter,Header,ContentInfo,Content, CurrentContents) ->
    io:format("PassContent : ~p~n", [H]),
    case pass_content_line(H, Delimiter) of
        end_message -> {mail,T,{Header,[{ContentInfo,list_to_binary(lists:reverse(Content))}|CurrentContents]}};
        new_format -> pass_content_info(T,Delimiter,Header,[],[{ContentInfo,list_to_binary(lists:reverse(Content))}|CurrentContents]);
        {data, Data} -> pass_content(T, Delimiter, Header, ContentInfo, [Data|Content], CurrentContents)
    end.

% Syntax definitions

pass_header_line("* "++T) ->
    {match,[{_,_},{X,Y}]} = re:run(T,"([0-9a-zA-Z]+) FETCH",[]),
    UID = string:substr(T, X+1, Y),
    {uid, UID};
pass_header_line("Return-Path: "++T) ->
    {returnpath, T};
pass_header_line("Delivered-To: "++T) ->
    {delievered, T};
pass_header_line("From: "++T) ->
    {from, T};
pass_header_line("Date: "++T) ->
    {date, T};
pass_header_line("Subject: "++T) ->
    {subject, T};
pass_header_line("--"++T) ->
    case lists:reverse(T) of
        "--"++_ -> false;
             _ -> {delimiter,T}
    end;
pass_header_line(_) ->
    false.

pass_content_info_line("Content-Disposition: attachment; filename=\""++T) ->
    {match,[_,{X,Y}]}=re:run(T,"(.+)\"", []),
    Filename=string:substr(T, X+1, Y),
    {attachment, Filename};        
pass_content_info_line("Content-Type: multipart/related;"++T) ->
    multipart;
pass_content_info_line("Content-Type: "++T) ->
    case re:run(T,"([a-zA-Z0-9\/]+);") of
        {match,[_,{X,Y}]} ->
            Contenttype=string:substr(T, X+1, Y),
            Result=[{contenttype, type_to_atom(Contenttype)}];
        nomatch -> Result=[]
    end,
    case re:run(T,"charset=\"(.+)\"") of
        {match,[_,{Z,V}]} -> Charset=string:substr(T, Z+1, V),
                             SecondResult=[{charset,encoding_to_atom(Charset)}|Result];
        nomatch -> SecondResult=Result
    end,
    SecondResult;
pass_content_info_line("Content-D"++_) ->
    end_content_info;
pass_content_info_line("Content-ID:"++T) -> 
    {match,[{_,_},{X,Y}]}=re:run(T,"[\s]*<([a-zA-Z0-9\-]+)"),
    ContentID=string:substr(T, X+1, Y), {contentid,list_to_binary(ContentID)};
pass_content_info_line(X) ->
    false.

pass_content_line("--"++T,Delimiter) ->
    End = Delimiter++"--",
    io:format("Delimiter Content --> ~p ~n", [T]),
    case T of
        End -> end_message;
        Delimiter -> new_format;
        NewDelimiter -> {attachment, NewDelimiter}
     end;
pass_content_line(T, _) ->
    {data, decode_line(T)}.

pass_attachment(Lines, Delimiter, FileInfo) ->
    pass_attachment(Lines, Delimiter, FileInfo,[],[]).

pass_attachment([H|T],Delimiter,[],Attachments) ->
    case pass_content_info_line(H) of
        {attachment, FileInfo} -> pass_attachment(T, Delimiter, FileInfo, [], Attachments);
        _ -> pass_attachment(T, Delimiter, [], Attachments)
    end.
            
pass_attachment([H|T], Delimiter, FileInfo, AContent, Attachments) ->
    case pass_attachment_line(H,Delimiter) of
        {data, D} -> pass_attachment(T, Delimiter, FileInfo, [D|AContent], Attachments);
        end_attachment -> RAContent=lists:append(lists:reverse(AContent)), DecodedContent=base64:decode(RAContent), pass_attachment(T, Delimiter, [], [{FileInfo, DecodedContent}|Attachments]);
        end_of_attachments -> RAContent=lists:append(lists:reverse(AContent)), DecodedContent=base64:decode(RAContent), {attachments, T, [{FileInfo, DecodedContent}|Attachments]}
    end.
    
pass_attachment_line("--"++T,Delimiter) ->
    End = Delimiter++"--",
    case T of
        End -> end_of_attachments;
        _ -> end_attachment
     end;
pass_attachment_line(Line, _) ->
    {data, Line}.
type_to_atom(X)->
    case X of
        "text/html" -> html;
        "text/plain" -> plain;
        _ -> list_to_binary(X)
    end.

encoding_to_atom(X) ->
    case X of
        "utf-8" -> utf8
    end.


% RFC IMAP (Quoted-Printable)

decode(Lines) ->
    decode(Lines,[]).

decode([], Acc) ->
   list_to_binary(lists:reverse(Acc));
decode([H|T], Acc) ->
    decode(T, [decode_line(H)|Acc]).

decode_line(Line) ->
    decode_line(Line,[]).
decode_line([],Acc) ->
    erlang:list_to_binary(lists:reverse(Acc));
decode_line([H|T],Acc) when [H]=="=" andalso length(T) > 1 ->
    [H1,H2|T2] = T,
    Hex = [H1, H2],
    C = erlang:list_to_integer(Hex, 16),
    decode_line(T2,[<<C>>|Acc]);
decode_line([H|T],Acc) when [H]=="=" ->
    decode_line(T,Acc);
decode_line([H|T], Acc) ->
    decode_line(T, [H|Acc]).
