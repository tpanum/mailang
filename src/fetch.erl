-module(fetch).
-compile([export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% pass(Lines) when is_list(Lines) ->
%     {ok, Chunks} = find_delimiter_in_lines(Lines).

find_delimiter_in_lines([],_,Chunks) ->
    {ok, lists:reverse(Chunks)};

find_delimiter_in_lines([H|T],ProcessedLines, Chunks) ->
    case find_delimiter(H) of
        false -> find_delimiter_in_lines(T,[H|ProcessedLines], Chunks);
        X -> case length(ProcessedLines) of
                 0 -> find_delimiter_in_lines(T,[],Chunks);
                 _ -> find_delimiter_in_lines(T,[],[lists:reverse(ProcessedLines)|Chunks])
             end
    end;
find_delimiter_in_lines(_,_,_) ->
    false.

find_delimiter([C1, C2|T]) when [C1,C2] == "--" ->
    T;
find_delimiter(_) ->
    false.

find_mime_info([]) ->
    false;

find_mime_info([H|T]) ->
    case re:run(H, "Content-Type: ([a-zA-Z/]+); charset=\"([a-zA-Z0-9\-]+)", []) of
        nomatch -> find_mime_info(T);
        {match,[_,{X,Y},{V,Z}]} -> Type=string:substr(H,X+1,Y),
                                   Encoding=string:substr(H,V+1,Z),
                                   [_,_|T2] = T,
                                   {ok,type_to_atom(Type),encoding_to_atom(Encoding),T2}
    
    end.

type_to_atom(X)->
    case X of
        "text/html" -> html;
        "text/plain" -> plain
    end.

encoding_to_atom(X) ->
    case X of
        "utf-8" -> utf8
    end.

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

pass(Lines) ->
    pass_lines(Lines, []).
pass_lines([H|T],Messages) ->
    case pass_header(H) of
        {uid, UID} -> case pass(T, {[{uid, UID}],[]}) of
                          {mail,Rest,Mail} -> pass_lines(Rest, [Mail|Messages]);
                          _ -> Messages
                      end;
        _  -> Messages
    end.

pass([],_)->
    false;
pass([H|T],{Header,Content}) ->
    case pass_header(H) of
            {delimiter, D} -> pass(T,{Header,Content},D,{[],[]},contentinfo);
            {K,V} -> pass(T,{[{K,V}|Header],Content});
            false -> pass(T,{Header,Content})
    end.

pass([],_,_,_,_) ->
    erlang:error(missing_content);
pass([H|T],{Header,Content},Delimiter,{ContentInfo,[]},contentinfo) -> 
    io:format("Passing as Contentinfo: \"~p\"~n",[H]),
    case pass_content_header(H) of
        {attachment, Filename} -> File=pass_attachment(T, Delimiter), io:format("Passing as Content: \"~p\"~n",[H]), pass(T, {Header, Content, File}, Delimiter, {ContentInfo,[]}, content);
        end_content_header -> pass(T, {Header,Content,[]},Delimiter,{ContentInfo,[]},content);
        {K, V} -> pass(T, {Header,Content}, Delimiter,{[{K,V}|ContentInfo],[]},contentinfo);
        L when is_list(L) -> pass(T, {Header,Content}, Delimiter,{L++ContentInfo,[]},contentinfo);
        false -> pass(T, {Header,Content}, Delimiter, {ContentInfo,[]},contentinfo)
    end;
pass([H|T],{Header,Content,Attachments},Delimiter,{ContentInfo,CurrentContent},content) ->
        io:format("Passing as Content: \"~p\"~n",[H]),
    case pass_content(H,Delimiter) of
        end_message -> {mail,T,{Header,[{ContentInfo,list_to_binary(lists:reverse(CurrentContent))}|Content],Attachments}};
        new_format -> pass(T,{Header,[{ContentInfo,lists:reverse(CurrentContent)}|Content]},Delimiter,{[],[]},contentinfo);
        {data,Line} -> pass(T,{Header,Content,Attachments},Delimiter,{ContentInfo,[Line|CurrentContent]},content);
        {attachment, AttachmentDelimiter} -> Attachment=pass_attachment(T, AttachmentDelimiter), {mail, T, {Header,Content}, Attachment}                                             
    end.
            


pass_header("* "++T) ->
    {match,[{_,_},{X,Y}]} = re:run(T,"([0-9a-zA-Z]+) FETCH",[]),
    UID = string:substr(T, X+1, Y),
    {uid, UID};
pass_header("Return-Path: "++T) ->
    {returnpath, T};
pass_header("Delivered-To: "++T) ->
    {delievered, T};
pass_header("From: "++T) ->
    {from, T};
pass_header("Date: "++T) ->
    {date, T};
pass_header("Subject: "++T) ->
    {subject, T};
pass_header("--"++T) ->
    io:format("Delimiter HEADER --> ~p ~n", [T]),
    {delimiter,T};
pass_header(_) ->
    false.

pass_content_header("Content-Type: "++T) ->
    case re:run(T,"([a-zA-Z0-9\/]+); charset=\"([A-Za-z0-9\-]+)",[]) of
        {match,[_,{X,Y},{Z,V}]} ->
            Contenttype=string:substr(T, X+1, Y),
            Charset=string:substr(T, Z+1, V),
            [{contenttype, type_to_atom(Contenttype)},{charset,encoding_to_atom(Charset)}];
        nomatch -> false
    end;
pass_content_header("Content-D"++_) ->
    end_content_header;
pass_content_header(X) ->
    false.

pass_attachment(Lines, Delimiter) ->
    pass_attachment(Lines, Delimiter, {[],[]}).

pass_attachment([],_,{AHeader,AContent}) ->
    RAContent=lists:append(lists:reverse(AContent)),
    DecodedContent=base64:decode(RAContent),
    {AHeader, DecodedContent};
pass_attachment([H|T], Delimiter, {AHeader, AContent}) ->
    case pass_attachment_line(H,Delimiter) of
        {data, D} -> pass_attachment(T, Delimiter,{AHeader,[D|AContent]});
        header -> pass_attachment(T, Delimiter,{AHeader, AContent});
        end_attachment -> pass_attachment([], Delimiter, {AHeader,AContent});
        {K, V} -> pass_attachment(T, Delimiter,{[{K,V}|AHeader],AContent})
    end.
    
pass_attachment_line("--"++T,Delimiter) ->
    End = Delimiter++"--",
    case T of
        _ -> end_attachment
     end;
pass_attachment_line("Content-Disposition: attachment; filename=\""++T, _) ->
    {match,[_,{X,Y}]}=re:run(T,"(.+)\"", []),
    Filename=string:substr(T, X+1, Y),
    {filename, Filename};
pass_attachment_line("Content"++_,_)->
    header;
pass_attachment_line(Line, _) ->
    {data, Line}.


pass_content("--"++T,Delimiter) ->
    End = Delimiter++"--",
    io:format("Delimiter Content --> ~p ~n", [T]),
    case T of
        End -> end_message;
        Delimiter -> new_format;
        NewDelimiter -> {attachment, NewDelimiter}
     end;
pass_content(T, _) ->
    {data,decode_line(T)}.
    
search_prefix(String, Prefix) ->
	PrefixLength = lists:flatlength(Prefix),
	StringLength = lists:flatlength(String),
	RevString = lists:reverse(String),
	RevPreString = lists:nthtail(StringLength-PrefixLength, RevString),
	RevPreString == lists:reverse(Prefix).

-ifdef(TEST).

find_delimiter_in_lines_test() ->
    Data=["--hello", "something", "--hello", "something2", "--hello"],
    ?assertEqual({ok, [["something"],["something2"]]}, find_delimiter_in_lines(Data,[],[])).

find_mime_info_test() ->
    Data=["Content-Type: text/html; charset=\"utf-8\"","Content-Transfer-Encoding: quoted-printable","Content-Disposition: inline","Hello"],
    ?assertEqual({ok,html,utf8,["Hello"]},find_mime_info(Data)).

% decode_test() ->
%     Data=["color: rgb(153, 153, 153); text-decoration: none; =22>Computer Science =E2=","=80=94 Aalborg University</a></div><div><br></div></div>"],
%     ?assertEqual(something,decode(Data)).

pass_test() ->
    Data=["* 1 FETCH (BODY[] {2573}",
          "From: Thomas Kobber Panum <tpanum08@student.aau.dk>",
          "Subject: Mail 1",
          "--5182a9be_327b23c6_9f",
          "Content-Type: text/plain; charset=\"utf-8\"",
          "Content-Transfer-Encoding: quoted-printable",
          "Content-Disposition: inline",
          "1 =20",
          "--5182a9be_327b23c6_9f",
          "Content-Type: text/html; charset=\"utf-8\"",
          "Content-Transfer-Encoding: quoted-printable",
          "Content-Disposition: inline",
          "<div>1</div>",
          "--5182a9be_327b23c6_9f--",
         "* 2 FETCH (BODY[] {2573}",
          "From: Thomas Kobber Panum <tpanum08@student.aau.dk>",
          "Subject: Mail 1",
          "--5182a9be_327b23c6_9f",
          "Content-Type: text/plain; charset=\"utf-8\"",
          "Content-Transfer-Encoding: quoted-printable",
          "Content-Disposition: inline",
          "2 =20",
          "--5182a9be_327b23c6_9f",
          "Content-Type: text/html; charset=\"utf-8\"",
          "Content-Transfer-Encoding: quoted-printable",
          "Content-Disposition: inline",
          "<div>2</div>",
          "--5182a9be_327b23c6_9f--"],
    Output=pass(Data),
    io:format("~p~n",[Output]),
    ?assert(Output).

-endif.
