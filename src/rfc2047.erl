-module(rfc2047).
-export([decode/1]).

decode(Lines) ->
    decode(Lines,[]).

decode([], Acc) ->
    [_|ListofBin]=lists:map(fun(A) ->
                      [<<"\r\n">>, A]
              end, lists:reverse(Acc)),
   list_to_binary(lists:append(ListofBin));
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
  
