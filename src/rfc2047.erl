-module(rfc2047).
-export([decode/1]).

decode(String) when is_list(String) ->
    decode(String, []).

decode([], Acc) ->
    lists:reverse(Acc);

decode([H|T], Acc) when [H] == "=" andalso length(T) >= 2 ->
    [Hex1, Hex2|RemainingString] = T,
    Hex = [Hex1, Hex2],
    C = erlang:list_to_integer(Hex, 16),
    decode(RemainingString, [C|Acc]);

decode([H|T], Acc) when [H] == "=" ->
    decode(T, Acc); 

decode([H|T], Acc) ->
    decode(T, [H|Acc]).  