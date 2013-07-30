-module(mailconnector).

-behaviour(gen_server).

-export([start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, search_prefix/2]).

start_link(Host, imap) ->
    gen_server:start_link({Host, local}, ?MODULE, [Host, 143], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([Host, Port]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]),
    {ok, Sock}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({login, Username, Password}, _From, State) ->
	Output = action(State, string:join(["LOGIN", Username, Password], " ")),
	case Output of
		{ok, Response, Data} -> Result = {Response, Data};
		_ -> Result = false
	end,
    {reply, Result, State};

handle_call(list, _From, State) ->
    Resp = action(State, "LIST \"\" \"*\""),
    {reply, Resp, State};

handle_call({select, MailBox}, _From, State) ->
	Output = action(State, string:join(["SELECT", MailBox], " ")),
	case Output of
		{ok, Response, Data} -> Result = {Response, Data};
		_ -> Result = false
	end,
    {reply, Result, State};

handle_call({examine, MailBox}, _From, State) ->
	Output = action(State, string:join(["EXAMINE", MailBox], " ")),
	case Output of
		{ok, Response, Data} -> Result = {Response, Data};
		_ -> Result = false
	end,
    {reply, Result, State};


handle_call({fetch, From, To}, _From, State) ->
    {reply, multiFetch(From, To, State), State};

handle_call({fetch, Num}, _From, State) when is_integer(Num) ->
    handle_call({fetch, integer_to_list(Num)}, _From, State);

handle_call({fetch, Num}, _From, State) ->
	FetchContent = "BODY[]",
	Output = action(State, string:join(["FETCH", Num, FetchContent], " ")),
	case Output of
		{ok, _, ["* NO"++_]} -> Result = false;
		{ok, Count, Data} -> Result=refactored_fetch:pass(Data)
	end,
    {reply, Result,State};
handle_call(_Command, _From, _State) ->
    {reply, not_valid, _State}.

handle_cast(_Command, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

multiFetch(From, To, State) when is_list(From) ->
    multiFetch(list_to_integer(From), To, State);
multiFetch(From, To, State) ->
    multiFetch(From, To, State, []).

multiFetch(From, To, State, Mails) when is_list(From) ->
    multiFetch(string:to_integer(From), To, State, Mails);
multiFetch(From, To="*", State, Mails) ->
    Result=handle_call({fetch, From}, false, State),
    case Result of
           {_,false,_} -> case Mails of
                              [] -> false;
                              _ -> lists:reverse(Mails)
                          end;
           {_, [Mail],NewState} -> multiFetch(From+1, To, NewState, [Mail|Mails])
    end.

get_content_fetch(Count, FetchContent, Data) ->
	[FirstElement|Tail] = Data,
	Prefix = string:join(["*", Count, "FETCH", "("++FetchContent], " "),
	io:format("~p~n~p~n", [Prefix, FirstElement]),
	case search_prefix(FirstElement, Prefix) of
		false -> false;
		_ ->  [LastElement|RevPureData] = lists:reverse(Tail), [LastChar|RemainingChars] = lists:reverse(LastElement),
			case LastChar of
				41 -> lists:reverse(RevPureData	);
				_ -> false
			end
	end.


action(_State = Socket, Command) ->
        Identifier=uuid:uuid_to_string(uuid:get_v4()),
	Message = list_to_binary(lists:concat([Identifier, " ", Command, "\r\n"])),
%	io:format("~p~n", [Message]),
	gen_tcp:send(Socket, Message),
	listener(Socket, Identifier).

fetch(_State = Socket) ->
        Identifier=uuid:uuid_to_string(uuid:get_v4()),
	Message = list_to_binary(lists:concat([Identifier, "BODY[2]", "\r\n"])),
	io:format("~p~n", [Message]),
	gen_tcp:send(Socket, Message),
	listener(Socket, Identifier).

listener(Sock, Identifier) ->
    listener(Sock,Identifier, []).

listener(Sock, Identifier, PreData) ->
    receive
	{tcp, _Socket, Reply} ->
		case analyze_packet(Reply, Identifier, PreData) of
			{ok, Message, Data} -> {ok, Identifier, Data};
			{uncomplete, Data} -> listener(Sock, Identifier, Data)
		end
    after 10000 ->
	    timeout
    end.

analyze_packet(Packet, Identifier, OldData) when is_binary(Packet) ->
	Messages = string:tokens(binary_to_list(Packet), "\r\n"),
	case process_messages(Messages, Identifier, OldData) of
		{data, Data} -> {uncomplete, Data};
		{ok, Message, Data} -> {ok, Message, Data}
	end.

process_messages(Messages, Identifier) ->
	process_messages(Messages, Identifier, []).

process_messages([], _, []) ->
 false;  

process_messages([], Identifier, Data) ->
	{data, Data};   

process_messages([H|T], Identifier, Data) ->
	case process_message(H, Identifier) of
		{ok, Message} -> {ok, Message, lists:reverse(Data)};
		{data, Output} -> process_messages(T, Identifier, [Output|Data])
	end.

process_message(Message, Identifier) ->
    case search_prefix(Message, Identifier) of
        true -> {ok, Message};
        false -> {data, Message}
    end.

search_prefix(String, Prefix) ->
	PrefixLength = lists:flatlength(Prefix),
	StringLength = lists:flatlength(String),
    DeltaLength = StringLength-PrefixLength,
    case DeltaLength of
        N when N > 0 -> RevString = lists:reverse(String), RevPreString = lists:nthtail(DeltaLength, RevString), RevPreString == lists:reverse(Prefix);
        _ -> false
    end.

% find_message(Messages, Count) ->
% 	find_message(Messages, Count, []).

% find_message([], _, _) ->
%  false;    

% find_message([H|T], Count, Data) ->
% 	case process_message(H, Count) of
% 		{ok, Message} -> {ok, Message, lists:reverse(Data)};
% 		{data, Output} -> find_message(T, Count, [Output|Data])
% 	end.
