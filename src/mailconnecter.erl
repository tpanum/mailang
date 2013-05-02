-module(mailconnecter).

-behaviour(gen_server).

-export([start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Host, imap) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, 143], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([Host, Port]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    {ok, {Sock, 0}}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call([login, Username, Password], _From, State) ->
	{NewState, Resp} = action(State, string:join(["LOGIN", Username, Password], " ")),
	[Status|_RestofResp] = string:tokens(Resp, " "),
	case Status of
		"OK" -> Result = ok;
		_ -> Result = false
	end,
    {reply, Result, NewState};

handle_call(list, _From, State) ->
	{NewState, Resp} = action(State, "LIST \"\" \"*\""),
    {reply, Resp, NewState};

handle_call(_Command, _From, _State) ->
    {reply, not_valid, _State}.

handle_cast({From, MessageTuple}, State) ->
    _Pid = spawn(fun() -> external_task:start(From, MessageTuple) end),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




action(_State = {Socket, Counter}, Command) ->
	NewCount = Counter+1,
	CounterAsList = lists:flatten(io_lib:format("~p ", [NewCount])),
	Message = list_to_binary(lists:concat([CounterAsList, Command, "\r\n"])),
	io:format("~p~n", [Message]),
	gen_tcp:send(Socket, Message),
	{{Socket, NewCount}, listener(Socket, NewCount)}.



listener(_Sock, Count) ->
    receive
	{_, _, Reply} ->
		Messages = string:tokens(binary_to_list(Reply), "\r\n"),
		find_message(Messages, Count)
    after 5000 ->
	    timeout
    end.

process_message(Message, Count) ->
	StringCount = lists:flatten(io_lib:format("~p", [Count])),
	case [MCount|PureMessage] = string:tokens(Message, " ") of
		M when StringCount == MCount ->
			{ok, string:join(PureMessage, " ")};
		_ -> false
	end.

find_message([], _) ->
 false;    

find_message([H|T], Count) ->
	case process_message(H, Count) of
		{ok, Message} -> Message;
		_ -> find_message(T, Count)
	end.
