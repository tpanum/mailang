-module(mailconnector_pop3).

-behaviour(gen_server).

-define(CON_OPTIONS, [binary, {packet, 0}, {active, true}]).

-export([start_link/3, stop/0, is_frontier/1, join_crlf/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn, type}).


start_link(Host,Port,ConnectionType) ->
    gen_server:start_link({global, Host}, ?MODULE, [Host, Port, ConnectionType], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([Host, Port, ConnectionType]) ->
    {ok, Sock} = connect_by_type(Host, Port, ConnectionType),
    listener(Sock, 1),
    {ok, #state{conn=Sock, type=ConnectionType}}.

connect_by_type(Host, Port, ssl) ->
    ssl:start(),
    ssl:connect(Host, Port, ?CON_OPTIONS);
connect_by_type(Host, Port, tcp) ->
    gen_tcp:connect(Host, Port, ?CON_OPTIONS).

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({login, Username, Password}, _From, State) ->
    {ok, _}=action(State, user, Username),
    {ok, _}=action(State, pass, Password),
    {reply, ok, State};

handle_call(fetch_all, _From, State) ->
    {ok, Amount, _}=action(State, stat),
    List=action(State, multifetch, Amount),
    {reply, List, State};

handle_call(_Command, _From, _State) ->
    {reply, not_valid, _State}.

handle_cast(_Command, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    action(State, quit),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

action(State, quit) ->
    send(State, "QUIT");
action(State, list) ->
    Cmd="LIST",
    send(State, Cmd),
    listener(State#state.conn, unknown);
action(State, stat) ->
    Cmd="STAT",
    send(State, Cmd),
    {ok, Output}=listener(State#state.conn, 1),
    [UnwrappedOutput]=Output,
    SplittedOutput=binary:split(UnwrappedOutput,<<" ">>, [global]),
    [_,HighestID,TotalSize|_]=SplittedOutput,
    {ok, list_to_integer(binary_to_list(HighestID)), TotalSize}.



action(State, fetch, Target) when is_integer(Target) ->
    action(State, fetch, integer_to_list(Target));
action(State, fetch, Target) ->
    Cmd=string:join(["RETR", Target], " "),
    send(State, Cmd),
    {ok, Output}=listener(State, unknown),
    fetch:pass(mimemail:decode(Output));
action(State, multifetch, ID) ->
    action(State, multifetch, ID, []);
action(State, user, Username) ->
    Cmd=string:join(["USER", Username], " "),
    send(State, Cmd),
    listener(State#state.conn, 1);
action(State, pass, Password) ->
    Cmd=string:join(["PASS", Password], " "),
    send(State, Cmd),
    listener(State#state.conn, 1).


action(_, multifetch, 0, FetchedMails) ->
    FetchedMails;
action(State, multifetch, ID, FetchedMails) ->
    FetchedMail=action(State, fetch, ID),
    action(State, multifetch, ID-1, [FetchedMail|FetchedMails]).

send(State, Cmd) ->
    case State#state.type of
        ssl -> ssl:send(State#state.conn,list_to_binary(string:concat(Cmd,"\r\n")));
        tcp -> gen_tcp:send(State#state.conn,list_to_binary(string:concat(Cmd,"\r\n")))
    end.

listener(Sock, unknown, Data) ->
    receive
        {_, _Sock, Reply} ->
            SplittedResponse=binary:split(Reply, <<"\r\n">>, [global]),
            ParsedSplittedResponse=case Data of
                                       [] -> [_|X]=SplittedResponse, X;
                                       _ -> SplittedResponse
                                   end,
            [_|RemovedBlank]=lists:reverse(ParsedSplittedResponse),
            [EndMark|ReversedReply]=RemovedBlank,
            CompactReply=lists:reverse(ReversedReply),
            case EndMark of
                <<".">> -> {ok, list_to_binary([Data,join_crlf(CompactReply)])};
                    _ -> listener(Sock, unknown, list_to_binary([Data,join_crlf(ParsedSplittedResponse)]))
            end
    after 5000 -> 
            timeout
    end.

listener(Sock, unknown) ->
    listener(Sock, unknown, []);
listener(Sock, ResponseLength) when is_integer(ResponseLength) ->
    receive
        {_, _Sock, Reply} ->
            SplittedResponse=binary:split(Reply, <<"\r\n">>),
            ActualResponse=lists:reverse(lists:nthtail(ResponseLength, lists:reverse(SplittedResponse))),
            case lists:flatlength(SplittedResponse)-lists:flatlength(ActualResponse) of
                X when X > 1 -> erlang:error(unexpected_resp_length);
                X -> ok
            end,
            case binary:first(lists:last(ActualResponse)) of
                43 -> {ok, ActualResponse}; % 43 == "+"
                45 -> {error, ActualResponse} % 45 == "-"
            end
            
    after 5000 ->
          timeout
    end.

join_crlf(ListofBin)->
    List=lists:map(fun(A) ->
                                  [<<"\r\n">>,A]
                          end, ListofBin),
    [_|JoinedBinList]=lists:append(List),
    list_to_binary(JoinedBinList).
   

fold_content(Data) when is_list(Data) ->
     lists:foldl(fun(I, {Status, A}) ->
                         case I of
                             [] -> 
                                 case Status of
                                     ok -> {[[<<"">>]], A};
                                     _ -> {[[<<"">>]|Status], A}
                                 end;
                             [H|_] -> case is_frontier(H) of
                                          {true,_,_}->
                                              case Status of
                                                  ok -> {ok, [I|A]};
                                                  Status -> {ok, [I,join_lists(lists:reverse(Status))|A]}
                                               end;
                                          false -> 
                                              case Status of
                                                  ok ->
                                                      {[I], A};
                                                  _ ->
                                                      {[I|Status], A}
                                              end
                                      end
                         end
                 end,
                 {ok, []},
                 Data).
is_frontier(Binary) when is_binary(Binary) ->
    is_frontier(binary_to_list(Binary));
is_frontier(String) ->
    case string:substr(String,1,2) of
        "--" -> case string:substr(String, string:len(String)-1,2) of
                    "--" -> {true, closing, string:substr(String, 3, string:len(String)-4)};
                    _ -> {true, opening, string:substr(String, 3)}
                end;
        _ -> false
    end.

join_lists(ListofLists) ->
    [_|Result]=lists:foldl(fun(I, A) ->
                        lists:append([A, [<<"">>], I])
                end,
                [],
                ListofLists),
    Result.
                    
