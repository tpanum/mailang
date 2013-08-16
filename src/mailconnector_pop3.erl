-module(mailconnector_pop3).

-behaviour(gen_server).

-export([start_link/1, stop/0, is_frontier/1, join_crlf/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

start_link(Host) ->
    gen_server:start_link({global, Host}, ?MODULE, [Host, 110], []).

stop() ->
	gen_server:call(?MODULE, stop).

init([Host, Port]) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]),
    listener(Sock, 1),
    {ok, #state{conn=Sock}}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call({login, Username, Password}, _From, State) ->
    {ok, _}=action(State#state.conn, user, Username),
    {ok, _}=action(State#state.conn, pass, Password),
    {reply, ok, State};

handle_call(fetch_all, _From, State) ->
    {ok, Amount, _}=action(State#state.conn, stat),
    List=action(State#state.conn, multifetch, Amount),
    {reply, List, State};

handle_call(_Command, _From, _State) ->
    {reply, not_valid, _State}.

handle_cast(_Command, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    action(State#state.conn, quit),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

action(Conn, quit) ->
    send(Conn, "QUIT");
action(Conn, list) ->
    Cmd="LIST",
    send(Conn, Cmd),
    listener(Conn, unknown);
action(Conn, stat) ->
    Cmd="STAT",
    send(Conn, Cmd),
    {ok, Output}=listener(Conn, 1),
    [UnwrappedOutput]=Output,
    SplittedOutput=binary:split(UnwrappedOutput,<<" ">>, [global]),
    [_,HighestID,TotalSize|_]=SplittedOutput,
    {ok, list_to_integer(binary_to_list(HighestID)), TotalSize}.



action(Conn, fetch, Target) when is_integer(Target) ->
    action(Conn, fetch, integer_to_list(Target));
action(Conn, fetch, Target) ->
    Cmd=string:join(["RETR", Target], " "),
    send(Conn, Cmd),
    {ok, Output}=listener(Conn, unknown),
    fetch:pass(mimemail:decode(Output));
action(Conn, multifetch, ID) ->
    action(Conn, multifetch, ID, []);
action(Conn, user, Username) ->
    Cmd=string:join(["USER", Username], " "),
    send(Conn, Cmd),
    listener(Conn, 1);
action(Conn, pass, Password) ->
    Cmd=string:join(["PASS", Password], " "),
    send(Conn, Cmd),
    listener(Conn, 1).


action(Conn, multifetch, 0, FetchedMails) ->
    FetchedMails;
action(Conn, multifetch, ID, FetchedMails) ->
    FetchedMail=action(Conn, fetch, ID),
    action(Conn, multifetch, ID-1, [FetchedMail|FetchedMails]).

send(Conn, Cmd) ->
    gen_tcp:send(Conn,list_to_binary(string:concat(Cmd,"\r\n"))).

listener(Sock, unknown, Data) ->
    receive
        {tcp, _Sock, Reply} ->
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
        {tcp, _Sock, Reply} ->
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
                    
