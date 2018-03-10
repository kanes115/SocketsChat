-module(sockserv_serv).
-behaviour(gen_server).

-record(state, {socket, state, name = none, addr}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket, state = wait_for_conn}}.

handle_call(get_info, _From, State) ->
    {reply, State, State};
handle_call(_, _, S) ->
    {noreply, S}.


handle_cast({pass_msg, From, Msg}, #state{state = logged_in, socket = Socket} = State) ->
    send(Socket, From, Msg, []),
    {noreply, State};
handle_cast(accept, S = #state{socket = ListenSocket, state = wait_for_conn}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    sockserv_sup:start_socket(),
    %send(AcceptSocket, "Tell your name:", []),
    {noreply, S#state{socket=AcceptSocket, state = wait_for_config}}.


handle_info({tcp, Socket, Data}, #state{state = wait_for_config} = S) ->
    Config = line(Data),
    {Name, UdpAddr} = parse_config(Config),
    case sockserv_sup:login(Name, UdpAddr) of
        ok ->
            io:format("~p logged in!~n", [Name]),
            send(Socket, "You are logged in!", []),
            {noreply, S#state{state = logged_in, name = Name}};
        {error, already_exists} ->
            io:format("~p already exists~n", [Name]),
            gen_tcp:close(Socket),
            {stop, normal, stopped}
    end;
handle_info({tcp, _Socket, Str}, #state{state = logged_in, name = Name} = S) ->
    Msg = line(Str),
    broadcast(Msg, Name),
    {noreply, S};
handle_info({tcp_closed, Socket}, #state{state = logged_in, name = Name}) ->
    sockserv_sup:logout(Name),
    gen_tcp:close(Socket),
    io:format("Session for ~p closed~n", [Name]),
    {stop, normal, stopped};
handle_info({tcp_closed, Socket}, _State) ->
    gen_tcp:close(Socket),
    io:format("Socket closed without logging in"),
    {stop, normal, stopped}.


% TODO close UDP sockects here as well
terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket);
terminate(_, stopped) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_config(Config) ->
    case string:tokens(Config, ":") of
        [Name, Ip, Port] ->
            {PortInt, []} = string:to_integer(Port),
            {ok, IpTuple} = inet:parse_address(Ip),
            {Name, {IpTuple, PortInt}};
        _ ->
            error
    end.

line("\r\n") ->
    "";
line(Str) ->
    hd(string:tokens(Str, "\r\n")).


send(Socket, Name, Str, Args) ->
    send(Socket, Name ++ ": " ++ Str, Args).

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok.

broadcast(Msg, MyName) ->
    AllUsers = sockserv_sup:get_all_users(),
    AllButMe = lists:keydelete(self(), 1, AllUsers),
    lists:foreach(fun({Pid, _Addr}) ->
                          gen_server:cast(Pid, {pass_msg, MyName, Msg}) end,
                 AllButMe).
