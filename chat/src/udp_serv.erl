-module(udp_serv).

-behaviour(gen_server).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {socket}).

-compile([{parse_transform, lager_transform}]).

start_link(Port) ->
    gen_server:start_link(?MODULE, Port, []).



init(Port) ->
    lager:info("UDP server starts"),
    {ok, Socket} = gen_udp:open(Port),
    {ok, #state{socket = Socket}}.


handle_call(_, _, S) ->
    {noreply, S}.

handle_cast(_, S) ->
    {noreply, S}.


handle_info({udp, Socket, Ip, Port, Data}, #state{socket = Socket} = S) ->
    lager:info("Ip: ~p port: ~p sends multimedia", [Ip, Port]),
    broadcast(Socket, Data, {Ip, Port}),
    {noreply, S}.


terminate(app_shutdown, #state{socket = Socket}) ->
    gen_tcp:close(Socket).


broadcast(OwnSocket, Data, MyAddr) ->
    AllUsers = sockserv_sup:get_all_users(),
    AllButMe = lists:filter(fun({_Pid, Addr}) ->
                               Addr =/= MyAddr end, AllUsers),
    lists:foreach(fun({_Pid, Addr}) ->
                          send(OwnSocket, Addr, Data) end,
                  AllButMe).

send(OwnSocket, {Ip, Port}, Data) ->
    gen_udp:send(OwnSocket, Ip, Port, Data).
