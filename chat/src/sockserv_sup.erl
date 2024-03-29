-module(sockserv_sup).
-behaviour(supervisor).

-export([start_link/2, start_socket/0]).
-export([init/1]).
-export([login/2, logout/1, get_all_users/0]).
% debug functions
-export([get_children_states/0]).
-export([stop/0]).


-record(client, {pid, addr}).

start_link(Port, AcceptorsNumber) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptorsNumber]).

stop() ->
    lists:foreach(fun({_, Pid, _, _}) ->
                          gen_server:stop(Pid, app_shutdown, 2000) end,
                 supervisor:which_children(?MODULE)).

init([Port, AcceptorsNumber]) ->
    prepare_ets(),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, {packet, line}]),
    spawn_link(fun() -> empty_listeners(AcceptorsNumber) end),
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,
            {sockserv_serv, start_link, [ListenSocket]},
            temporary, 1000, worker, [sockserv_serv]}
          ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners(Amount) ->
    [start_socket() || _ <- lists:seq(1, Amount)],
    ok.

prepare_ets() ->
    ets:new(sessions, [public, set, named_table]).

get_all_users() ->
    lists:map(fun({_Name, #client{pid = Pid, addr = Addr}}) ->
                      {Pid, Addr} end,
              ets:tab2list(sessions)).


login(Name, Addr) ->
    case ets:lookup(sessions, Name) of
        [] ->
            ets:insert(sessions, {Name, #client{pid = self(), addr = Addr}}),
            ok;
        [{Name, _}] ->
            {error, already_exists}
    end.

logout(Name) ->
    ets:delete(sessions, Name).


get_children_states() ->
    lists:map(fun({_, Pid, _, _}) ->
                      try gen_server:call(Pid, get_info, 500)
                      catch _:_ ->
                              {Pid, not_responding}
                      end
              end,
              supervisor:which_children(?MODULE)).
