%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(PORT, 4005).
-define(ACCEPTOR_NUMBER, 5).

-compile([{parse_transform, lager_transform}]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    lager:info("Server starts at port ~p with ~p acceptors", [?PORT, ?ACCEPTOR_NUMBER]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    sockserv_sup:stop().

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SockServ = #{
      id => sockserv_sup,
      start => {sockserv_sup, start_link, [?PORT, ?ACCEPTOR_NUMBER]},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [sockserv_sup]
     },
    UdpServ = #{
      id => udp_serv,
      start => {udp_serv, start_link, [?PORT]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [udp_serv]
     },
    {ok, { {one_for_all, 0, 1}, [SockServ, UdpServ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
