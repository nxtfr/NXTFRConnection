%%%-------------------------------------------------------------------
%% @doc nxtfr_connection top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nxtfr_connection_statem_sup).
-author("christian@flodihn.se").
-behaviour(supervisor).

-export([start/3]).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 1},

    NxtfrConnectionStateM = #{
        id => nxtfr_connection_statem,
        start => {nxtfr_connection_statem, start_link, []},
        restart => transient},

    ChildSpecs = [NxtfrConnectionStateM],
    {ok, {SupFlags, ChildSpecs}}.

%% external functions

start(CallbackModule, TransportModule, Socket) ->
    supervisor:start_child(?MODULE, [CallbackModule, TransportModule, Socket]).