%%%-------------------------------------------------------------------
%% @doc nxtfr_connection top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nxtfr_connection_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5},

    NxtfrConnection = #{
        id => nxtfr_connection,
        start => {nxtfr_connection, start_link, []
        }},

    NxtfrConnectionStateMSup = #{
        id => nxtfr_connection_statem_sup,
        start => {nxtfr_connection_statem_sup, start_link, []},
        type => supervisor},

    ChildSpecs = [NxtfrConnection, NxtfrConnectionStateMSup],
    {ok, {SupFlags, ChildSpecs}}.
