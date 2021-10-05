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

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5},

    NxtfrConnection = #{
        id => nxtfr_connection,
        start => {
            nxtfr_connection,
            start_link,
            [[
                {callback_module, nxtfr_dev_connection},
                {transport_module, gen_tcp},
                %{dhfile, "priv/dh2048.pem"},
                %{cacertfile, "priv/rootca.crt"},
                %{certfile, "priv/user.crt"},
                %{keyfile, "priv/user.key"},
                {packet, 2},
                {port, 2000},
                {reuseaddr, true}
                ]]
        }},

    NxtfrConnectionStateMSup = #{
        id => nxtfr_connection_statem_sup,
        start => {nxtfr_connection_statem_sup, start_link, []},
        type => supervisor},

    ChildSpecs = [NxtfrConnection, NxtfrConnectionStateMSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
