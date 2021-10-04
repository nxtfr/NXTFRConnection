%%%-------------------------------------------------------------------
%% @doc nxtfr_connection public API
%% @end
%%%-------------------------------------------------------------------

-module(nxtfr_connection_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nxtfr_connection_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
