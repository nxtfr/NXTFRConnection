-module(nxtfr_dev_connection).

-define(LOGIN, 1).
-define(REGISTER, 1).
-define(PLAYING, 1).

-export([connected/2]).

connected(Packet, Data) ->
    error_logger:info_report({?MODULE, {packet, Packet}, connected, Data}),
    {next_state, connected, Data}.