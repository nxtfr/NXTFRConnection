-module(nxtfr_dev_connection).

-export([connected/1]).

connected(Packet, State, Data) ->
    error_logger:info_report({?MODULE, Packet, State, Data}),
    {next_state, State, Data};