-module(nxtfr_connection_statem).
-author("christian@flodihn.se").
-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-record(data, {callback_module, transport_module, socket, connection_data}).

%% Transport module is either ssl or gen_tcp
start_link(CallbackModule, TransportModule, Socket) ->
    gen_statem:start_link(?MODULE, [CallbackModule, TransportModule, Socket], []).

init([CallbackModule, TransportModule, Socket]) ->
    Data = #data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket = Socket,
        connection_data = undefined},
    TransportModule:setopts(Socket, [{active, once}]),
    error_logger:info_msg("Connection established for client ~p.", [Socket]),
    {ok, connected, Data}.

callback_mode() ->
    handle_event_function.

handle_event(info, {ssl, Socket, Packet}, State, #data{
        callback_module = CallbackModule, transport_module = TransportModule, connection_data = ConnectionData} = Data) ->
    error_logger:info_msg("TCP: ~p ~p.", [Packet, State]),
    TransportModule:setopts(Socket, [{active, once}]),
    case apply(CallbackModule, State, [Packet, ConnectionData]) of
        {next_state, NextState, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, noreply, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, Reply, NewConnectionData} ->
            send_reply(Socket, Reply, TransportModule),
            {next_state, NextState, Data#data{connection_data = NewConnectionData}}
    end;

handle_event(Info, Event, State, Data) ->
    error_logger:info_report({unknown_event, Info, Event, State, Data}),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

send_reply(Socket, Reply, TransportModule) ->
    TransportModule:send(Socket, Reply).