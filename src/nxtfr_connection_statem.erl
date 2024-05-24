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
    ConnectionData = CallbackModule:init(Socket, TransportModule),
    Data = #data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket = Socket,
        connection_data = ConnectionData},
    error_logger:info_msg("Connection established for client ~p.", [Socket]),
    GenStatemPid = self(),
    spawn_link(fun() -> nxtfr_connection:wait_for_packets(GenStatemPid, Socket, TransportModule) end),
    {ok, connected, Data}.

callback_mode() ->
    handle_event_function.

handle_event(info, {ssl, Socket, Packet}, State, #data{
        callback_module = CallbackModule, transport_module = TransportModule, connection_data = ConnectionData} = Data) ->
    case apply(CallbackModule, State, [Packet, ConnectionData]) of
        {next_state, NextState, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, noreply, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, Reply, NewConnectionData} ->
            nxtfr_connection:send_to_client(Socket, Reply, TransportModule),
        {next_state, NextState, Data#data{connection_data = NewConnectionData}}
    end;

handle_event(info, {tcp, Socket, Packet}, State, #data{
        callback_module = CallbackModule, transport_module = TransportModule, connection_data = ConnectionData} = Data) ->
    case apply(CallbackModule, State, [Packet, ConnectionData]) of
        {next_state, NextState, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, noreply, NewConnectionData} ->
            {next_state, NextState, Data#data{connection_data = NewConnectionData}};
        {next_state, NextState, Reply, NewConnectionData} ->
            nxtfr_connection:send_to_client(Socket, Reply, TransportModule),
            {next_state, NextState, Data#data{connection_data = NewConnectionData}}
    end;

handle_event(info, Event, State, Data) ->
    error_logger:info_msg("Unknown event: ~p.", [Event]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.