-module(nxtfr_connection_statem).
-author("christian@flodihn.se").
-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-record(data, {callback_module, transport_module, socket, connection_data}).

-define(HEADER_BYTE_SIZE, 2).
-define(HEADER_BIT_SIZE, 16).

%% Transport module is either ssl or gen_tcp
start_link(CallbackModule, TransportModule, Socket) ->
    gen_statem:start_link(?MODULE, [CallbackModule, TransportModule, Socket], []).

init([CallbackModule, TransportModule, Socket]) ->
    Data = #data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket = Socket,
        connection_data = undefined},
    error_logger:info_msg("Connection established for client ~p.", [Socket]),
    GenStatemPid = self(),
    spawn_link(fun() -> wait_for_packets(GenStatemPid, Socket, TransportModule) end),
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
            send_reply(Socket, Reply, TransportModule),
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
            send_reply(Socket, Reply, TransportModule),
            {next_state, NextState, Data#data{connection_data = NewConnectionData}}
    end;

handle_event(info, Event, State, Data) ->
    error_logger:info_msg("Unknown event: ~p.", [Event]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

send_reply(Socket, Reply, TransportModule) ->
    MessageSize = byte_size(Reply),
    Header = <<MessageSize:?HEADER_BIT_SIZE/integer-unsigned-little>>,
    TransportModule:send(Socket, Header),
    TransportModule:send(Socket, Reply).

wait_for_packets(ConnectionPid, Socket, TransportModule) ->
    case wait_for_bytes(?HEADER_BYTE_SIZE, Socket, TransportModule) of
        {ok, <<MessageSize:16/integer-unsigned-little>>} ->
            % Receive bytes equal to the size specified in the header
            case wait_for_bytes(MessageSize, Socket, TransportModule) of
                {ok, Data} ->
                    ConnectionPid ! {tcp, Socket, Data},
                    wait_for_packets(ConnectionPid, Socket, TransportModule);
                {error, closed} -> pass
            end;
        {error, closed} -> pass
    end.

wait_for_bytes(NumBytes, Socket, TransportModule) ->
    case TransportModule:recv(Socket, NumBytes) of
        {ok, Data} -> 
            {ok, Data};
        {error, enotconn} ->
             error_logger:info_msg("Client socket ~p disconnected", [Socket]),
             {error, tcp_closed};
        {error, closed} -> 
             error_logger:info_msg("Client socket ~p disconnected", [Socket]),
            {error, closed};
        {error, einval} ->
            error_logger:error_report("Socket in active mode not supported by transport module ~p.", [TransportModule])
    end.
