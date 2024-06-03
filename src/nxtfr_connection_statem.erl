-module(nxtfr_connection_statem).
-author("christian@flodihn.se").
-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-record(state_data, {callback_module, transport_module, socket, fsm_state_data, socket_listener_pid}).

%% Transport module is either ssl or gen_tcp
start_link(CallbackModule, TransportModule, Socket) ->
    gen_statem:start_link(?MODULE, [CallbackModule, TransportModule, Socket], []).

init([CallbackModule, TransportModule, Socket]) ->
    FsmStateData = CallbackModule:init(Socket, TransportModule),
    Self = self(),
    SocketListenerPid = spawn_link(fun() -> nxtfr_connection:wait_for_packets(Self, Socket, TransportModule) end),
    StateData = #state_data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket = Socket,
        fsm_state_data = FsmStateData,
        socket_listener_pid = SocketListenerPid},
    error_logger:info_msg("Connection established for client ~p (~p).", [Socket, TransportModule]),
    {ok, connected, StateData}.

callback_mode() ->
    handle_event_function.

handle_event(info, {ssl, _Socket, closed}, State, StateData) ->
    apply_callback(State, {socket, closed}, StateData);

handle_event(info, {ssl, _Socket, Packet}, State, StateData) ->
    apply_callback(State, {socket, Packet}, StateData);

handle_event(info, {tcp, _Socket, closed}, State, StateData) ->
    apply_callback(State, {socket, closed}, StateData);

handle_event(info, {tcp, _Socket, Packet}, State, StateData) ->
    apply_callback(State, {socket, Packet}, StateData);

handle_event(info, Event, State, StateData) ->
    apply_callback(State, {info, Event}, StateData).

terminate(_Reason, _State, _State) ->
    ok.

code_change(_Vsn, State, StateData, _Extra) ->
    {ok, State, StateData}.

apply_callback(State, Event, #state_data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket = Socket,
        fsm_state_data = FsmStateData,
        socket_listener_pid = SocketListenerPid} = StateData) ->
    case apply(CallbackModule, State, [Event, FsmStateData]) of
        {next_state, NextState, NewFsmStateData} ->
            {next_state, NextState, StateData#state_data{fsm_state_data = NewFsmStateData}};
        {next_state, NextState, noreply, NewFsmStateData} ->
            {next_state, NextState, StateData#state_data{fsm_state_data = NewFsmStateData}};
        {next_state, NextState, Reply, NewFsmStateData} ->
            nxtfr_connection:send_to_client(Socket, Reply, TransportModule),
            {next_state, NextState, StateData#state_data{fsm_state_data = NewFsmStateData}};
        {stop, Reason} ->
            exit(SocketListenerPid, normal),
            exit(Reason)
    end.