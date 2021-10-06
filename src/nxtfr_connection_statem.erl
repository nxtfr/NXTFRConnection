-module(nxtfr_connection_statem).

-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-record(data, {callback_module, transport_module, socket}).

%% Transport module is either ssl or gen_tcp
start_link(CallbackModule, TransportModule, Socket) ->
    gen_statem:start_link(?MODULE, [CallbackModule, TransportModule, Socket], []).

init([CallbackModule, TransportModule, Socket]) ->
    Data = #data{
        callback_module = CallbackModule,
        transport_module = TransportModule,
        socket=Socket},
    {ok, connected, Data}.

callback_mode() ->
    handle_event_function.

handle_event(info, {ssl, Socket, Packet}, State, #data{callback_module = CallbackModule, transport_module = TransportModule} = Data) ->
    TransportModule:setopts(Socket, [{active, once}]),
    case apply(CallbackModule, State, [Packet, Data]) of
        {next_state, NextState, NewData} ->
            {next_state, NextState, NewData};
        {next_state, NextState, noreply, NewData} ->
            {next_state, NextState, NewData};
        {next_state, NextState, Reply, NewData} ->
            send_reply(Socket, Reply, TransportModule),
            {next_state, NextState, NewData}
    end;

handle_event(_, Event, State, Data) ->
    error_logger:info_report({?MODULE, Event, State}),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

send_reply(Socket, Reply, TransportModule) ->
    TransportModule:send(Socket, Reply).