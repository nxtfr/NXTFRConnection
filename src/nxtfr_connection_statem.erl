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
    {ok, connection, Data}.

callback_mode() ->
    handle_event_function.

handle_event(_, {ssl, Socket, Packet}, State, #data{callback_module = CallbackModule} = Data) ->
    error_logger:info_report({?MODULE, Packet, State, CallbackModule}),
    {next_state, State, Data};

handle_event(_, Event, State, Data) ->
    error_logger:info_report({?MODULE, Event, State}),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.