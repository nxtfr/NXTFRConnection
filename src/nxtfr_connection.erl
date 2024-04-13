-module(nxtfr_connection).
-author("christian@flodihn.se").
-behaviour(gen_server).

-define(DEFAULT_TCP_OPTS, [
    {nodelay, false},
    {reuseaddr, true}]).

%% External exports
-export([start_link/0]).

%% Internal exports
-export([acceptor/3]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2
    ]).

% server state
-record(state, {
    listen_socket,
    callback_module,
    transport_module,
    connections = 0
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init([]) ->
    {ok, AutoDiscoveryGroup} = application:get_env(nxtfr_connection, autodiscovery_group),
    {ok, UserOptions} = application:get_env(nxtfr_connection, options),
    nxtfr_event:notify({join_autodiscovery_group, AutoDiscoveryGroup}),
    CallbackModule = get_user_option(callback_module, UserOptions, mandatory),
    Port = get_user_option(port, UserOptions, mandatory),
    TransportModule = get_user_option(transport_module, UserOptions, gen_tcp),
    DefaultNumListenProcs = get_user_option(num_listen_procs, UserOptions, 10),
    TcpOptions = extract_tcp_options(UserOptions, TransportModule),
    case TransportModule of
        ssl ->
            ssl:start();
        _ ->
            pass
    end,
    case TransportModule:listen(Port, TcpOptions) of
        {ok, ListenSocket} ->
            spawn_acceptor_pool(ListenSocket, CallbackModule, TransportModule, DefaultNumListenProcs),
            {ok, #state{
                listen_socket = ListenSocket,
                callback_module = CallbackModule,
                transport_module = TransportModule}};
        Reason ->
            {stop, Reason}
    end.

handle_call(Call, _From, State) ->
    error_logger:error_report([{undefined_call, Call}]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    error_logger:error_report([{undefined_cast, Cast}]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_report([{undefined_info, Info}]),
    {nopreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

spawn_acceptor_pool(_ListenSocket, _CallbackModule, _TransportModule, 0) ->
    done;

spawn_acceptor_pool(ListenSocket, CallbackModule, TransportModule, Procs) ->
    spawn_link(?MODULE, acceptor, [ListenSocket, CallbackModule, TransportModule]),
    spawn_acceptor_pool(ListenSocket, CallbackModule, TransportModule, Procs -1).

acceptor(ListenSocket, CallbackModule, TransportModule) ->
    case TransportModule of
        ssl ->
            accept_ssl(ListenSocket, CallbackModule);
        gen_tcp ->
            accept_tcp(ListenSocket, CallbackModule);
        InvalidTransportModule ->
            error_logger:error_report({{invalid_transport_module, InvalidTransportModule}, {valid_options, [ssl, gen_tcp]}})
    end,
    ?MODULE:acceptor(ListenSocket, CallbackModule, TransportModule).

accept_ssl(ListenSocket, CallbackModule) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, TLSTransportSocket} ->
            {ok, Socket} = ssl:handshake(TLSTransportSocket),
            case nxtfr_connection_statem_sup:start(CallbackModule, ssl, Socket) of
                {ok, Pid} ->
                    pass;
                Error ->
                    error_logger:info_report([{accept_ssl, Error}])
            end;
        Error ->
            error_logger:info_report([{accept_ssl, Error}])
    end.

accept_tcp(ListenSocket, CallbackModule) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case nxtfr_connection_statem_sup:start(CallbackModule, gen_tcp, Socket) of
                {ok, Pid} ->
                    pass;
                Error ->
                    error_logger:info_report([{accept_tcp, Error}])
            end;
        Error ->
            error_logger:info_report([{accept_tcp, Error}])
    end.

extract_tcp_options(UserOptions, ssl) ->
    extract_tcp_options([
        dhfile,
        cacertfile,
        certfile,
        keyfile,
        nodelay,
        reuseaddr],
        UserOptions,
        ?DEFAULT_TCP_OPTS,
        []);

extract_tcp_options(UserOptions, gen_tcp) ->
    extract_tcp_options([
        nodelay,
        reuseaddr],
        UserOptions,
        ?DEFAULT_TCP_OPTS,
        []).

extract_tcp_options([], _UserOptions, _DefaultOptions, Acc) ->
    lists:flatten([[{mode, binary}, {packet, raw}, {active, false}] | Acc]);

extract_tcp_options([Key | Rest], UserOptions, DefaultOptions, Acc) ->
    case proplists:lookup(Key, UserOptions) of
        none ->
            case proplists:lookup(Key, DefaultOptions) of
                none ->
                    extract_tcp_options(Rest, UserOptions, DefaultOptions, Acc);
                UserOption ->
                    extract_tcp_options(Rest, UserOptions, DefaultOptions, [UserOption | Acc])
            end;
        UserOption ->
            extract_tcp_options(Rest, UserOptions, DefaultOptions, [UserOption | Acc])
    end.

get_user_option(Key, Proplist, mandatory) ->
    case proplists:get_value(Key, Proplist) of
        undefined -> error({mandatory_user_option_missing, Key});
        Value -> Value
    end;

get_user_option(Key, Proplist, Default) ->
    proplists:get_value(Key, Proplist, Default).

