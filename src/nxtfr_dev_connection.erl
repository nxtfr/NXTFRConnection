-module(nxtfr_dev_connection).
-author("christian@flodihn.se").

-define(LOGIN, 1).
-define(REGISTER, 2).
-define(PLAYING, 3).

-define(LOGIN_SUCCESS, 1).
-define(REGISTER_SUCCESS, 2).
-define(STARTED_PLAYING, 3).
-define(PLAY_PACKET, 4).

-export([
    connected/2,
    logged_in/2,
    playing/2]).

connected(<<?LOGIN:8/integer, Packet/binary>>, Data) ->
    error_logger:info_report({?MODULE, {login_packet, Packet}, connected, Data}),
    String = make_string(<<"Login Successful">>),
    {next_state, logged_in, <<?LOGIN_SUCCESS:8/integer, String/binary>>, Data};

connected(<<
        Len:8/integer, Str:Len/binary,
        Float:32/float-little,
        Int:32/integer-signed-little,
        Short:16/integer-signed-little,
        Uint:32/integer-unsigned-little,
        UShort:16/integer-unsigned-little>> = Message, Data) ->
    error_logger:info_report({?MODULE, {hello_world_packet, Len, Str, Float, Int, Short, Uint, UShort}, connected, Data}),
    {next_state, connected, Message, Data};

connected(UnknownPacket, Data) ->
    error_logger:info_report({?MODULE, {unknown_packet, UnknownPacket}, connected, Data}),
    {next_state, connected, noreply, Data}.

logged_in(<<?REGISTER:8/integer, Packet/binary>>, Data) ->
    error_logger:info_report({?MODULE, {register_packet, Packet}, logged_in, Data}),
    String = make_string(<<"Registration Successful">>),
    {next_state, logged_in, <<?REGISTER_SUCCESS:8/integer, String/binary>>, Data};

logged_in(<<?PLAYING:8/integer, Packet/binary>>, Data) ->
    error_logger:info_report({?MODULE, {playing_packet, Packet}, logged_in, Data}),
    String = make_string(<<"Started playing">>),
    {next_state, playing, <<?STARTED_PLAYING:8/integer, String/binary>>, Data};

logged_in(UnknownPacket, Data) ->
    error_logger:info_report({?MODULE, {unknown_packet, UnknownPacket}, logged_in, Data}),
    {next_state, logged_in, noreply, Data}.

playing(Packet, Data) ->
    error_logger:info_report({?MODULE, {packet, Packet}, playing, Data}),
    {next_state, playing, <<?PLAY_PACKET:8/integer, Packet/binary>>, Data}.

make_string(String) ->
    StrSize = byte_size(String),
    <<StrSize:8/integer, String/bitstring>>.