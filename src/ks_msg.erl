-module(ks_msg).
-behaviour(ow_router).

-export([decode/2, decode/4, encode/2, encode/4]).

-define(PROTOBUF_LIB, ks_pb).
-define(APP, ks).

encode(SubMsg, Call) ->
    encode(SubMsg, Call, ?PROTOBUF_LIB, ?APP).
encode(SubMsg, Call, Lib, App) ->
    ow_msg:encode(SubMsg, Call, Lib, App).
decode(Msg, Session) -> 
    decode(Msg, Session, ?PROTOBUF_LIB, ?APP).
decode(Msg, Session, Lib, App) -> 
    ow_msg:decode(Msg, Session, Lib, App).
