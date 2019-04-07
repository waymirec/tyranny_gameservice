-define(OPCODE_LEN, 16).

-define(OP_IDENT, 20).
-define(OP_PING, 21).
-define(OP_PONG, 22).

-define(HDR_PING, ?OP_PING:?OPCODE_LEN).
-define(HDR_PONG, ?OP_PONG:?OPCODE_LEN).
