-define(OPCODE_LEN, 32).
-define(OP_PING, 0).
-define(OP_PONG, 1).

-define(HDR_PING, ?OP_PING:?OPCODE_LEN).
-define(HDR_PONG, ?OP_PONG:?OPCODE_LEN).
