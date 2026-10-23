import struct

# Opcode constants (Lua 5.1)
OP_MOVE = 0
OP_LOADK = 1
OP_LOADBOOL = 2
OP_LOADNIL = 3
OP_GETUPVAL = 4
OP_GETGLOBAL = 5
OP_GETTABLE = 6
OP_SETGLOBAL = 7
OP_SETUPVAL = 8
OP_SETTABLE = 9
OP_NEWTABLE = 10
OP_SELF = 11
OP_ADD = 12
OP_SUB = 13
OP_MUL = 14
OP_DIV = 15
OP_MOD = 16
OP_POW = 17
OP_UNM = 18
OP_NOT = 19
OP_LEN = 20
OP_CONCAT = 21
OP_JMP = 22
OP_EQ = 23
OP_LT = 24
OP_LE = 25
OP_TEST = 26
OP_TESTSET = 27
OP_CALL = 28
OP_TAILCALL = 29
OP_RETURN = 30
OP_FORLOOP = 31
OP_FORPREP = 32
OP_TFORLOOP = 33
OP_SETLIST = 34
OP_CLOSE = 35
OP_CLOSURE = 36
OP_VARARG = 37

def iABC(op, a, b, c):
    return (op & 0x3F) | ((a & 0xFF) << 6) | ((c & 0x1FF) << 14) | ((b & 0x1FF) << 23)

def iABx(op, a, bx):
    return (op & 0x3F) | ((a & 0xFF) << 6) | ((bx & 0x3FFFF) << 14)

def isBx(op, a, sbx):
    return (op & 0x3F) | ((a & 0xFF) << 6) | (((sbx + 131071) & 0x3FFFF) << 14)

def RK_const(idx):
    return 0x100 | (idx & 0xFF)

def RK_reg(r):
    return r & 0xFF

def write_string(s):
    if s is None:
        return struct.pack("<Q", 0)
    data = s.encode("utf-8") + b"\x00"
    return struct.pack("<Q", len(data)) + data

def write_header():
    # 12-byte header: \027Lua, 0x51, 0, 1, 4, 8, 4, 8, 0
    return b"\x1bLua\x51\x00\x01\x04\x08\x04\x08\x00"

def write_proto(source_name, line_defined, last_line_defined, num_upvalues, num_params, is_vararg, max_stack, code, constants, sub_protos=None):
    if sub_protos is None:
        sub_protos = []
    
    buf = bytearray()
    buf.extend(write_string(source_name))
    buf.extend(struct.pack("<iiBBBB", line_defined, last_line_defined, num_upvalues, num_params, is_vararg, max_stack))
    
    # Code
    buf.extend(struct.pack("<i", len(code)))
    for insn in code:
        buf.extend(struct.pack("<I", insn))
        
    # Constants
    buf.extend(struct.pack("<i", len(constants)))
    for c in constants:
        if c is None:
            buf.append(0) # Nil
        elif isinstance(c, bool):
            buf.append(1) # Boolean
            buf.append(1 if c else 0)
        elif isinstance(c, (int, float)):
            buf.append(3) # Number
            buf.extend(struct.pack("<d", float(c)))
        elif isinstance(c, str):
            buf.append(4) # String
            buf.extend(write_string(c))
            
    # Sub-prototypes
    buf.extend(struct.pack("<i", len(sub_protos)))
    for sp in sub_protos:
        buf.extend(sp)
        
    # Source lines debug info
    buf.extend(struct.pack("<i", len(code)))
    for i in range(len(code)):
        buf.extend(struct.pack("<i", line_defined + (i // 2)))
        
    # Local vars debug info
    buf.extend(struct.pack("<i", 0))
    
    # Upvalue names debug info
    buf.extend(struct.pack("<i", 0))
    
    return bytes(buf)

def generate_complex_safe():
    # Constants:
    # 0: 0.0, 1: 1.0, 2: 2.0, 3: 3.0, 4: "under threshold", 5: "exceeded threshold"
    constants = [0.0, 1.0, 2.0, 3.0, "under threshold", "exceeded threshold"]
    
    # 25 instructions implementing loop, nested branches, multiple returns
    code = [
        # pc 0: total = 0
        iABx(OP_LOADK, 2, 0),
        # pc 1: flag = true
        iABC(OP_LOADBOOL, 3, 1, 0),
        # pc 2: init = 1
        iABx(OP_LOADK, 4, 1),
        # pc 3: limit = limit (R0)
        iABC(OP_MOVE, 5, 0, 0),
        # pc 4: step = 1
        iABx(OP_LOADK, 6, 1),
        # pc 5: forprep R4 -> jumps to pc 17 (sbx = 11)
        isBx(OP_FORPREP, 4, 11),
        
        # Loop body: pc 6..16
        # pc 6: R8 = i (R7) % 2 (K2)
        iABC(OP_MOD, 8, RK_reg(7), RK_const(2)),
        # pc 7: if R8 == 0 (K0)
        iABC(OP_EQ, 0, RK_reg(8), RK_const(0)),
        # pc 8: jump to odd branch (pc 12, sbx = 3)
        isBx(OP_JMP, 0, 3),
        
        # Even branch:
        # pc 9: R9 = i * 3
        iABC(OP_MUL, 9, RK_reg(7), RK_const(3)),
        # pc 10: total = total + R9
        iABC(OP_ADD, 2, RK_reg(2), RK_reg(9)),
        # pc 11: jump to check threshold (pc 13, sbx = 1)
        isBx(OP_JMP, 0, 1),
        
        # Odd branch:
        # pc 12: total = total - i
        iABC(OP_SUB, 2, RK_reg(2), RK_reg(7)),
        
        # Check threshold:
        # pc 13: if threshold (R1) < total (R2)
        iABC(OP_LT, 0, RK_reg(1), RK_reg(2)),
        # pc 14: false -> jump to loop latch (pc 17, sbx = 2)
        isBx(OP_JMP, 0, 2),
        
        # Break path:
        # pc 15: flag = false
        iABC(OP_LOADBOOL, 3, 0, 0),
        # pc 16: break out of loop -> jump to pc 18 (sbx = 1)
        isBx(OP_JMP, 0, 1),
        
        # Loop latch:
        # pc 17: forloop R4 -> back to pc 6 (sbx = -12)
        isBx(OP_FORLOOP, 4, -12),
        
        # Post-loop diamond:
        # pc 18: test flag (R3)
        iABC(OP_TEST, 3, 0, 0),
        # pc 19: false -> jump to pc 22 (sbx = 2)
        isBx(OP_JMP, 0, 2),
        
        # True branch:
        # pc 20: msg (R8) = "under threshold" (K4)
        iABx(OP_LOADK, 8, 4),
        # pc 21: jump to return (pc 23, sbx = 1)
        isBx(OP_JMP, 0, 1),
        
        # False branch:
        # pc 22: msg (R8) = "exceeded threshold" (K5)
        iABx(OP_LOADK, 8, 5),
        
        # Merge & Return:
        # pc 23: move R9 = R8
        iABC(OP_MOVE, 9, 8, 0),
        # pc 24: return total (R2) and msg (R9)
        iABC(OP_RETURN, 2, 3, 0),
    ]
    
    # Outer main proto (calls complex_func)
    main_proto = write_proto(
        source_name="@complex_test.lua",
        line_defined=1,
        last_line_defined=30,
        num_upvalues=0,
        num_params=2,
        is_vararg=0,
        max_stack=12,
        code=code,
        constants=constants
    )
    
    return write_header() + main_proto

def generate_complex_exploit():
    constants = [0.0, 42.0]
    # Code with multiple intentional binary exploits
    code = [
        # pc 0: LOADK R0 K(0)
        iABx(OP_LOADK, 0, 0),
        # pc 1: Out-of-bounds forward jump (target pc = 1 + 1 + 80 = 82 > code length 6)
        isBx(OP_JMP, 0, 80),
        # pc 2: Read of uninitialized register R(9)
        iABC(OP_MOVE, 1, 9, 0),
        # pc 3: Out-of-bounds backward jump (target pc = 3 + 1 - 10 = -6 < 0)
        isBx(OP_JMP, 0, -10),
        # pc 4: Conditional test without following JMP (at boundary)
        iABC(OP_EQ, 0, RK_reg(0), RK_const(1)),
    ]
    
    exploit_proto = write_proto(
        source_name="@exploit_payload.lua",
        line_defined=1,
        last_line_defined=10,
        num_upvalues=0,
        num_params=0,
        is_vararg=0,
        max_stack=5,
        code=code,
        constants=constants
    )
    return write_header() + exploit_proto

if __name__ == "__main__":
    with open("complex_safe.luac", "wb") as f:
        f.write(generate_complex_safe())
    print("[+] Generated complex_safe.luac successfully.")

    with open("complex_exploit.luac", "wb") as f:
        f.write(generate_complex_exploit())
    print("[+] Generated complex_exploit.luac successfully.")
