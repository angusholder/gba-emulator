def match_ext(n, pat):
    pat = pat.lower()
    for i in range(0, 12):
        bit = (1 << (11 - i)) & n != 0
        if pat[i] == '0':
            if bit:
                return False
        elif pat[i] == '1':
            if not bit:
                return False
        elif pat[i] == 'x':
            continue
    return True

def match(n, *pat):
    return any(match_ext(n, p) for p in pat)

AND = 0b0000
EOR = 0b0001
SUB = 0b0010
RSB = 0b0011
ADD = 0b0100
ADC = 0b0101
SBC = 0b0110
RSC = 0b0111
TST = 0b1000
TEQ = 0b1001
CMP = 0b1010
CMN = 0b1011
ORR = 0b1100
MOV = 0b1101
BIC = 0b1110
MVN = 0b1111

OPCODES = {
    AND: ('and', 'ands'),
    EOR: ('eor', 'eors'),
    SUB: ('sub', 'subs'),
    RSB: ('rsb', 'rsbs'),
    ADD: ('add', 'adds'),
    ADC: ('adc', 'adcs'),
    SBC: ('sbc', 'sbcs'),
    RSC: ('rsc', 'rscs'),
    TST: (None,  'tst'),
    TEQ: (None,  'teq'),
    CMP: (None,  'cmp'),
    CMN: (None,  'cmn'),
    ORR: ('orr', 'orrs'),
    MOV: ('mov', 'movs'),
    BIC: ('bic', 'bics'),
    MVN: ('mvn', 'mvns'),
}

DATA_PROCESSING_FLAG_UPDATES = {}

DATA_PROCESSING_FLAG_UPDATES[AND] = DATA_PROCESSING_FLAG_UPDATES[EOR] = DATA_PROCESSING_FLAG_UPDATES[ORR] = DATA_PROCESSING_FLAG_UPDATES[MOV] = DATA_PROCESSING_FLAG_UPDATES[BIC] = DATA_PROCESSING_FLAG_UPDATES[MVN] = DATA_PROCESSING_FLAG_UPDATES[TST] = DATA_PROCESSING_FLAG_UPDATES[TEQ] = '''\
    set_zn(arm, result);\
'''

DATA_PROCESSING_FLAG_UPDATES[ADD] = DATA_PROCESSING_FLAG_UPDATES[ADC] = DATA_PROCESSING_FLAG_UPDATES[CMN] = '''\
    set_zn(arm, result);
    add_set_vc(arm, op1, op2);\
'''

DATA_PROCESSING_FLAG_UPDATES[RSB] = '''\
    set_zn(arm, result);
    sub_set_vc(arm, op2, op1);\
'''

DATA_PROCESSING_FLAG_UPDATES[SUB] = DATA_PROCESSING_FLAG_UPDATES[SBC] = DATA_PROCESSING_FLAG_UPDATES[CMP] = '''\
    set_zn(arm, result);
    sub_set_vc(arm, op1, op2);\
'''

DATA_PROCESSING_FLAG_UPDATES[RSC] = '''\
    set_zn(arm, result);
    sub_set_vc(arm, op2, op1);\
'''

DATA_PROCESSING_CALCULATION = {
    AND: 'op1 & op2',
    EOR: 'op1 ^ op2',
    SUB: 'op1.wrapping_sub(op2)',
    RSB: 'op2.wrapping_sub(op1)',
    ADD: 'op1.wrapping_add(op2)',
    ADC: 'op1.wrapping_add(op2.wrapping_add(arm.cpsr.c as u32))',
    SBC: 'op1.wrapping_sub(op2.wrapping_add(!arm.cpsr.c as u32))',
    RSC: 'op2.wrapping_sub(op1.wrapping_add(!arm.cpsr.c as u32))',
    TST: 'op1 & op2',
    TEQ: 'op1 ^ op2',
    CMP: 'op1.wrapping_sub(op2)',
    CMN: 'op1.wrapping_add(op2)',
    ORR: 'op1 | op2',
    MOV: 'op2',
    BIC: 'op1 & !op2',
    MVN: '!op2',
}

BARREL_SHIFT_OPS = {
    0b00: 'lsl',
    0b01: 'lsr',
    0b10: 'asr',
    0b11: 'ror',
}

LDM = {
# (up, preindex)
(True, False): ('ldmia', '''
    for i in 0..15 { // Post-increment
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        let target = add_cycles!(cycles, interconnect.read32(addr));
        arm.branch_to(interconnect, target);
        addr += 4;
    }\
'''),
(True, True): ('ldmib', '''
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            addr += 4;
            check_watchpoint!(arm, addr);
            arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        addr += 4;
        check_watchpoint!(arm, addr);
        let target = add_cycles!(cycles, interconnect.read32(addr));
        arm.branch_to(interconnect, target);
    }\
'''),
(False, False): ('ldmda', '''
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4 + 4;
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        let target = add_cycles!(cycles, interconnect.read32(addr));
        arm.branch_to(interconnect, target);
    }
    addr = new_base;\
'''),
(False, True): ('ldmdb', '''
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4;
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        let target = add_cycles!(cycles, interconnect.read32(addr));
        arm.branch_to(interconnect, target);
    }
    addr = new_base;\
''')
}

STM = {
# (up, preindex)
(True, False): ('stmia', '''
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            cycles += interconnect.write32(addr, arm.regs[i]);
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        // address of current instruction + 12 is stored
        cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
        addr += 4;
    }\
'''),
(True, True): ('stmib', '''
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            addr += 4;
            check_watchpoint!(arm, addr);
            cycles += interconnect.write32(addr, arm.regs[i]);
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        addr += 4;
        check_watchpoint!(arm, addr);
        // address of current instruction + 12 is stored
        cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
    }\
'''),
(False, False): ('stmda', '''
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4 + 4;
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            cycles += interconnect.write32(addr, arm.regs[i]);
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        // address of current instruction + 12 is stored
        cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
    }
    addr = new_base;\
'''),
(False, True): ('stmdb', '''
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4;
    for i in 0..15 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            cycles += interconnect.write32(addr, arm.regs[i]);
            addr += 4;
        }
    }
    if reglist & (1 << REG_PC) != 0 {
        check_watchpoint!(arm, addr);
        // address of current instruction + 12 is stored
        cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
    }
    addr = new_base;\
''')
}

def get_second_operand(I, set_cc, discriminant):
    if I:
        return '''{
        let rotate = op >> 8 & 0xF;
        let imm = op & 0xFF;
        imm.rotate_right(rotate * 2)
    }\
'''
    else:
        if discriminant & 0x1: #  Shift by register
            shift_amount = 'arm.regs[(op >> 8 & 0xF) as usize]'
            cycles = 1
        else: # Shift by 5-bit immediate
            shift_amount = 'op >> 7 & 0x1F'
            cycles = 0

        shift_type = discriminant >> 1 & 3
        shift_func = BARREL_SHIFT_OPS[shift_type]
        if set_cc:
            shift_func += '_set_flags'

        return f'''{{
        let rm = arm.regs[(op & 0xF) as usize];
        let shift_amount = {shift_amount};
        cycles += {cycles};
        barrel_shift_{shift_func}(arm, rm, shift_amount)
    }}\
'''

def emit_data_processing(imm, discr):
    opcode = (i >> 5) & 0xF
    set_cc = i & 0x010 != 0
    if opcode in (TST, TEQ, CMP, CMN):
        assert set_cc
    op2 = get_second_operand(imm, set_cc, discr)
    calculation = DATA_PROCESSING_CALCULATION[opcode]
    flag_update = DATA_PROCESSING_FLAG_UPDATES[opcode] if set_cc else ''
    result_writeback = '''
    if rd_index != REG_PC {
        arm.regs[rd_index] = result;
    } else {
        arm.branch_to(interconnect, result);
    }\
'''
    if opcode & 0b1100 == 0b1000:
        result_writeback = ''

    return f'''\
    let op1 = arm.regs[(op >> 16 & 0xF) as usize];
    let rd_index = (op >> 12 & 0xF) as usize;
    let op2 = {op2};
    let result = {calculation};
{flag_update}{result_writeback}'''

def decode(i):
    ins_name = None
    fn_body = None

    # Multiply
    if match(i, '000000xx1001'):
        acc = i & 0x020 != 0
        set_cc = i & 0x010 != 0

        ins_name = 'mla' if acc else 'mul'
        if set_cc: ins_name += 's'

        result = '{ cycles += 1; rm.wrapping_mul(rs).wrapping_add(rn) }' if acc else 'rm.wrapping_mul(rs)'
        flag_update = '\n\n    set_zn(arm, result);' if set_cc else ''

        fn_body = f'''\
    let rd_index = (op >> 16 & 0xF) as usize;
    let rn_index = (op >> 12 & 0xF) as usize;
    let rs_index = (op >> 8 & 0xF) as usize;
    let rm_index = (op & 0xF) as usize;

    debug_assert!(rd_index != rm_index);
    debug_assert!(rd_index != REG_PC);
    debug_assert!(rs_index != REG_PC);
    debug_assert!(rn_index != REG_PC);
    debug_assert!(rm_index != REG_PC);

    let rn = arm.regs[rn_index];
    let rs = arm.regs[rs_index];
    let rm = arm.regs[rm_index];

    cycles += (rs.leading_zeros() / 8) as i32;

    let result = {result};
    arm.regs[rd_index] = result;{flag_update}'''
    # Multiply Long
    if match(i, '00001xxx1001'):
        assert not ins_name
        unsigned = i & 0x040 != 0
        acc = i & 0x020 != 0
        set_cc = i & 0x010 != 0

        ins_name = 'mlal' if acc else 'mull'
        ins_name = ('u' if unsigned else 's') + ins_name
        if set_cc: ins_name += 's'

        flag_update = '\n    arm.cpsr.c = result == 0;\n    arm.cpsr.n = (result as i64) < 0;' if set_cc else ''

        if unsigned:
            cycle_count = '1 + rs.leading_zeros() / 8'
        else:
            cycle_count = '1 + cmp::max(rs.leading_zeros(), (!rs).leading_zeros()) / 8'

        if unsigned:
            if acc:
                result = '''{
        let a = arm.regs[rm_index] as u64;
        let b = arm.regs[rs_index] as u64;

        let low = arm.regs[rd_lo_index] as u64;
        let high = arm.regs[rd_hi_index] as u64;
        let c = low | (high << 32);
        cycles += 1;
        a.wrapping_mul(b).wrapping_add(c)
    }'''
            else:
                result = '''{
        let a = arm.regs[rm_index] as u64;
        let b = arm.regs[rs_index] as u64;

        a.wrapping_mul(b)
    }'''
        else:
            if acc:
                result = '''{
        let a = arm.regs[rm_index] as i32 as i64;
        let b = arm.regs[rs_index] as i32 as i64;

        let low = arm.regs[rd_lo_index] as i32 as i64;
        let high = arm.regs[rd_hi_index] as i32 as i64;
        let c = low | (high << 32);
        cycles += 1;
        (a * b + c) as u64
    }'''
            else:
                result = '''{
        let a = arm.regs[rm_index] as i32 as i64;
        let b = arm.regs[rs_index] as i32 as i64;

        (a * b) as u64
    }'''

        fn_body = f'''\
    let rd_hi_index = (op >> 16 & 0xF) as usize;
    let rd_lo_index = (op >> 12 & 0xF) as usize;
    let rs_index = (op >> 8 & 0xF) as usize;
    let rm_index = (op & 0xF) as usize;

    debug_assert!(rd_hi_index != REG_PC);
    debug_assert!(rd_lo_index != REG_PC);
    debug_assert!(rs_index != REG_PC);
    debug_assert!(rm_index != REG_PC);
    debug_assert!(rd_hi_index != rd_lo_index);
    debug_assert!(rd_hi_index != rm_index);
    debug_assert!(rd_lo_index != rm_index);

    let rs = arm.regs[rs_index];
    cycles += ({cycle_count}) as i32;

    let result: u64 = {result};
    arm.regs[rd_lo_index] = result as u32;
    arm.regs[rd_hi_index] = (result >> 32) as u32;
    {flag_update}'''

    # Single Data Transfer
    if match(i, '011xxxxxXXX0', '010xxxxxXXXX'):
        # TODO: Load instructions have an extra internal cycle for address calculation, but apparently stores don't?
        assert not ins_name
        imm = i & 0x200 == 0
        preindex = i & 0x100 != 0
        up = i & 0x080 != 0
        byte = i & 0x040 != 0
        writeback = i & 0x020 != 0
        load = i & 0x010 != 0

        ins_name = 'ldr' if load else 'str'
        if byte: ins_name += 'b'
        ins_name += '_pre' if preindex else '_post'
        ins_name += 'inc' if up else 'dec'
        ins_name += '_wb' if writeback else ''

        if imm:
            offset = 'op & 0xFFF'
            ins_name += '_imm'
        else:
            shift_type = i >> 1 & 3
            ins_name += '_' + BARREL_SHIFT_OPS[shift_type] + '_imm'
            offset = get_second_operand(False, False, i)

        if not up:
            offset = f'-(({offset}) as i32) as u32'

        addr = 'rn.wrapping_add(offset)' if preindex else 'rn'

        operation = {
            (False, False): 'cycles += interconnect.write32(addr, arm.regs[rd_index]);',
            (False, True ): 'cycles += interconnect.write8(addr, arm.regs[rd_index] as u8);',
            (True , False): 'add_cycles!(cycles, interconnect.read32(addr))',
            (True , True ): 'add_cycles!(cycles, interconnect.read8(addr)) as u32',
        }[(load, byte)]

        if load:
            operation = f'''\
let value = {operation};
    cycles += 1; // internal cycle for address calculation
    if rd_index != REG_PC {{
        arm.regs[rd_index] = value;
    }} else {{
        arm.branch_to(interconnect, value);
    }}'''

        # assert preindex or writeback
        if writeback:
            post_op = '\n\n    arm.regs[rn_index] += offset;'
        else:
            post_op = ''

        fn_body = f'''\
    let rn_index = (op >> 16 & 0xF) as usize;
    let rd_index = (op >> 12 & 0xF) as usize;
    let rn = arm.regs[rn_index];

    let offset = {offset};

    let addr = {addr};
    check_watchpoint!(arm, addr);

    {operation};{post_op}'''

    # Halfword Data Load
    if match(i, '000xxxx11111', '000xxxx11011', '000xxxx11101') and match(i, '0000xx111xx1', '0001xx111xx1', '0001xx011xx1'):
        assert not ins_name
        preindex = i & 0x100 != 0
        up = i & 0x080 != 0
        imm = i & 0x040 != 0
        writeback = i & 0x020 != 0
        signed = i & 0x004 != 0
        half = i & 0x002 != 0

        (ins_name, operation) = {
            (True, True): ('ldrsh', 'add_cycles!(cycles, interconnect.read16(addr)) as i16 as u32'),
            (False, True): ('ldrh', 'add_cycles!(cycles, interconnect.read16(addr)) as u32'),
            (True, False): ('ldrsb', 'add_cycles!(cycles, interconnect.read8(addr)) as i8 as u32'),
        }[(signed, half)]

        ins_name += '_pre' if preindex else '_post'
        ins_name += 'inc' if up else 'dec'
        ins_name += '_wb' if writeback else ''
        ins_name += '_immoffset' if imm else '_regoffset'

        assert preindex or writeback
        if writeback:
            post_op = '\n\n    arm.regs[rn_index] += offset;'
        else:
            post_op = ''

        addr = 'rn.wrapping_add(offset)' if preindex else 'rn'

        offset = '((op >> 4) & 0xF0) | (op & 0x0F)' if imm else 'arm.regs[(op & 0xF) as usize]'

        fn_body = f'''\
    let rn_index = (op >> 16 & 0xF) as usize;
    let rd_index = (op >> 12 & 0xF) as usize;
    let rn = arm.regs[rn_index];

    let offset = {offset};

    let addr = {addr};
    check_watchpoint!(arm, addr);

    let value = {operation};
    cycles += 1; // internal cycle for address calculation
    if rd_index != REG_PC {{
        arm.regs[rd_index] = value;
    }} else {{
        arm.branch_to(interconnect, value);
    }}{post_op}'''

    # Halfword Data Store
    if match(i, '0001xx001011', '0000xx101011', '0001xx101011'):
        assert not ins_name
        preindex = i & 0x100 != 0
        up = i & 0x080 != 0
        imm = i & 0x040 != 0
        writeback = i & 0x020 != 0

        ins_name = 'strh'
        ins_name += '_pre' if preindex else '_post'
        ins_name += 'inc' if up else 'dec'
        ins_name += '_wb' if writeback else ''
        ins_name += '_immoffset' if imm else '_regoffset'

        assert preindex or writeback
        if writeback:
            post_op = '\n\n    arm.regs[rn_index] += offset;'
        else:
            post_op = ''

        addr = 'rn.wrapping_add(offset)' if preindex else 'rn'

        offset = '((op >> 4) & 0xF0) | (op & 0x0F)' if imm else 'arm.regs[(op & 0xF) as usize]'

        fn_body = f'''\
    let rn_index = (op >> 16 & 0xF) as usize;
    let rd_index = (op >> 12 & 0xF) as usize;
    let rn = arm.regs[rn_index];

    let offset = {offset};

    let addr = {addr};
    check_watchpoint!(arm, addr);

    cycles += interconnect.write16(addr, rn as u16);{post_op}'''

    # Branch
    if match(i, '101xXXXXxxxx'):
        assert not ins_name
        link = i & 0x100 != 0

        ins_name = 'bl' if link else 'b'
        do_link = '\n    arm.regs[REG_LR] = pc - 4;\n' if link else ''

        fn_body = f'''\
    // Sign extend offset
    let offset = if op & 0x80_0000 != 0 {{
        ((op & 0xFF_FFFF) | 0xFF00_0000) << 2
    }} else {{
        (op & 0xFF_FFFF) << 2
    }};

    let pc = arm.regs[REG_PC];
    let addr = pc.wrapping_add(offset);
    {do_link}
    arm.branch_to(interconnect, addr);'''

    # Data Processing (register)
    is_test_ins = ((i >> 5) & 0b1100) == 0b1000
    set_cc = ((i >> 4) & 1) != 0
    if match(i, '000XXXXx0xx0', '000XXXXx1xx0', '000XXXXx0xx1') and (set_cc or not is_test_ins):
        assert not ins_name
        opcode = (i >> 5) & 0xF
        set_cc = i & 0x010 != 0
        shift_op = i >> 1 & 3
        assert OPCODES[opcode][set_cc] is not None
        ins_name = OPCODES[opcode][set_cc] + '_' + BARREL_SHIFT_OPS[shift_op]
        if i & 1:
            ins_name += '_reg'
        else:
            ins_name += '_imm'
        fn_body = emit_data_processing(False, i)
    # Data Processing (immediate)
    if match(i, '001XXXXxXXXX') and (set_cc or not is_test_ins):
        assert not ins_name
        opcode = (i >> 5) & 0xF
        set_cc = i & 0x010 != 0
        assert OPCODES[opcode][set_cc] is not None
        ins_name = OPCODES[opcode][set_cc] + '_imm'
        fn_body = emit_data_processing(True, i)

    # SWI
    if match(i, '1111xxxxxxxx'):
        assert not ins_name
        ins_name = 'swi'

    # Branch and Exchange
    if match(i, '000100100001'):
        assert not ins_name
        ins_name = 'bx'

    # Undefined
    if match(i, '011xXXXXxxx1'):
        assert not ins_name
        ins_name = 'und'

    # Block Data Transfer
    if match(i, '100xxxxxXXXX'):
        assert not ins_name
        load = ((i >> 4) & 1) != 0
        writeback = ((i >> 5) & 1) != 0
        # set_cc = ((i >> 6) & 1) != 0
        up = ((i >> 7) & 1) != 0
        preindex = ((i >> 8) & 1) != 0

        if load:
            (ins_name, fn_body) = LDM[(up, preindex)]
        else:
            (ins_name, fn_body) = STM[(up, preindex)]

        if writeback:
            fn_body += '    arm.regs[rn_index] = addr;'
            ins_name += '_wb'

        fn_body = f'''\
    let rn_index = (op >> 16 & 0xF) as usize;
    let set_cc = op >> 22 & 1 != 0;
    let reglist = op & 0xFFFF;

    if set_cc {{
        panic!("{ins_name} set_cc is unimplemented");
    }}

    let mut addr = arm.regs[rn_index];
    {fn_body}'''

    # MRS (PSR to register)
    if match(i, '00010x000000'):
        assert not ins_name
        ins_name = 'mrs_reg'

    # MSR (Register to PSR/PSR_flags)
    if match(i, '00010x100000'):
        assert not ins_name
        ins_name = 'msr_reg'

    # MSR (Imm to PSR flags)
    if match(i, '00110x10xxxx'):
        assert not ins_name
        ins_name = 'msr_flag_imm'

    # Single Data Swap
    elif match(i, '00010x001001'):
        byte = i & 0x040 != 0
        ins_name = 'swpb' if byte else 'swp'

    # Coprocessor
    if match(i, '110xxxxxXXXX', '1110xxxxXXXX'):
        assert not ins_name
        ins_name = 'coprocessor'

    return (ins_name, fn_body)



FN_TEMPLATE = '''\
#[no_mangle]
fn op_{ins_name}(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {{
    let mut cycles = Cycle(0);

{fn_body}

    arm.cycles += cycles;
    StepEvent::None
}}
'''

lut = []
functions = {}
for i in range(1<<12):
    (ins_name, fn_body) = decode(i)

    if ins_name is not None:
        lut.append('op_' + ins_name)
    else:
        lut.append('unhandled')
        ins = 0xE0012304 | ((i&0xF) << 4) | ((i&0xFF0) << 16)
        # print(f'{ins:08X}')

    if fn_body is not None:
        full_fn = FN_TEMPLATE.format(
            ins_name = ins_name,
            fn_body = fn_body
        )

        # Check that a particular function always gets generated the same
        # ie: that we don't follow different code paths when decoding
        # bit patterns that should function identically.
        assert functions.get(ins_name) is None or functions[ins_name] == full_fn

        functions[ins_name] = full_fn

def writelines(output):
    lut_string = f'pub static ARM_LUT: [ArmOp; 4096] = [{", ".join(lut)}];'
    output(lut_string)

    for name, fn in sorted(functions.items(), key=lambda it: it[0]):
        output(fn)

def generate(filename):
    with open(filename, 'wt') as f:
        def write_fn(*args, **kwargs):
            print(*args, **kwargs, file=f)
        writelines(write_fn)
