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
            cycles = '\ninterconnect.add_internal_cycles(1);'
        else: # Shift by 5-bit immediate
            shift_amount = 'op >> 7 & 0x1F'
            cycles = ''

        shift_type = discriminant >> 1 & 3
        shift_func = BARREL_SHIFT_OPS[shift_type]
        if set_cc:
            shift_func += '_set_flags'

        return f'''{{
        let rm = arm.regs[(op & 0xF) as usize];
        let shift_amount = {shift_amount};{cycles}
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
            offset = f'(({offset}) as i32).wrapping_neg() as u32'

        addr = 'rn.wrapping_add(offset)' if preindex else 'rn'

        operation = {
            (False, False): 'interconnect.write32(addr, arm.regs[rd_index]);',
            (False, True ): 'interconnect.write8(addr, arm.regs[rd_index] as u8);',
            (True , False): 'interconnect.read32(addr)',
            (True , True ): 'interconnect.read8(addr) as u32',
        }[(load, byte)]

        if load:
            operation = f'''\
let value = {operation};
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
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
            (True, True): ('ldrsh', 'interconnect.read16(addr) as i16 as u32'),
            (False, True): ('ldrh', 'interconnect.read16(addr) as u32'),
            (True, False): ('ldrsb', 'interconnect.read8(addr) as i8 as u32'),
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
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
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

    interconnect.write16(addr, rn as u16);{post_op}'''

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

    return (ins_name, fn_body)



FN_TEMPLATE = '''\
#[no_mangle]
fn op_{ins_name}(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {{
{fn_body}

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
    lut_string = f'pub static ARM_LUT: [fn(&mut Arm7TDMI, &mut Interconnect, u32) -> StepEvent; 4096] = [{", ".join(lut)}];'
    output(lut_string)

    for name, fn in sorted(functions.items(), key=lambda it: it[0]):
        output(fn)

def generate(filename):
    with open(filename, 'wt') as f:
        def write_fn(*args, **kwargs):
            print(*args, **kwargs, file=f)
        writelines(write_fn)
