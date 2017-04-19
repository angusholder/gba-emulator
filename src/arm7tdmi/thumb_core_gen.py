def match(i, begin, end=None):
    if end is None:
        end = begin
    return (i >> 2) in range(begin, end + 1)

ALU_OPS = {
0b0000: (
    'and_reg', 'AND Rd, Rs',
    '''
    let result = rd & rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
'''),

0b0001: (
    'eor_reg', 'EOR Rd, Rs',
    '''
    let result = rd ^ rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
'''),

0b0010: (
    'lsl_reg', 'LSL Rd, Rs',
    '''
    let result = barrel_shift_lsl_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    interconnect.add_internal_cycles(1);
'''),

0b0011: (
    'lsr_reg', 'LSR Rd, Rs',
    '''
    let result = barrel_shift_lsr_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    interconnect.add_internal_cycles(1);
'''),

0b0100: (
    'asr_reg', 'ASR Rd, Rs',
    '''
    let result = barrel_shift_asr_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    interconnect.add_internal_cycles(1);
'''),

0b0101: (
    'adc_reg', 'ADC Rd, Rs',
    '''
    let result = rd.wrapping_add(rs).wrapping_add(arm.cpsr.c as u32);
    set_zn(arm, result);
    add_set_vc(arm, rd, rs);
    arm.regs[rd_index] = result;
'''),

0b0110: (
    'sbc_reg', 'SBC Rd, Rs',
    '''
    let result = rs.wrapping_sub(rs).wrapping_sub(!arm.cpsr.c as u32);
    set_zn(arm, result);
    sub_set_vc(arm, rd, rs);
    arm.regs[rd_index] = result;
'''),

0b0111: (
    'ror_reg', 'ROR Rd, Rs',
    '''
    let result = barrel_shift_ror_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    interconnect.add_internal_cycles(1);
'''),

0b1000: (
    'tst_reg', 'TST Rd, Rs',
    '''
    set_zn(arm, rd & rs);
'''),

0b1001: (
    'neg_reg', 'NEG Rd, Rs',
    '''
    let result = (rs as i32).wrapping_neg() as u32;
    set_zn(arm, result);
    sub_set_vc(arm, 0, rs);
    arm.regs[rd_index] = result;
'''),

0b1010: (
    'cmp_reg', 'CMP Rd, Rs',
    '''
    let result = rd.wrapping_sub(rs);
    set_zn(arm, result);
    sub_set_vc(arm, rd, rs);
'''),

0b1011: (
    'cmn_reg', 'CMN Rd, Rs',
    '''
    let result = rs.wrapping_add(rd);
    set_zn(arm, result);
    add_set_vc(arm, rd, rs);
'''),

0b1100: (
    'orr_reg', 'ORR Rd, Rs',
    '''
    let result = rd | rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
'''),

0b1101: (
    'mul_reg', 'MUL Rd, Rs',
    '''
    let result = rd.wrapping_mul(rs);
    set_zn(arm, result);
    interconnect.add_internal_cycles((rs.leading_zeros() / 8) as i32);
    // MUL sets c and v to meaningless values, so we don't need to touch them.
    arm.regs[rd_index] = result;
'''),

0b1110: (
    'bic_reg', 'BIC Rd, Rs',
    '''
    let result = rd & !rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
'''),

0b1111: (
    'mvn_reg', 'MVN Rd, Rs',
    '''
    let result = !rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
'''),
}

CONDITION_CODES = {
    0b0000: 'Eq', # Z=1:            equal
    0b0001: 'Ne', # Z=0:            not equal
    0b0010: 'Cs', # C=1:            unsigned higher or same
    0b0011: 'Cc', # C=0:            unsigned lower
    0b0100: 'Mi', # N=1:            negative
    0b0101: 'Pl', # N=0:            positive or zero
    0b0110: 'Vs', # V=1:            overflow
    0b0111: 'Vc', # V=0:            no overflow
    0b1000: 'Hi', # C=1 AND Z=0:    unsigned higher
    0b1001: 'Ls', # C=0 OR Z=1:     unsigned lower or same
    0b1010: 'Ge', # N=V:            greater or equal
    0b1011: 'Lt', # N!=V:           less than
    0b1100: 'Gt', # Z=0 AND (N=V):  greater than
    0b1101: 'Le', # Z=1 OR (N!=V):  less than or equal
    0b1110: 'Al', # 1:              always
}

BRANCH_CC = '''
    let offset = ((op & 0xFF) as i8 as u32) << 1;
    if arm.eval_condition_code(ConditionCode::{cc}) {{
        let addr = arm.regs[REG_PC].wrapping_add(offset);
        arm.branch_to(interconnect, addr);
    }}
'''

LOAD_OP_REG_OFFSET = '''
    let rb = arm.regs[(op >> 3 & 7) as usize];
    let ro = arm.regs[(op >> 6 & 7) as usize];
    let addr = rb.wrapping_add(ro);
    check_watchpoint!(arm, addr);
    arm.regs[(op & 7) as usize] = interconnect.{load_fn}(addr) as {T} as u32;
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
'''

STORE_OP_REG_OFFSET = '''
    let rb = arm.regs[(op >> 3 & 7) as usize];
    let ro = arm.regs[(op >> 6 & 7) as usize];
    let addr = rb.wrapping_add(ro);
    check_watchpoint!(arm, addr);
    interconnect.{store_fn}(addr, arm.regs[(op & 7) as usize] as {T});
'''

LOAD_OP_IMMED_OFFSET = '''
    let rb = arm.regs[(op >> 3 & 7) as usize];
    let offset = ((op >> 6 & 0x1F) as u32) * mem::size_of::<{T}>() as u32;
    let addr = rb.wrapping_add(offset);
    check_watchpoint!(arm, addr);
    arm.regs[(op & 7) as usize] = interconnect.{load_fn}(addr) as {T} as u32;
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
'''

STORE_OP_IMMED_OFFSET = '''
    let rb = arm.regs[(op >> 3 & 7) as usize];
    let offset = ((op >> 6 & 0x1F) as u32) * mem::size_of::<{T}>() as u32;
    let addr = rb.wrapping_add(offset);
    check_watchpoint!(arm, addr);
    interconnect.{store_fn}(addr, arm.regs[(op & 7) as usize] as {T});
'''

FUNCTIONS = {
'lsl_imm': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let imm = (op >> 6 & 0x1F) as u32;
    let result = barrel_shift_lsl_set_flags(arm, rs, imm);
    set_zn(arm, result);
    arm.regs[(op & 7) as usize] = result;
''',

'lsr_imm': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let imm = (op >> 6 & 0x1F) as u32;
    let result = barrel_shift_lsr_set_flags(arm, rs, imm);
    set_zn(arm, result);
    arm.regs[(op & 7) as usize] = result;
''',

'asr_imm':'''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let imm = (op >> 6 & 0x1F) as u32;
    let result = barrel_shift_asr_set_flags(arm, rs, imm);
    set_zn(arm, result);
    arm.regs[(op & 7) as usize] = result;
''',

'add3_reg': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let rn = arm.regs[(op >> 6 & 7) as usize];
    let result = rs.wrapping_add(rn);
    set_zn(arm, result);
    add_set_vc(arm, rs, rn);
    arm.regs[(op & 7) as usize] = result;
''',

'sub3_reg': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let rn = arm.regs[(op >> 6 & 7) as usize];
    let result = rs.wrapping_sub(rn);
    set_zn(arm, result);
    sub_set_vc(arm, rs, rn);
    arm.regs[(op & 7) as usize] = result;
''',

'add3_imm': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let imm = (op >> 6 & 7) as u32;
    let result = rs.wrapping_add(imm);
    set_zn(arm, result);
    add_set_vc(arm, rs, imm);
    arm.regs[(op & 7) as usize] = result;
''',

'sub3_imm': '''
    let rs = arm.regs[(op >> 3 & 7) as usize];
    let imm = (op >> 6 & 7) as u32;
    let result = rs.wrapping_sub(imm);
    set_zn(arm, result);
    sub_set_vc(arm, rs, imm);
    arm.regs[(op & 7) as usize] = result;
''',

'mov_imm': '''
    let imm = (op & 0xFF) as u32;
    set_zn(arm, imm);
    arm.regs[(op >> 8 & 7) as usize] = imm;
''',

'cmp_imm': '''
    let imm = (op & 0xFF) as u32;
    let rd = arm.regs[(op >> 8 & 7) as usize];
    let result = rd.wrapping_sub(imm);
    set_zn(arm, result);
    sub_set_vc(arm, rd, imm);
''',

'add_imm': '''
    let imm = (op & 0xFF) as u32;
    let rd_index = (op >> 8 & 7) as usize;
    let rd = arm.regs[rd_index];
    let result = rd.wrapping_add(imm);
    set_zn(arm, result);
    add_set_vc(arm, rd, imm);
    arm.regs[rd_index] = result;
''',

'sub_imm': '''
    let imm = (op & 0xFF) as u32;
    let rd_index = (op >> 8 & 7) as usize;
    let rd = arm.regs[rd_index];
    let result = rd.wrapping_sub(imm);
    set_zn(arm, result);
    sub_set_vc(arm, rd, imm);
    arm.regs[rd_index] = result;
''',

'add_hreg': '''
    let rs = arm.regs[(op >> 3 & 0xF) as usize];
    let rd_index = ((op & 0b111) | ((op & 0x80) >> 4)) as usize;
    arm.regs[rd_index] = arm.regs[rd_index].wrapping_add(rs);
''',
'cmp_hreg': '''
    let rs = arm.regs[(op >> 3 & 0xF) as usize];
    let rd = arm.regs[((op & 0b111) | ((op & 0x80) >> 4)) as usize];
    let result = rd.wrapping_sub(rs);
    set_zn(arm, result);
    sub_set_vc(arm, rd, rs);
''',
'mov_hreg': '''
    let rs = arm.regs[(op >> 3 & 0xF) as usize];
    let rd_index = ((op & 0b111) | ((op & 0x80) >> 4)) as usize;
    arm.regs[rd_index] = rs;
''',
'bx': '''
    let rs = arm.regs[(op >> 3 & 0xF) as usize];
    arm.branch_exchange(interconnect, rs);
''',

'ldr_pcrel_immoffset': '''
    let offset = ((op & 0xFF) << 2) as u32;
    let addr = (arm.regs[REG_PC] & !2) + offset;
    check_watchpoint!(arm, addr);
    arm.regs[(op >> 8 & 7) as usize] = interconnect.read32(addr);
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
''',

'str_sprel_immoffset': '''
    let rd = arm.regs[(op >> 8 & 7) as usize];
    let offset = ((op & 0xFF) << 2) as u32;
    let addr = arm.regs[REG_SP].wrapping_add(offset);
    check_watchpoint!(arm, addr);
    interconnect.write32(addr, rd);
''',
'ldr_sprel_immoffset': '''
    let offset = ((op & 0xFF) << 2) as u32;
    let addr = arm.regs[REG_SP].wrapping_add(offset);
    check_watchpoint!(arm, addr);
    arm.regs[(op >> 8 & 7) as usize] = interconnect.read32(addr);
    interconnect.add_internal_cycles(1); // internal cycle for address calculation
''',

'add_imm_pcrel': '''
    let imm = ((op & 0xF) << 2) as u32;
    arm.regs[(op >> 8 & 7) as usize] = arm.regs[REG_PC] + imm;
''',
'add_imm_sprel': '''
    let imm = ((op & 0xF) << 2) as u32;
    arm.regs[(op >> 8 & 7) as usize] = arm.regs[REG_SP] + imm;
''',

'add_sp_imm': '''
    let offset = ((op & 0x7F) << 2) as u32;
    if (op & 0x80) == 0 {
        arm.regs[REG_SP] += offset;
    } else {
        arm.regs[REG_SP] -= offset;
    }
''',

'push': '''
    let reglist = op & 0xFF;
    let store_lr = (op & 0x0100) != 0;
    let mut sp = arm.regs[REG_SP] - reglist.count_ones() * 4;
    if store_lr { sp -= 4; }
    arm.regs[REG_SP] = sp; // writeback

    for i in 0..8 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, sp);
            interconnect.write32(sp, arm.regs[i]);
            sp += 4;
        }
    }

    if store_lr {
        check_watchpoint!(arm, sp);
        interconnect.write32(sp, arm.regs[REG_LR]);
    }
''',
'pop': '''
    let reglist = op & 0xFF;
    let mut sp = arm.regs[REG_SP];
    let load_pc = (op & 0x0100) != 0;

    for i in 0..8 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, sp);
            arm.regs[i] = interconnect.read32(sp);
            sp += 4;
        }
    }

    if load_pc {
        check_watchpoint!(arm, sp);
        let addr = interconnect.read32(sp);
        sp += 4;
        arm.branch_to(interconnect, addr & !1);
    }

    arm.regs[REG_SP] = sp;
''',

'stmia': '''
    let rb_index = (op >> 8 & 7) as usize;
    let mut rb = arm.regs[rb_index];

    for i in 0..8 {
        if op & (1 << i) != 0 {
            check_watchpoint!(arm, rb);
            interconnect.write32(rb, arm.regs[i]);
            rb += 4;
        }
    }

    arm.regs[rb_index] = rb;
''',
'ldmia': '''
    let rb_index = (op >> 8 & 7) as usize;
    let mut rb = arm.regs[rb_index];

    for i in 0..8 {
        if op & (1 << i) != 0 {
            check_watchpoint!(arm, rb);
            arm.regs[i] = interconnect.read32(rb);
            rb += 4;
        }
    }

    arm.regs[rb_index] = rb;
''',

'swi': '''
    arm.signal_swi(interconnect);
''',

'b': '''
    let offset = sign_extend((op & 0x7FF) as u32, 11) << 1;
    let addr = arm.regs[REG_PC].wrapping_add(offset);
    arm.branch_to(interconnect, addr);
''',

'bl_hi': '''
    let hi_offset = sign_extend((op & 0x7FF) as u32, 11) << 12;
    arm.regs[REG_LR] = arm.regs[REG_PC].wrapping_add(hi_offset);
''',
'bl_lo': '''
    let lo_offset = ((op & 0x7FF) << 1) as u32;
    arm.regs[REG_LR] = arm.regs[REG_LR].wrapping_add(lo_offset);
    let temp = arm.regs[REG_PC];
    let target = arm.regs[REG_LR];
    arm.branch_to(interconnect, target);

    // LR contains address of instruction following this,
    // and has bit0 set to force thumb mode upon return.
    arm.regs[REG_LR] = (temp - 2) | 1;
''',

'undef': '''
    arm.signal_undef(interconnect);
'''
}

def decode(i):
    ins_name = None
    fn_body = None
    example = None

    if match(i, 0, 7):
        ins_name = 'lsl_imm'
        example = 'LSL Rd, Rs, #imm5'
    elif match(i, 8, 0xF):
        ins_name = 'lsr_imm'
        example = 'LSR Rd, Rs, #imm5'
    elif match(i, 0x10, 0x17):
        ins_name = 'asr_imm'
        example = 'ASR Rd, Rs, #imm5'

    elif match(i, 0x18, 0x19):
        ins_name = 'add3_reg'
        example = 'ADD Rd, Rs, Rn'
    elif match(i, 0x1A, 0x1B):
        ins_name = 'sub3_reg'
        example = 'SUB Rd, Rs, Rn'
    elif match(i, 0x1C, 0x1D):
        ins_name = 'add3_imm'
        example = 'ADD Rd, Rs, #imm3'
    elif match(i, 0x1E, 0x1F):
        ins_name = 'sub3_imm'
        example = 'SUB Rd, Rs, #imm3'

    elif match(i, 0x20, 0x27):
        ins_name = 'mov_imm'
        example = 'MOV Rd, #imm8'
    elif match(i, 0x28, 0x2F):
        ins_name = 'cmp_imm'
        example = 'CMP Rd, #imm8'
    elif match(i, 0x30, 0x37):
        ins_name = 'add_imm'
        example = 'ADD Rd, #imm8'
    elif match(i, 0x38, 0x3F):
        ins_name = 'sub_imm'
        example = 'SUB Rd, #imm8'

    elif match(i, 0x40, 0x43):
        opcode = i & 0xF
        (ins_name, example, fn_body) = ALU_OPS[opcode]
        fn_body = f'''
    let rd_index = (op & 7) as usize;
    let rd = arm.regs[rd_index];
    let rs = arm.regs[(op >> 3 & 7) as usize];
    {fn_body}
'''

    elif match(i, 0x44):
        ins_name = 'add_hreg'
        example = 'ADD Rhd, Rhs'
    elif match(i, 0x45):
        ins_name = 'cmp_hreg'
        example = 'CMP Rhd, Rhs'
    elif match(i, 0x46):
        ins_name = 'mov_hreg'
        example = 'MOV Rhd, Rhs'
    elif match(i, 0x47):
        ins_name = 'bx'
        example = 'BX Rh'

    elif match(i, 0x48, 0x4F):
        ins_name = 'ldr_pcrel_immoffset'
        example = 'LDR Rd, [PC, #imm8]'

    elif match(i, 0x50, 0x51):
        ins_name = 'str_regoffset'
        example = 'STR Rd, [Rb, Ro]'
        fn_body = STORE_OP_REG_OFFSET.format(T='u8', store_fn='write8')
    elif match(i, 0x52, 0x53):
        ins_name = 'strh_regoffset'
        example = 'STRH Rd, [Rb, Ro]'
        fn_body = STORE_OP_REG_OFFSET.format(T='u16', store_fn='write16')
    elif match(i, 0x54, 0x55):
        ins_name = 'strb_regoffset'
        example = 'STRB Rd, [Rb, Ro]'
        fn_body = STORE_OP_REG_OFFSET.format(T='u32', store_fn='write32')

    elif match(i, 0x56, 0x57):
        ins_name = 'ldsb_regoffset'
        example = 'LDSB Rd, [Rb, Ro]'
        fn_body = LOAD_OP_REG_OFFSET.format(T='i8', load_fn='read8')
    elif match(i, 0x58, 0x59):
        ins_name = 'ldr_regoffset'
        example = 'LDR Rd, [Rb, Ro]'
        fn_body = LOAD_OP_REG_OFFSET.format(T='u32', load_fn='read32')
    elif match(i, 0x5A, 0x5B):
        ins_name = 'ldrh_regoffset'
        example = 'LDRH Rd, [Rb, Ro]'
        fn_body = LOAD_OP_REG_OFFSET.format(T='u16', load_fn='read16')
    elif match(i, 0x5C, 0x5D):
        ins_name = 'ldrb_regoffset'
        example = 'LDRB Rd, [Rb, Ro]'
        fn_body = LOAD_OP_REG_OFFSET.format(T='u8', load_fn='read8')
    elif match(i, 0x5E, 0x5F):
        ins_name = 'ldsh_regoffset'
        example = 'LDSH Rd, [Rb, Ro]'
        fn_body = LOAD_OP_REG_OFFSET.format(T='i16', load_fn='read16')

    elif match(i, 0x60, 0x67):
        ins_name = 'str_immoffset'
        example = 'STR Rd, [RB, #imm5]'
        fn_body = STORE_OP_IMMED_OFFSET.format(T='u32', store_fn='write32')
    elif match(i, 0x70, 0x77):
        ins_name = 'strb_immoffset'
        example = 'STRB Rd, [RB, #imm5]'
        fn_body = STORE_OP_IMMED_OFFSET.format(T='u8', store_fn='write8')
    elif match(i, 0x80, 0x87):
        ins_name = 'strh_immoffset'
        example = 'STRH Rd, [RB, #imm5]'
        fn_body = STORE_OP_IMMED_OFFSET.format(T='u16', store_fn='write16')

    elif match(i, 0x68, 0x6F):
        ins_name = 'ldr_immoffset'
        example = 'LDR Rd, [RB, #imm5]'
        fn_body = LOAD_OP_IMMED_OFFSET.format(T='u32', load_fn='read32')
    elif match(i, 0x78, 0x7F):
        ins_name = 'ldrb_immoffset'
        example = 'LDRB Rd, [RB, #imm5]'
        fn_body = LOAD_OP_IMMED_OFFSET.format(T='u8', load_fn='read8')
    elif match(i, 0x88, 0x8F):
        ins_name = 'ldrh_immoffset'
        example = 'LDRH Rd, [RB, #imm5]'
        fn_body = LOAD_OP_IMMED_OFFSET.format(T='u16', load_fn='read16')

    elif match(i, 0x90, 0x97):
        ins_name = 'str_sprel_immoffset'
        example = 'STR Rd, [SP, #imm10]'
    elif match(i, 0x98, 0x9F):
        ins_name = 'ldr_sprel_immoffset'
        example = 'LDR Rd, [SP, #imm10]'

    elif match(i, 0xA0, 0xA7):
        ins_name = 'add_imm_pcrel'
        example = 'ADD Rd, PC, #imm10'
    elif match(i, 0xA8, 0xAF):
        ins_name = 'add_imm_sprel'
        example = 'ADD Rd, SP, #imm10'

    elif match(i, 0xB0, 0xB0):
        ins_name = 'add_sp_imm'
        example = 'ADD SP, {+-}#imm9'

    elif match(i, 0xB4, 0xB5):
        ins_name = 'push'
        example = 'PUSH { rlist, [LR] }'
    elif match(i, 0xBC, 0xBD):
        ins_name = 'pop'
        example = 'POP { rlist, [PC] }'

    elif match(i, 0xC0, 0xC7):
        ins_name = 'stmia'
        example = 'STMIA Rd!, { rlist }'
    elif match(i, 0xC8, 0xCF):
        ins_name = 'ldmia'
        example = 'LDMIA Rd!, { rlist }'

    elif match(i, 0xD0, 0xDD):
        code = (i >> 2) & 0xF
        cc = CONDITION_CODES[code]
        ins_name = 'b' + cc.lower()
        example = 'B{cc} #soffset9'.format(cc=cc.upper())
        fn_body = BRANCH_CC.format(cc=cc)

    elif match(i, 0xDF):
        ins_name = 'swi'
        example = 'SWI #imm8'

    elif match(i, 0xE0, 0xE7):
        ins_name = 'b'
        example = 'B #soffset12'

    elif match(i, 0xF0, 0xF7):
        ins_name = 'bl_hi'
        example = 'BL_hi #7FF___'
    elif match(i, 0xF8, 0xFF):
        ins_name = 'bl_lo'
        example = 'BL_lo #___FFF'

    if ins_name is None:
        ins_name = 'undef'
        example = 'und'
    if fn_body is None:
        fn_body = FUNCTIONS[ins_name]

    return (ins_name, example, fn_body)

FN_TEMPLATE = '''\
// {example}
fn op_{ins_name}(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent {{
{fn_body}

    StepEvent::None
}}
'''

def writelines(output):
    names = []
    examples = {}
    fn_bodies = {}
    for i in range(0, 1 << 10):
        (ins_name, example, fn_body) = decode(i)
        names.append('op_' + ins_name)
        examples[ins_name] = example
        if fn_bodies.get(ins_name):
            assert fn_bodies[ins_name] == fn_body
        fn_bodies[ins_name] = fn_body

    ary = ', '.join(names)
    output(f'static THUMB_LUT: [ThumbOp; 1024] = [{ary}];')
    for (ins_name, fn_body) in fn_bodies.items():
        example = examples[ins_name]
        output(FN_TEMPLATE.format(
            ins_name=ins_name,
            fn_body=fn_body.strip('\n'),
            example=example
        ))

def generate(filename):
    with open(filename, 'wt') as f:
        def write_fn(*args, **kwargs):
            print(*args, **kwargs, file=f)
        writelines(write_fn)
