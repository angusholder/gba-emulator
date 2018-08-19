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

def decode(i):
    ins_name = None
    fn_body = None

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
