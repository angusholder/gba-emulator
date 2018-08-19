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
