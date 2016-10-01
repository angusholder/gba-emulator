#[repr(C, packed)]
struct GBAHeader {
    entry_point: u32,
    nintendo_logo: [u8; 156],
    game_title: [u8; 12],
    game_code: [u8; 4],
    maker_code: [u8; 2],
    fixed_value: u8, // 0x96
    main_unit_code: u8,
    device_type: u8,
    reserved_area: [u8; 7],
    software_version: u8,
    complement_check: u8,
    reserved_area2: [u8; 2],

    ram_entry_point: u32,
    boot_mode: u8,
    slave_id_number: u8,
    unused: [u8; 26],
    joybus_entry_point: u32,
}
