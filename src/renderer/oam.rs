use num::FromPrimitive;

#[derive(Clone, Copy, Default)]
pub struct ObjTransform {
    pub pa: u16,
    pub pb: u16,
    pub pc: u16,
    pub pd: u16,
}

enum_from_primitive! {
#[derive(Clone, Copy)]
enum ObjMode {
    Normal = 0,
    SemiTransparent = 1,
    ObjWindow = 2,
}
}

impl Default for ObjMode {
    fn default() -> ObjMode {
        ObjMode::Normal
    }
}

#[derive(Clone, Copy)]
struct ObjDimensions(u8);

impl Default for ObjDimensions {
    fn default() -> ObjDimensions {
        ObjDimensions(0)
    }
}

impl ObjDimensions {
    fn new(size: u8, shape: u8) -> ObjDimensions {
        assert!(size <= 3);
        assert!(shape <= 2);

        let mut this = ObjDimensions(0);
        this.set_size(size);
        this.set_shape(shape);
        this
    }

    fn get(&self) -> (u8, u8) {
        static OBJ_SHAPES: [(u8, u8); 12] = [
            // Square
            ( 8, 8),
            (16,16),
            (32,32),
            (64,64),

            // Horizontal
            (16, 8),
            (32, 8),
            (32,16),
            (64,32),

            // Vertical
            ( 8,16),
            ( 8,32),
            (16,32),
            (32,64),
        ];

        OBJ_SHAPES[self.0 as usize]
    }

    fn get_shape(&self) -> u8 {
        self.0 >> 2
    }
    fn set_shape(&mut self, shape: u8) {
        self.0 = (self.0 & 3) | (shape << 2);
    }

    fn get_size(&self) -> u8 {
        self.0 & 3
    }
    fn set_size(&mut self, size: u8) {
        self.0 = (self.0 & 0xC) | size;
    }
}

#[derive(Clone, Copy, Default)]
pub struct ObjAttributes {
    y_coord: u8,
    x_coord: u16,
    mosaic: bool,
    mode: ObjMode,
    chr_name: u16,
    priority: u8,

    // If use_palette_index, this chooses 1 of 16 contiguous blocks in palette
    // memory, and then pixels of tiles are 4-bit indices into the block.
    // Otherwise, the pixels of the tile are 8-bit indices into palette memory.
    palette_index: u8,
    use_palette_index: bool,

    rot_scale_flag: bool,

    // if !rot_scale_flag
    disable: bool,
    hflip: bool,
    vflip: bool,

    // if rot_scale_flag
    double_size: bool,
    transform_index: u8,

    dimensions: ObjDimensions,
}

impl ObjAttributes {
    pub fn get_reg0(&self) -> u16 {
        ObjAttribute0Reg {
            y_coord: self.y_coord,
            mode: self.mode as u8,
            mosaic: self.mosaic,
            single_palette_mode: self.use_palette_index,
            shape: self.dimensions.get_shape(),
            disable: self.disable,
            double_size: self.double_size,
            rot_scale_flag: self.rot_scale_flag,
        }.into()
    }

    pub fn set_reg0(&mut self, reg_value: u16) {
        let reg = ObjAttribute0Reg::from(reg_value);

        self.y_coord = reg.y_coord;
        self.mode = ObjMode::from_u8(reg.mode).unwrap();
        self.mosaic = reg.mosaic;
        self.use_palette_index = reg.single_palette_mode;
        self.dimensions.set_shape(reg.shape);
        self.disable = reg.disable;
        self.double_size = reg.double_size;
    }

    pub fn get_reg1(&self) -> u16 {
        ObjAttribute1Reg {
            x_coord: self.x_coord,
            obj_size: self.dimensions.get_size(),
            hflip: self.hflip,
            vflip: self.vflip,
            transform_index: self.transform_index,
        }.into()
    }

    pub fn set_reg1(&mut self, reg_value: u16) {
        let reg = ObjAttribute1Reg::from(reg_value);

        self.x_coord = reg.x_coord;
        self.dimensions.set_size(reg.obj_size);
        self.hflip = reg.hflip;
        self.vflip = reg.vflip;
        self.transform_index = reg.transform_index;
    }

    pub fn get_reg2(&self) -> u16 {
        ObjAttribute2Reg {
            chr_name: self.chr_name,
            priority: self.priority,
            palette_index: self.palette_index,
        }.into()
    }

    pub fn set_reg2(&mut self, reg_value: u16) {
        let reg = ObjAttribute2Reg::from(reg_value);

        self.chr_name = reg.chr_name;
        self.priority = reg.priority;
        self.palette_index = reg.palette_index;
    }
}

unpacked_bitfield_struct! {
#[derive(Default)]
struct ObjAttribute0Reg: u16 {
    (0,8) y_coord: u8, // (0-255)
    (8,1) rot_scale_flag: bool,

    // if rot_scale_flag:
        (9,1) double_size: bool,

    // else:
        (9,1) disable: bool,

    (10,2) mode: u8, // (0=Normal, 1=Semi-Transparent, 2=OBJ Window, 3=Prohibited)
    (12,1) mosaic: bool,
    (13,1) single_palette_mode: bool, // (0=16/16, 1=256/1)
    (14,2) shape: u8, // (0=Square,1=Horizontal,2=Vertical,3=Prohibited)
}

#[derive(Default)]
struct ObjAttribute1Reg: u16 {
    (0,9) x_coord: u16, // (0-511)

    // if rot_scale_flag:
        (9,5) transform_index: u8, // (0-31)
    // else:
        // 9-11  Not used
        (12,1) hflip: bool,
        (13,1) vflip: bool,

    (14,2) obj_size: u8, // (0..3, depends on OBJ Shape, see Attr 0)
         // Size  Square   Horizontal  Vertical
         // 0     8x8      16x8        8x16
         // 1     16x16    32x8        8x32
         // 2     32x32    32x16       16x32
         // 3     64x64    64x32       32x64
}

struct ObjAttribute2Reg: u16 {
    (0,10) chr_name: u16, // (0-1023=Tile Number)
    (10,2) priority: u8, // (0-3; 0=Highest)
    (12,4) palette_index: u8, // (0-15) (Not used in 256 color/1 palette mode)
}
}