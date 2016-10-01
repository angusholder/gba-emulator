use num::NumCast;

pub trait MyInto<T> {
    fn my_into(self) -> T;
}

impl MyInto<bool> for u8 { fn my_into(self) -> bool { self != 0 } }
impl MyInto<bool> for u16 { fn my_into(self) -> bool { self != 0 } }
impl MyInto<bool> for u32 { fn my_into(self) -> bool { self != 0 } }

impl<T> MyInto<u8> for T where T: NumCast{ fn my_into(self) -> u8 { NumCast::from(self).unwrap() } }
impl<T> MyInto<u16> for T where T: NumCast { fn my_into(self) -> u16 { NumCast::from(self).unwrap() } }
impl<T> MyInto<u32> for T where T: NumCast { fn my_into(self) -> u32 { NumCast::from(self).unwrap() } }

macro_rules! bitfield_struct {
    (
        $($struct_name:ident: $underlying_type:ty {
            $(($start:expr, $width:expr) $field_name:ident: $field_type:ty,)+
        })+
    ) => { $(
        #[derive(Clone, Copy, Default, Debug)]
        struct $struct_name {
            $($field_name: $field_type,)+
        }

        impl From<$underlying_type> for $struct_name {
            fn from(n: $underlying_type) -> $struct_name {
                $struct_name {
                    $($field_name: (((1 << $width) - 1) & (n >> $start)).my_into()),+
                }
            }
        }

        impl From<$struct_name> for $underlying_type {
            fn from(s: $struct_name) -> $underlying_type {
                let mut n = 0;
                $(n |= (s.$field_name as $underlying_type) << $start;)*
                n
            }
        }
    )+ }
}
