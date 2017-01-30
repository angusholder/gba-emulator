macro_rules! unpack {
    (u8,    $n:expr) => { $n as u8 };
    (u16,   $n:expr) => { $n as u16 };
    (u32,   $n:expr) => { $n as u32 };
    (u64,   $n:expr) => { $n as u64 };
    (usize, $n:expr) => { $n as usize };

    (bool, $n:expr) => { $n != 0 };

    ($T:ident, $n:expr) => { Into::<$T>::into($n) };
}

macro_rules! to_primitive {
    ($packed_type:ty, u8,    $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u16,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u32,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u64,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, usize, $n:expr) => { $n as $packed_type };

    ($packed_type:ty, bool, $n:expr) => { $n as $packed_type };

    ($packed_type:ty, $T:ident, $n:expr) => { Into::<$packed_type>::into($n) };
}

// #[macro_export]
macro_rules! unpacked_bitfield_struct {
    { $(
        $(#[$attr:meta])*
        pub struct $unpacked_type:ident: $packed_type:ty {
            $( ($start:expr, $width:expr) $field_name:ident: $field_type:ident,)+
        }
    )+ } => { $(
        $(#[$attr])*
        pub struct $unpacked_type {
            $(pub $field_name: $field_type),+
        }

        impl From<$packed_type> for $unpacked_type {
            fn from(n: $packed_type) -> Self {
                $unpacked_type { $(
                    $field_name: unpack!(
                        $field_type,
                        ((1 << $width) - 1) & (n >> $start)
                    )
                ),+ }
            }
        }

        impl From<$unpacked_type> for $packed_type {
            fn from(s: $unpacked_type) -> Self {
                $(to_primitive!($packed_type, $field_type, s.$field_name) << $start)|+
            }
        }
    )+ };

    { $(
        $(#[$attr:meta])*
        struct $unpacked_type:ident: $packed_type:ty {
            $( ($start:expr, $width:expr) $field_name:ident: $field_type:ident,)+
        }
    )+ } => { $(
        $(#[$attr])*
        struct $unpacked_type {
            $(pub $field_name: $field_type),+
        }

        impl From<$packed_type> for $unpacked_type {
            fn from(n: $packed_type) -> Self {
                $unpacked_type { $(
                    $field_name: convert!(
                        $field_type,
                        ((1 << $width) - 1) & (n >> $start)
                    )
                ),+ }
            }
        }

        impl From<$unpacked_type> for $packed_type {
            fn from(s: $unpacked_type) -> Self {
                $((s.$field_name as $packed_type) << $start)|+
            }
        }
    )+ };
}
