#[macro_export]
macro_rules! __layout_offset {
    (@each $heap:ident $addr:ident => $field:ident: Word) => { { $addr += 1; } };
    (@each $heap:ident $addr:ident => $field:ident: Raw) => { { $addr += 1; } };
    (@each $heap:ident $addr:ident => $field:ident: [Word]) => { {
            $addr += $heap.words().get($addr).unwrap().raw().unwrap() as usize;
    } };
    (@each $heap:ident $addr:ident => $field:ident: [Raw]) => { {
            $addr += $heap.words().get($addr).unwrap().raw().unwrap() as usize;
    } };
    ($heap:ident, $addr:ident => $($prev_field:ident: $prev_type:tt)*) => {
        {
            #[allow(unused_mut)]
            let mut addr = $addr;
            $(__layout_offset!(@each $heap addr => $prev_field: $prev_type);)*
            addr
        }
    };
}

#[macro_export]
macro_rules! __layout_access {
    ([$($suffix:tt)*] $heap:ident, $addr:ident => $t:ident) => {{
        interpolate_idents! {
            $heap
                .[ words $($suffix)* ]()
                .[ get $($suffix)* ]($addr)
                .unwrap()
        }
    }};
    ([$($suffix:tt)*] $heap:ident, $addr:ident => [$t:ident]) => {{
        let len = $heap.words().get($addr).unwrap().raw().unwrap() as usize;
        interpolate_idents! {
            $heap
                .[ words $($suffix)* ]()
                .[ get $($suffix)* ]($addr + 1..$addr + 1 + len)
                .unwrap()
        }
    }};
}

#[macro_export]
macro_rules! __layout_needs_header {
    ($p:tt if Word $($rest:tt)*) => { __layout_needs_header!($p if $($rest)*) };
    ($p:tt if Raw $($rest:tt)*) => { $p };
    ($p:tt if [Word] $($rest:tt)*) => { $p };
    ($p:tt if [Raw] $($rest:tt)*) => { $p };
    ($p:tt if) => {};
}

#[macro_export]
macro_rules! __layout_type {
    ($var:ident => Word) => {
        $var
    };
    ($var:ident => Raw) => {
        $var
    };
    ($var:ident => [Word]) => {
        [$var]
    };
    ($var:ident => [Raw]) => {
        [$var]
    };
}

#[macro_export]
macro_rules! __layout_field {
    ($name:ident { $($prev_field:ident: $prev_type:tt)* => $field:ident : $type:tt }) => {
        impl<'a, W: Word> $name<'a, W> {
            pub fn $field(&'a self) -> &'a __layout_type!(W => $type) {
                let heap = &*self.heap;
                let addr = self.offset;
                __layout_offset!(heap, addr => $($prev_field : $prev_type)*);
                __layout_access!([] heap, addr => $type)
            }

            interpolate_idents! {
                pub fn [ $field _mut ] (&'a mut self) -> &'a mut __layout_type!(W => $type) {
                    let heap = &mut *self.heap;
                    let addr = self.offset;
                    __layout_offset!(heap, addr => $($prev_field : $prev_type)*);
                    __layout_access!([[_mut]] heap, addr => $type)
                }
            }
        }
    };
}

#[macro_export]
macro_rules! __layout_folded_fields {
    ($name:ident $($fold:tt)*) => {
        $(__layout_field!($name $fold);)*
    };
}

#[macro_export]
macro_rules! __layout_fold {
    (@rec $k:tt [$($prev_field:ident : $prev_type:tt)*] [$($agg:tt)*] $field:ident : $type:tt $($rest:tt)*) => {
        __layout_fold!(@rec
            $k
            [$($prev_field: $prev_type)* $field: $type]
            [$($agg)* { $($prev_field : $prev_type)* => $field : $type }]
            $($rest)*
        );
    };
    (@rec ($k:ident!($($kargs:tt)*)) [$($field:ident : $type:tt)*] [$($agg:tt)*]) => {
        $k!($($kargs)* $($agg)*);
    };
    ($k:ident!($($kargs:tt)*) $($field:ident : $type:tt)*) => { __layout_fold!(@rec ($k!($($kargs)*)) [] [] $($field : $type)*); };
}

#[macro_export]
macro_rules! __layout_struct {
    (struct $name:tt { $($field:ident : $type:tt)* }) => {
        pub struct $name<'a, W: Word> {
            offset: usize,
            heap: &'a mut Heap<W>,
        }
        
        __layout_fold!(__layout_folded_fields!($name) $($field : $type)*);

        impl<'a, W: Word> $name<'a, W> {
            pub fn as_slice(&'a self) -> &'a [W] {
                let heap = &*self.heap;
                let start = self.offset;
                let end = __layout_offset!(heap, start => $($field : $type)*);
                heap.words().get(start..end).unwrap()
            }

            pub fn as_mut_slice(&'a mut self) -> &'a mut [W] {
                let heap = &mut *self.heap;
                let start = self.offset;
                let end = __layout_offset!(heap, start => $($field : $type)*);
                heap.words_mut().get_mut(start..end).unwrap()
            }
        }
    };
}

#[macro_export]
macro_rules! __layout_def {
    (type Tag = $tag:ident; where { $(struct $name:ident $innards:tt)* }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        pub enum $tag {
            $($name,)*
        }
    };
}

#[macro_export]
macro_rules! __layout_splat_defs {
    ($(type $foo:ident = $bar:ident;)* where $stuff:tt) => {
        $(__layout_def!(type $foo = $bar; where $stuff);)*
    };
}

#[macro_export]
macro_rules! layout {
    ($(type $foo:ident = $bar:ident;)* where { $(struct $name:ident { $($field:ident : $type:tt),* $(,)* })+ }) => {
        __layout_splat_defs!($(type $foo = $bar;)* where { $(struct $name { $($field : $type)* })* });
        $(__layout_struct!(struct $name { $($field : $type)* });)*
    };
}
