macro_rules! ref_impl {
    (impl<T: $traitb: ident $(+ $bound:ident)*> $traiti:ident for &T {
        $($body:tt)*
    }) => {
        impl<'a, T: $traitb $(+ $bound)*> $traiti for &'a T {
            $($body)*
        }

        impl<'a, T: $traitb $(+ $bound)*> $traiti for &'a mut T {
            $($body)*
        }

        impl<T: $traitb $(+ $bound)*> $traiti for ::std::boxed::Box<T> {
            $($body)*
        }

        impl<T: $traitb $(+ $bound)*> $traiti for ::std::rc::Rc<T> {
            $($body)*
        }
    };
}
