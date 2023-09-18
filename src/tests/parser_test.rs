use crate::ast_nodes;
use crate::Parser;
use ast_nodes::*;
use insta::*;
use std::*;

macro_rules! test_func {
    ($func:expr,($($name:ident : $input:expr)*)) => {
        $(
        #[test]
        fn $name () {
            // Perform your desired operation using the input parameter
            assert_debug_snapshot!($func($input));
        }
        )*
    };
}
