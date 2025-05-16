use crate::ast::traits::Node;
use std::fmt::Debug;

pub trait Expression: Debug + Node {
    fn string_representation(&self) -> String;

    fn as_node(&self) -> &dyn Node;
}
