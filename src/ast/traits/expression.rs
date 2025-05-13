use crate::ast::traits::Node;
use std::any::Any;
use std::fmt::Debug;

pub trait Expression: Debug + Any + Node {
    fn string_representation(&self) -> String;
}
