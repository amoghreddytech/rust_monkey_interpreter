use crate::ast::traits::Node;
use std::any::Any;
use std::fmt::Debug;

pub trait Statement: Debug + Any + Node {
    fn string_representation(&self) -> String;

    fn token_literal(&self) -> String;
}
