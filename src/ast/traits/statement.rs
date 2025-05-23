use crate::ast::traits::Node;
use std::fmt::Debug;

pub trait Statement: Debug + Node {
    fn string_representation(&self) -> String;

    fn token_literal(&self) -> String;

    fn as_node(&self) -> &dyn Node;
}
