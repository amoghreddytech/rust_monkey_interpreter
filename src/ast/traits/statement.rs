use std::any::Any;
use std::fmt::Debug;

pub trait Statement: Debug + Any {
    fn string_representation(&self) -> String;

    fn as_any(&self) -> &dyn Any;

    fn token_literal(&self) -> String;
}
