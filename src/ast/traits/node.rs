use std::{any::Any, fmt::Debug};

pub trait Node: Any + Debug + Clone {
    fn as_any(&self) -> &dyn Any;
    fn token_literal(&self) -> String;
}
