use std::{any::Any, fmt::Debug};

pub trait Node: Any + Debug {
    fn as_any(&self) -> &dyn Any;
}
