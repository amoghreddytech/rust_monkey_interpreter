use std::any::Any;

pub trait Node {
    fn as_any(&self) -> &dyn Any;
}
