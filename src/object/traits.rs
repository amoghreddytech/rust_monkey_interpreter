use std::{any::Any, fmt::Debug};

pub type ObjectType = &'static str;

pub trait Object: Any + Debug {
    fn get_type(&self) -> ObjectType;

    fn inspect(&self) -> String;

    fn as_any(&self) -> &dyn Any;
}
