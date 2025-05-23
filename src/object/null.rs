use crate::object::traits::{Object, ObjectType};

const NULL_OBJ: &'static str = "NULL";

#[derive(Debug, Clone)]
pub struct Null {}

impl Null {
    pub fn new() -> Self {
        Self {}
    }
}

impl Object for Null {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn get_type(&self) -> ObjectType {
        NULL_OBJ
    }
}
