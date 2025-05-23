use crate::object::traits::{Object, ObjectType};

const BOOLEAN_OBJ: &'static str = "BOOLEAN";

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl Object for Boolean {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn get_type(&self) -> ObjectType {
        BOOLEAN_OBJ
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}
