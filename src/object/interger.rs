use crate::object::traits::{Object, ObjectType};

const INTEGER_OBJ: &'static str = "INTEGER";

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl Object for Integer {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn get_type(&self) -> ObjectType {
        INTEGER_OBJ
    }
}
