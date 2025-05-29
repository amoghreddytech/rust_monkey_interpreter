use dyn_clone::clone_trait_object;

use super::Object;

const RETURN_OBJ: &'static str = "RETURN_OBJECT";

clone_trait_object!(Object);

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Box<dyn Object>,
}

impl Return {
    pub fn new(value: Box<dyn Object>) -> Self {
        Self { value }
    }
}

impl Object for Return {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn get_type(&self) -> super::traits::ObjectType {
        RETURN_OBJ
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}
