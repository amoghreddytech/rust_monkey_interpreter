use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl Object {
    pub fn get_type(&self) -> &'static str {
        match self {
            Self::Integer(_) => "Integer",
            Self::Boolean(_) => "Boolean",
            Self::Null => "Null",
            Self::Return(_) => "RETURN",
        }
    }

    pub fn inspect(self) -> String {
        return match self {
            Self::Integer(int_val) => int_val.to_string(),
            Self::Boolean(bool_val) => bool_val.to_string(),
            Self::Null => "It's Null why do you need this lol?".to_string(),
            Self::Return(inner) => inner.inspect(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, val: Object) -> &Object {
        self.store.insert(name.clone(), val);
        self.store.get(&name).unwrap()
    }
}
