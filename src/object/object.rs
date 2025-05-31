#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Self::Integer(_) => "Integer".to_string(),
            Self::Boolean(_) => "Boolean".to_string(),
            Self::Null => "Null".to_string(),
            Self::Return(_) => "RETURN".to_string(),
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
