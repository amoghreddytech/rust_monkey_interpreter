use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use anyhow::{Error, Result, anyhow};

use crate::parser::{IdentifierLiteral, Statement};

use super::builtin;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function(FuctionObject),
    String(String),
    Builtin(BuiltinFunction),
    Array(Vec<Box<Object>>),
    HashMap(HashMap<Object, Object>),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(i), Object::Integer(j)) => i == j,
            (Object::Boolean(i), Object::Boolean(j)) => i == j,
            (Object::String(i), Object::String(j)) => i == j,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            _ => (),
        }
    }
}

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, Error>;

impl Object {
    pub fn get_type(&self) -> &'static str {
        match self {
            Self::Integer(_) => "Integer",
            Self::Boolean(_) => "Boolean",
            Self::Null => "Null",
            Self::Return(_) => "Return",
            Self::Function(_) => "Function",
            Self::String(_) => "String",
            Self::Builtin(_) => "Builtin",
            Self::Array(_) => "Array",
            Self::HashMap(_) => "Hash",
        }
    }

    pub fn inspect(&self) -> String {
        return match self {
            Self::Integer(int_val) => int_val.to_string(),
            Self::Boolean(bool_val) => bool_val.to_string(),
            Self::Null => "It's Null why do you need this lol?".to_string(),
            Self::Return(inner) => inner.inspect(),
            Self::Function(fo) => {
                let params: String = fo
                    .parameters
                    .iter()
                    .map(|c| c.string_literal())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("fn({}) {{\n{}\n}}", params, fo.body.string_representation())
            }
            Self::String(s) => s.to_string(),
            Self::Builtin(_) => "builtin function".to_string(),
            Self::Array(a) => a
                .iter()
                .map(|ob| ob.inspect())
                .collect::<Vec<String>>()
                .join(" ,"),
            Self::HashMap(map) => map
                .iter()
                .map(|(key, value)| format!("{}:{}", key.inspect(), value.inspect()))
                .collect::<Vec<String>>()
                .join(" ,"),
        };
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            store: HashMap::new(),
            outer: None,
        };

        env.set("len", Object::Builtin(builtin::len));
        env.set("first", Object::Builtin(builtin::first));
        env.set("last", Object::Builtin(builtin::last));
        env.set("rest", Object::Builtin(builtin::rest));
        env.set("push", Object::Builtin(builtin::push));
        env
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(obj) = self.store.get(&name.to_string()) {
            return Some(obj.clone());
        }

        if let Some(outer) = &self.outer {
            match outer.try_borrow() {
                Ok(env) => Some(env.get(&name.to_string())?),
                Err(e) => None,
            }
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.store.insert(name.to_string(), value);
    }
}

#[derive(Debug, Clone)]
pub struct FuctionObject {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: Box<Statement>,
    pub env: Rc<RefCell<Environment>>,
}

impl FuctionObject {
    pub fn new(
        parameters: Vec<IdentifierLiteral>,
        body: Box<Statement>,
        env: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}
