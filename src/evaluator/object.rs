use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

use anyhow::anyhow;

use crate::parser::{IdentifierLiteral, Statement};

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function(FuctionObject),
}

impl Object {
    pub fn get_type(&self) -> &'static str {
        match self {
            Self::Integer(_) => "Integer",
            Self::Boolean(_) => "Boolean",
            Self::Null => "Null",
            Self::Return(_) => "Return",
            Self::Function(_) => "Function",
        }
    }

    pub fn inspect(self) -> String {
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
        Self {
            store: HashMap::new(),
            outer: None,
        }
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
