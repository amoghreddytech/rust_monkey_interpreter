#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::{ast::statements::BlockStatement, object::object::Object};
use anyhow::{Error, Result};

use super::evaluator::evaluate;

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    val: i64,
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral {
    val: bool,
}

#[derive(Debug, Clone)]
pub struct PrefixLiteral {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixLiteral {
    pub operator: String,
    pub right: Box<Expression>,
    pub left: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfLiteral {
    condition: Box<Expression>,
    consequence: Block,
    alternative: Option<Block>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerExpression(IntegerLiteral),
    BooleanExpression(BooleanLiteral),
    PrefixExpression(PrefixLiteral),
    InfixExpression(InfixLiteral),
    IfExpression(IfLiteral),
}

impl Expression {
    fn evaluate(self) -> Result<Object, Error> {
        match self {
            // Integer Expression
            // Like the leaf it stores a value of the integer
            Self::IntegerExpression(int_literal) => {
                let inner_i64_representation = int_literal.val;
                Ok(Object::Integer(inner_i64_representation))
            }
            // Boolean expression
            // Same leaf concept I think
            Self::BooleanExpression(bool_literal) => Ok(Object::Boolean(bool_literal.val)),

            // Prefix Expression
            // It has like the operator component that can be ! and a right with can be a expression technically
            Self::PrefixExpression(prefix_literal) => {
                let right = prefix_literal.right.evaluate()?;
                eval_prefix_expression(&prefix_literal.operator, right)
            }

            Self::InfixExpression(infix_literal) => {
                let left = infix_literal.left.evaluate()?;
                let right = infix_literal.right.evaluate()?;
                eval_infix_expression(&infix_literal.operator, left, right)
            }

            Self::IfExpression(ie) => {
                let condition = ie.condition.evaluate()?;
                if is_truthy(&condition) {
                    eval_block_statement(ie.consequence)
                } else if let Some(alternative) = ie.alternative {
                    eval_block_statement(alternative)
                } else {
                    Err(Error::msg(format!(
                        "If expression failed : condition -> {:?} consequence -> {:?} alternative -> {:?}",
                        condition,
                        ie.consequence,
                        ie.alternative.unwrap()
                    )))
                }
            }
        }
    }
}

impl PrefixLiteral {
    pub fn new(operator: String, right: Box<Expression>) -> Self {
        Self { operator, right }
    }
}

fn eval_bang_operator(right: Object) -> Result<Object, Error> {
    match right {
        Object::Boolean(true) => Ok(Object::Boolean(false)),
        Object::Boolean(false) => Ok(Object::Boolean(true)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn eval_minus_operator(right: Object) -> Result<Object, Error> {
    match right {
        Object::Integer(value) => Ok(Object::Integer(-value)),
        _ => Err(Error::msg(format!(
            "unknown operator : -{}",
            right.get_type()
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, Error> {
    match operator {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_operator(right),
        _ => Err(Error::msg(format!(
            "unknown operator : {}{}",
            operator,
            right.get_type()
        ))),
    }
}

impl InfixLiteral {
    pub fn new(operator: String, left: Box<Expression>, right: Box<Expression>) -> Self {
        Self {
            operator,
            left,
            right,
        }
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, Error> {
    match (left, right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => match operator {
            "+" => Ok(Object::Integer(left_val + right_val)),
            "-" => Ok(Object::Integer(left_val - right_val)),
            "*" => Ok(Object::Integer(left_val * right_val)),
            "/" => Ok(Object::Integer(left_val / right_val)),
            "<" => Ok(Object::Boolean(left_val < right_val)),
            ">" => Ok(Object::Boolean(left_val > right_val)),
            "==" => Ok(Object::Boolean(left_val == right_val)),
            "!=" => Ok(Object::Boolean(left_val != right_val)),

            _ => Err(Error::msg(format!(
                "unkown operator : INTEGER {} INTEGER",
                operator,
            ))),
        },

        (Object::Boolean(left_val), Object::Boolean(right_val)) => match operator {
            "==" => Ok(Object::Boolean(left_val == right_val)),
            "!=" => Ok(Object::Boolean(left_val != right_val)),
            _ => Err(Error::msg(format!(
                "unkown operator : BOOLEAN {} BOOLEAN",
                operator,
            ))),
        },
        (l, r) => {
            let left_type = l.get_type();
            let right_type = r.get_type();
            Err(Error::msg(format!(
                "unkown operator :  {} {} {}",
                left_type, operator, right_type,
            )))
        }
    }
}

impl IfLiteral {
    pub fn new(condition: Box<Expression>, consequence: Block, alternative: Option<Block>) -> Self {
        Self {
            condition,
            consequence,
            alternative,
        }
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(false) => false,
        Object::Boolean(true) => true,
        Object::Null => false,
        _ => true,
    }
}

// This struct Holds all the statemetns
#[derive(Debug, Clone)]
struct Program {
    statements: Vec<Statement>,
}

impl Program {
    fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

// Statemet
// the Program is also a statemetn
// maybe I can change that, keep Program a struch
// and then after that I can do something with the steatemets.

#[derive(Debug, Clone)]
struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
enum Statement {
    ProgramStatemet(Program),
    ExpressionStatement(Expression),
    BlockStatement(Block),
    ReturnStatement(Expression),
}

fn eval_program(program: Program) -> Result<Object, Error> {
    let mut result = Object::Null;
    for stmt in program.statements {
        result = stmt.evaluate()?;
        if let Object::Return(boxed_val) = result {
            return Ok(*boxed_val);
        }
    }
    Ok(result)
}

fn eval_block_statement(block: Block) -> Result<Object, Error> {
    let mut result = Object::Null;
    for stmt in block.statements {
        result = stmt.evaluate()?;

        if let Object::Return(_) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

impl Statement {
    fn evaluate(self) -> Result<Object, Error> {
        match self {
            // Program basically has to evaluate all the staments
            Self::ProgramStatemet(program) => eval_program(program),
            // Expression Statemnt consists of one expression so
            Self::ExpressionStatement(expr) => expr.evaluate(),
            Self::BlockStatement(block) => eval_block_statement(block),
            Self::ReturnStatement(expr) => {
                let val = expr.evaluate()?;
                Ok(Object::Return(Box::new(val)))
            }
        }
    }
}

enum Node {
    Expr(Expression),
    Stmt(Statement),
}

fn eval(node: Node) -> Result<Object, Error> {
    match node {
        Node::Expr(expression) => expression.evaluate(),
        Node::Stmt(statement) => statement.evaluate(),
    }
}

// #[cfg(test)]
// mod test {
//     use super::*;
//     use crate::ast::ast::Program;
//     use crate::lexer::lexer::Lexer;
//     use crate::parser::parser::Parser;

//     fn test_eval(input: &str) -> Object {
//         let lexer = Lexer::new(input.to_string());
//         let parser = Parser::new(lexer);
//         let mut prog = Program::new(parser);
//         prog.parse_program();

//         let program = Node::Stmt(Statement::ProgramStatemet(prog));
//     }
// }
