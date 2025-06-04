use anyhow::{Error, anyhow};

use crate::parser::{
    AbstractSyntaxTree, BlockStatement, BooleanLiteral, Expression, IfLiteral, InfixLiteral,
    IntegerLiteral, PrefixLiteral, Statement,
};

use super::Object;

#[derive(Debug, Clone)]
pub enum Node<'a> {
    StatementNode(&'a Statement),
    ExpressionNode(&'a Expression),
    ProgramNode(&'a AbstractSyntaxTree),
}

pub fn eval(node: Node<'_>) -> Result<Object, Error> {
    match node {
        Node::ExpressionNode(expr) => eval_expression(expr),
        Node::StatementNode(stmt) => eval_statement(stmt),
        Node::ProgramNode(abstract_syntax_tree) => eval_program(abstract_syntax_tree),
    }
}

fn eval_program(ast: &AbstractSyntaxTree) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in &ast.statements {
        result = eval(Node::StatementNode(stmt))?;
    }

    Ok(result)
}

fn eval_block_statement(block_statment: &BlockStatement) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in block_statment.statements.iter() {
        let statement: &Statement = &*stmt;
        result = eval(Node::StatementNode(stmt))?;
    }

    Ok(result)
}

fn eval_expression(expr: &Expression) -> Result<Object, Error> {
    let object = match expr {
        Expression::IdentiferExpression(identifier_literal) => todo!(),
        Expression::IntegerExpression(ie) => evaluate_integer_literal(ie)?,
        Expression::PrefixExpression(pl) => evaluate_prefix_literal(pl)?,
        Expression::InfixExpression(il) => evaluate_infix_literal(il)?,
        Expression::BooleanExpression(be) => evaluate_boolean_literal(be)?,
        Expression::IfExpression(ie) => eval_if_literal(ie)?,
        Expression::FunctionExpression(function_literal) => todo!(),
        Expression::CallExpression(call_literal) => todo!(),
    };

    Ok(object)
}

fn eval_statement(stmt: &Statement) -> Result<Object, Error> {
    let object = match stmt {
        Statement::LetStatement(let_statement) => todo!(),
        Statement::ReturnStatement(return_statement) => todo!(),
        Statement::ExpressionStatement(es) => eval(Node::ExpressionNode(&*es.expression))?,
        Statement::BlockStatement(bs) => eval_block_statement(bs)?,
    };

    Ok(object)
}

fn evaluate_integer_literal(ie: &IntegerLiteral) -> Result<Object, Error> {
    Ok(Object::Integer(ie.get_value()))
}

fn evaluate_boolean_literal(be: &BooleanLiteral) -> Result<Object, Error> {
    Ok(Object::Boolean(be.get_value()))
}

fn evaluate_prefix_literal(prefix_literal: &PrefixLiteral) -> Result<Object, Error> {
    let expression: &Expression = &*prefix_literal.right;
    let expression_node = Node::ExpressionNode(expression);
    let right_object = eval(expression_node)?;

    match prefix_literal.operator.as_str() {
        "!" => match right_object {
            Object::Boolean(b) => Ok(Object::Boolean(!b)),
            _ => Err(anyhow!("'!' operator requires a boolean")),
        },
        "_" => match right_object {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Err(anyhow!("'-' operator requires a integer")),
        },
        _ => Err(anyhow!("unknown prefix operator")),
    }
}

fn evaluate_infix_literal(infix_literal: &InfixLiteral) -> Result<Object, Error> {
    let left_expression_node = Node::ExpressionNode(&*infix_literal.left);
    let right_expression_node = Node::ExpressionNode(&*infix_literal.right);

    let left_object = eval(left_expression_node)?;
    let right_object = eval(right_expression_node)?;

    match (&left_object, infix_literal.operator.as_str(), &right_object) {
        (Object::Integer(l), "+", Object::Integer(r)) => Ok(Object::Integer(l + r)),
        (Object::Integer(l), "-", Object::Integer(r)) => Ok(Object::Integer(l - r)),
        (Object::Integer(l), "*", Object::Integer(r)) => Ok(Object::Integer(l * r)),
        (Object::Integer(l), "/", Object::Integer(r)) => Ok(Object::Integer(l / r)),
        (Object::Integer(l), "<", Object::Integer(r)) => Ok(Object::Boolean(l < r)),
        (Object::Integer(l), ">", Object::Integer(r)) => Ok(Object::Boolean(l > r)),
        (Object::Integer(l), "==", Object::Integer(r)) => Ok(Object::Boolean(l == r)),
        (Object::Integer(l), "!=", Object::Integer(r)) => Ok(Object::Boolean(l != r)),
        (Object::Boolean(l), "==", Object::Boolean(r)) => Ok(Object::Boolean(l == r)),
        (Object::Boolean(l), "!=", Object::Boolean(r)) => Ok(Object::Boolean(l != r)),

        _ => Err(anyhow!("type mismatch in infix expression")),
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        _ => false,
    }
}

fn eval_if_literal(if_literal: &IfLiteral) -> Result<Object, Error> {
    let condition_expression: &Expression = &*if_literal.condition;
    let condition: Object = eval(Node::ExpressionNode(condition_expression))?;
    if is_truthy(&condition) {
        let consequence_statement: &Statement = &*if_literal.consequence;
        return eval(Node::StatementNode(consequence_statement));
    } else if let Some(ref alternative) = if_literal.alternative {
        let alternative_statement: &Statement = &*alternative;
        return eval(Node::StatementNode(alternative_statement));
    } else {
        return Ok(Object::Null);
    }
}
