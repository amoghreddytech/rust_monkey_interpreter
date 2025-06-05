use anyhow::{Error, Result, anyhow};

use crate::parser::{
    AbstractSyntaxTree, BlockStatement, BooleanLiteral, Expression, ExpressionStatement,
    IdentifierLiteral, IfLiteral, InfixLiteral, IntegerLiteral, LetStatement, PrefixLiteral,
    ReturnStatement, Statement,
};

use super::{Object, object::Environment};

#[derive(Debug, Clone)]
pub enum Node<'a> {
    StatementNode(&'a Statement),
    ExpressionNode(&'a Expression),
    ProgramNode(&'a AbstractSyntaxTree),
}

pub fn eval(node: Node<'_>, env: &mut Environment) -> Result<Object, Error> {
    match node {
        Node::ExpressionNode(expr) => eval_expression(expr, env),
        Node::StatementNode(stmt) => eval_statement(stmt, env),
        Node::ProgramNode(abstract_syntax_tree) => eval_program(abstract_syntax_tree, env),
    }
}

fn eval_program(ast: &AbstractSyntaxTree, env: &mut Environment) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in &ast.statements {
        result = eval(Node::StatementNode(stmt), env)?;
        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}

fn eval_block_statement(
    block_statment: &BlockStatement,
    env: &mut Environment,
) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in block_statment.statements.iter() {
        let statement: &Statement = &*stmt;
        result = eval(Node::StatementNode(stmt), env)?;

        if matches!(result, Object::Return(_)) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_let_statement(
    let_statement: &LetStatement,
    env: &mut Environment,
) -> Result<Object, Error> {
    let value = eval(Node::ExpressionNode((&let_statement.value)), env)?;
    env.set(let_statement.identifier.string_literal(), value.clone());
    Ok(value)
}

fn eval_return_statement(
    return_statement: &ReturnStatement,
    env: &mut Environment,
) -> Result<Object, Error> {
    let value = eval(Node::ExpressionNode(&return_statement.return_value), env)?;
    Ok(Object::Return(Box::new(value)))
}

fn eval_expression_statement(
    expression_statment: &ExpressionStatement,
    env: &mut Environment,
) -> Result<Object, Error> {
    eval(Node::ExpressionNode(&*expression_statment.expression), env)
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> Result<Object, Error> {
    let object = match expr {
        Expression::IdentiferExpression(ie) => evaluate_identifier_literal(ie, env)?,
        Expression::IntegerExpression(ie) => evaluate_integer_literal(ie, env)?,
        Expression::PrefixExpression(pl) => evaluate_prefix_literal(pl, env)?,
        Expression::InfixExpression(il) => evaluate_infix_literal(il, env)?,
        Expression::BooleanExpression(be) => evaluate_boolean_literal(be, env)?,
        Expression::IfExpression(ie) => eval_if_literal(ie, env)?,
        Expression::FunctionExpression(function_literal) => todo!(),
        Expression::CallExpression(call_literal) => todo!(),
    };

    Ok(object)
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Result<Object, Error> {
    let object = match stmt {
        Statement::LetStatement(ls) => eval_let_statement(ls, env)?,
        Statement::ReturnStatement(rs) => eval_return_statement(rs, env)?,
        Statement::ExpressionStatement(es) => eval_expression_statement(es, env)?,
        Statement::BlockStatement(bs) => eval_block_statement(bs, env)?,
    };

    Ok(object)
}

fn evaluate_identifier_literal(
    ie: &IdentifierLiteral,
    env: &mut Environment,
) -> Result<Object, Error> {
    let val = env.get(&ie.string_literal());

    match val {
        Some(obj) => return Ok(obj.clone()),
        None => Err(anyhow!(
            "Could not find the literal {:?}",
            ie.string_literal()
        )),
    }
}

fn evaluate_integer_literal(ie: &IntegerLiteral, env: &mut Environment) -> Result<Object, Error> {
    Ok(Object::Integer(ie.get_value()))
}

fn evaluate_boolean_literal(be: &BooleanLiteral, env: &mut Environment) -> Result<Object, Error> {
    Ok(Object::Boolean(be.get_value()))
}

fn evaluate_prefix_literal(
    prefix_literal: &PrefixLiteral,
    env: &mut Environment,
) -> Result<Object, Error> {
    let expression: &Expression = &*prefix_literal.right;
    let expression_node = Node::ExpressionNode(expression);
    let right_object = eval(expression_node, env)?;

    match prefix_literal.operator.as_str() {
        "!" => {
            let truthy = match right_object {
                Object::Boolean(b) => b,
                Object::Null => false,
                Object::Integer(i) => true,
                _ => true,
            };

            Ok(Object::Boolean(!truthy))
        }
        "-" => match right_object {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Err(anyhow!("'-' operator requires a integer")),
        },
        _ => Err(anyhow!("unknown prefix operator")),
    }
}

fn evaluate_infix_literal(
    infix_literal: &InfixLiteral,
    env: &mut Environment,
) -> Result<Object, Error> {
    let left_expression_node = Node::ExpressionNode(&*infix_literal.left);
    let right_expression_node = Node::ExpressionNode(&*infix_literal.right);

    let left_object = eval(left_expression_node, env)?;
    let right_object = eval(right_expression_node, env)?;

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
        Object::Integer(0) => false,
        _ => true,
    }
}

fn eval_if_literal(if_literal: &IfLiteral, env: &mut Environment) -> Result<Object, Error> {
    let condition_expression: &Expression = &*if_literal.condition;
    let condition: Object = eval(Node::ExpressionNode(condition_expression), env)?;
    if is_truthy(&condition) {
        let consequence_statement: &Statement = &*if_literal.consequence;
        return eval(Node::StatementNode(consequence_statement), env);
    } else if let Some(ref alternative) = if_literal.alternative {
        let alternative_statement: &Statement = &*alternative;
        return eval(Node::StatementNode(alternative_statement), env);
    } else {
        return Ok(Object::Null);
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, parser::parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Result<Object, Error> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let mut env = Environment::new();

        let program = parser
            .parse_program()
            .map_err(|errs| anyhow!("Parser errors : {:?}", errs))?;

        println!("{:?}", program.statements);
        eval(Node::ProgramNode(&program), &mut env)
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(value) => assert_eq!(*value, expected, "object has wrong value"),
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }

    #[test]
    fn test_let_statements() -> Result<()> {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            test_integer_object(&evaluated, expected);
        }

        Ok(())
    }
    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
            ("-5", -5),
            ("-10", -10),
            // You can add more complex expressions here
            ("-(-5)", 5),
            ("-10 + 20", 10),
            ("-10 * -2", 20),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            // Parentheses and grouping
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
            // Additional edge cases
            ("0", 0),
            ("-0", 0),
            ("1 + 2 + 3 + 4 + 5", 15),
            ("10 - 20 + 30", 20),
            ("(10 + 20) * (30 - 15)", 450),
            ("100 / 10 / 2", 5),
            ("3 + 4 * 2 / (1 - 5)", 1),
            // Multiple levels of nesting
            ("((((10 + 20) * 2) - 50) / 2)", 5),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_eval_integer_edge_cases() -> Result<(), Error> {
        let tests = vec![
            // Division edge cases
            ("10 / 3", 3), // Integer division
            ("-10 / 3", -3),
            ("10 / -3", -3),
            ("-10 / -3", 3),
            // // Large numbers
            ("2147483647", 2147483647),   // i32::MAX
            ("-2147483648", -2147483648), // i32::MIN
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            if let Object::Integer(value) = evaluated {
                assert_eq!(value, expected, "Failed for input: {}", input);
            } else {
                panic!("Expected integer for {}", input);
            }
        }
        Ok(())
    }
    fn test_boolean_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(value) => assert_eq!(*value, expected, "object has wrong value"),
            _ => panic!("object is not Boolean. got={:?}", obj),
        }
    }

    #[test]
    fn test_eval_boolean_expression() -> Result<(), Error> {
        let tests = vec![
            // Basic boolean literals
            ("true", true),
            ("false", false),
            // Numeric comparisons
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            // Boolean comparisons
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            // // Complex expressions
            ("(1 < 2) == true", true),
            ("(1 > 2) == false", true),
            // // Edge cases with parentheses
            ("!true == false", true),
            ("!!true == true", true),
            ("!(1 < 2)", false),
            // book cases =
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == false", true),
            ("!!true == true", true),
            ("!(1 < 2)", false),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            test_boolean_object(&evaluated, expected);
        }
        Ok(())
    }

    #[test]
    fn test_bang_operator() -> Result<(), Error> {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            test_boolean_object(&evaluated, expected);
        }
        Ok(())
    }

    #[test]
    fn test_if_else_expressions() -> Result<(), Error> {
        let tests = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            match expected {
                Some(value) => {
                    if let Object::Integer(i) = evaluated {
                        assert_eq!(i, value, "Failed for input: {}", input);
                    } else {
                        panic!("Expected integer for {}, got {:?}", input, evaluated);
                    }
                }
                None => {
                    assert!(
                        matches!(evaluated, Object::Null),
                        "Expected null for {}, got {:?}",
                        input,
                        evaluated
                    );
                }
            }
        }
        Ok(())
    }

    // Helper function similar to testNullObject
    fn assert_null(obj: &Object, context: &str) {
        assert!(
            matches!(obj, Object::Null),
            "Expected null, got {:?} for {}",
            obj,
            context
        );
    }

    #[test]
    fn test_return_statements() -> Result<(), Error> {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
if (10 > 1) {
return 10;
}
129
return 1;
}",
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            test_integer_object(&evaluated, expected);
        }
        Ok(())
    }
}
