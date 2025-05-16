use lazy_static::lazy_static;

use crate::{
    ast::{
        ast::Program,
        expressions::{
            BooleanExpression, CallExpression, ConditionalExpression, FunctionExpression,
            GroupedExpression, IdentifierExpression, InfixExpression, IntegerExpression,
            PrefixExpression,
        },
        statements::{BlockStatement, ExpressionStatement, LetStatement, ReturnStatement},
        traits::{Node, Statement},
    },
    object::{Boolean, Integer, Null, Object},
};

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    return match right.as_any() {
        e if e.is::<Boolean>() => {
            let object = e.downcast_ref::<Boolean>().unwrap();
            if object.value == true {
                Box::new(Boolean::new(false))
            } else {
                Box::new(Boolean::new(true))
            }
        }
        e if e.is::<Null>() => Box::new(Boolean::new(true)),
        _ => Box::new(Boolean::new(false)),
    };
}

fn eval_minus_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    return match right.as_any() {
        e if e.is::<Integer>() => {
            let object = e.downcast_ref::<Integer>().unwrap();
            let value = object.value;
            return Box::new(Integer::new(-value));
        }
        _ => Box::new(Null {}),
    };
}

fn eval_prefix_expression(operator: String, right: Box<dyn Object>) -> Option<Box<dyn Object>> {
    if operator == "!" {
        return Some(eval_bang_operator_expression(right));
    } else if operator == "-" {
        return Some(eval_minus_operator_expression(right));
    } else {
        None
    }
}

fn eval_integer_infix(operator: String, left: i64, right: i64) -> Box<dyn Object> {
    if operator == "+" {
        return Box::new(Integer::new(left + right));
    } else if operator == "-" {
        return Box::new(Integer::new(left - right));
    } else if operator == "/" {
        return Box::new(Integer::new(left / right));
    } else if operator == "*" {
        return Box::new(Integer::new(left * right));
    } else {
        Box::new(Null {})
    }
}

fn eval_infix_expression(
    operator: String,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Option<Box<dyn Object>> {
    if let (Some(left_int), Some(right_int)) = (
        left.as_any().downcast_ref::<Integer>(),
        right.as_any().downcast_ref::<Integer>(),
    ) {
        return Some(eval_integer_infix(
            operator,
            left_int.value,
            right_int.value,
        ));
    } else {
        return None;
    }
}

pub fn evaluate(node: &dyn Node) -> Option<Box<dyn Object>> {
    if let Some(program_node) = node.as_any().downcast_ref::<Program>() {
        return Some(eval_statements(&program_node.statements));
    }

    if let Some(prefix_node) = node.as_any().downcast_ref::<PrefixExpression>() {
        match &prefix_node.right {
            Some(expr) => {
                let node_ref: &dyn Node = expr.as_ref().as_node();
                let right = evaluate(node_ref).unwrap();
                return eval_prefix_expression(prefix_node.operator.clone(), right);
            }
            None => return None,
        }
    }

    if let Some(grouped_node) = node.as_any().downcast_ref::<GroupedExpression>() {
        return match &grouped_node.value {
            Some(grouped_expression) => evaluate(grouped_expression.as_ref().as_node()),
            _ => None,
        };
    }

    if let Some(infix_node) = node.as_any().downcast_ref::<InfixExpression>() {
        return match (&infix_node.left, &infix_node.right) {
            (Some(left_expr), Some(right_expr)) => {
                let left = evaluate(left_expr.as_ref().as_node())?;
                let right = evaluate(right_expr.as_ref().as_node())?;
                return eval_infix_expression(infix_node.operator.clone(), left, right);
            }
            _ => None,
        };
    }

    if let Some(boolean_node) = node.as_any().downcast_ref::<BooleanExpression>() {
        let value: bool = boolean_node.value;
        return Some(Box::new(Boolean::new(value)));
    }

    if let Some(int_node) = node.as_any().downcast_ref::<IntegerExpression>() {
        let value: i64 = int_node.value;
        return Some(Box::new(Integer::new(value)));
    }

    if let Some(expression_statement) = node.as_any().downcast_ref::<ExpressionStatement>() {
        if let Some(expression) = &expression_statement.expression {
            return match expression.as_any() {
                e if e.is::<BooleanExpression>() => e
                    .downcast_ref::<BooleanExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<CallExpression>() => {
                    e.downcast_ref::<CallExpression>().and_then(|e| evaluate(e))
                }
                e if e.is::<ConditionalExpression>() => e
                    .downcast_ref::<ConditionalExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<FunctionExpression>() => e
                    .downcast_ref::<FunctionExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<GroupedExpression>() => e
                    .downcast_ref::<GroupedExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<IdentifierExpression>() => e
                    .downcast_ref::<IdentifierExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<InfixExpression>() => e
                    .downcast_ref::<InfixExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<IntegerExpression>() => e
                    .downcast_ref::<IntegerExpression>()
                    .and_then(|e| evaluate(e)),
                e if e.is::<PrefixExpression>() => e
                    .downcast_ref::<PrefixExpression>()
                    .and_then(|e| evaluate(e)),
                _ => None,
            };
        }
    }

    None
}

fn eval_statements<'a>(stmts: &Vec<Box<dyn Statement>>) -> Box<dyn Object> {
    let mut result: Option<Box<dyn Object>> = None;

    for stmt in stmts {
        let mut node: &dyn Node;

        if let Some(let_node) = stmt.as_any().downcast_ref::<LetStatement>() {
            node = let_node;
            result = evaluate(node);
        }
        if let Some(return_node) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            node = return_node;
            result = evaluate(node);
        }
        if let Some(block_node) = stmt.as_any().downcast_ref::<BlockStatement>() {
            node = block_node;
            result = evaluate(node);
        }
        if let Some(expression_node) = stmt.as_any().downcast_ref::<ExpressionStatement>() {
            node = expression_node;
            result = evaluate(node);
        }
    }

    match result {
        Some(value) => return value,
        None => Box::new(Null {}),
    }
}

#[cfg(test)]

mod test {
    use crate::{lexer::lexer::Lexer, object::boolean::Boolean, parser::parser::Parser};

    use super::*;

    fn test_eval(input: String) -> Option<Box<dyn Object>> {
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        return evaluate(&p);
    }

    fn test_integer_object(object: Box<dyn Object>, expected: i64) {
        println!("{:?}", object);
        let int_object = object
            .as_any()
            .downcast_ref::<Integer>()
            .expect("should be an integer object");
        assert_eq!(int_object.value, expected);
    }

    fn test_boolean_object(object: Box<dyn Object>, expected: bool) {
        let int_object = object
            .as_any()
            .downcast_ref::<Boolean>()
            .expect("should be an Boolean object");
        assert_eq!(int_object.value, expected);
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5".to_string(), 5),
            ("10".to_string(), 10),
            ("-5".to_string(), -5),
            ("-10".to_string(), -10),
            ("5 + 5 + 5 + 5 - 10".to_string(), 10),
            ("2 * 2 * 2 * 2 * 2".to_string(), 32),
            ("-50 + 100 + -50".to_string(), 0),
            ("5 * 2 + 10".to_string(), 20),
            ("5 + 2 * 10".to_string(), 25),
            ("20 + 2 * -10".to_string(), 0),
            ("50 / 2 * 2 + 10".to_string(), 60),
            ("2 * (5 + 10)".to_string(), 30),
            ("3 * 3 * 3 + 10".to_string(), 37),
            ("3 * (3 * 3) + 10".to_string(), 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(), 50),
        ];

        for t in tests {
            let (input, output) = t;
            let evaluated = test_eval(input);
            test_integer_object(evaluated.unwrap(), output);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: Vec<(String, bool)> =
            vec![("true;".to_string(), true), ("false".to_string(), false)];

        for t in tests {
            let (input, output) = t;
            let evaluated = test_eval(input);
            test_boolean_object(evaluated.unwrap(), output);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: Vec<(String, bool)> = vec![
            ("!true".to_string(), false),
            ("!false".to_string(), true),
            ("!5".to_string(), false),
            ("!!true".to_string(), true),
            ("!!false".to_string(), false),
            ("!!5".to_string(), true),
        ];

        for t in tests {
            let (input, output) = t;
            let evaluated = test_eval(input);
            test_boolean_object(evaluated.unwrap(), output);
        }
    }
}
