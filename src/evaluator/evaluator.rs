use crate::{
    ast::{
        ast::Program,
        expressions::{
            BooleanExpression, ConditionalExpression, GroupedExpression, InfixExpression,
            IntegerExpression, PrefixExpression,
        },
        statements::{BlockStatement, ExpressionStatement, ReturnStatement},
        traits::{Node, Statement},
    },
    errors::eval_errors::EvalError,
    object::{Boolean, Integer, Null, Object, Return},
};

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Result<Box<dyn Object>, EvalError> {
    return match right.as_any() {
        e if e.is::<Boolean>() => {
            let object = e.downcast_ref::<Boolean>().unwrap();
            if object.value == true {
                Ok(Box::new(Boolean::new(false)))
            } else {
                Ok(Box::new(Boolean::new(true)))
            }
        }
        e if e.is::<Null>() => Ok(Box::new(Boolean::new(true))),
        _ => Ok(Box::new(Boolean::new(false))),
    };
}

fn eval_minus_operator_expression(right: Box<dyn Object>) -> Result<Box<dyn Object>, EvalError> {
    return match right.as_any() {
        e if e.is::<Integer>() => {
            let object = e.downcast_ref::<Integer>().unwrap();
            let value = object.value;
            return Ok(Box::new(Integer::new(-value)));
        }
        _ => Err(EvalError::unkown_prefix("-".to_string(), right.get_type())),
    };
}

fn eval_prefix_expression(
    operator: String,
    right: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Err(EvalError::unkown_prefix(operator, right.get_type())),
    }
}

fn eval_integer_infix(
    operator: String,
    left: i64,
    right: i64,
) -> Result<Box<dyn Object>, EvalError> {
    return match operator.as_str() {
        "+" => Ok(Box::new(Integer::new(left + right))),
        "-" => Ok(Box::new(Integer::new(left - right))),
        "*" => Ok(Box::new(Integer::new(left * right))),
        "/" => Ok(Box::new(Integer::new(left / right))),
        "<" => Ok(Box::new(Boolean::new(left < right))),
        ">" => Ok(Box::new(Boolean::new(left > right))),
        "==" => Ok(Box::new(Boolean::new(left == right))),
        "!=" => Ok(Box::new(Boolean::new(left != right))),
        _ => Err(EvalError::unkown_infix(
            left.to_string(),
            operator,
            right.to_string(),
        )),
    };
}

fn eval_infix_expression(
    operator: String,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Result<Box<dyn Object>, EvalError> {
    if left.get_type() != right.get_type() {
        return Err(EvalError::type_mismatch(
            left.get_type(),
            operator,
            right.get_type(),
        ));
    }

    if let (Some(left_int), Some(right_int)) = (
        left.as_any().downcast_ref::<Integer>(),
        right.as_any().downcast_ref::<Integer>(),
    ) {
        return eval_integer_infix(operator, left_int.value, right_int.value);
    }

    if let (Some(left_bool), Some(right_bool)) = (
        left.as_any().downcast_ref::<Boolean>(),
        right.as_any().downcast_ref::<Boolean>(),
    ) {
        if operator == "==" {
            return Ok(Box::new(Boolean::new(left_bool.value == right_bool.value)));
        } else if operator == "!=" {
            return Ok(Box::new(Boolean::new(left_bool.value != right_bool.value)));
        }
    }

    return Err(EvalError::unkown_infix(
        left.get_type(),
        operator,
        right.get_type(),
    ));
}

fn is_truthy(condition: Box<dyn Object>) -> bool {
    if let Some(_) = condition.as_any().downcast_ref::<Null>() {
        return false;
    }

    if let Some(cond) = condition.as_any().downcast_ref::<Boolean>() {
        if cond.value == true {
            return true;
        } else if cond.value == false {
            return false;
        }
    }

    true
}

fn eval_if_expression(node: &ConditionalExpression) -> Result<Box<dyn Object>, EvalError> {
    let condition = evaluate(node.condition.as_node());
    if let Ok(cond) = condition {
        if is_truthy(cond) {
            return evaluate(&node.consequence);
        } else if node.alternative.is_none() {
            match &node.alternative {
                Some(n) => return evaluate(n),
                None => return Ok(Box::new(Null {})),
            }
        }
    }

    return Ok(Box::new(Null {}));
}

fn eval_program(program: &Program) -> Result<Box<dyn Object>, EvalError> {
    let mut result: Box<dyn Object> = Box::new(Null::new());

    for stmt in &program.statements {
        result = evaluate(stmt.as_node())?;

        if let Some(return_obj) = result.as_any().downcast_ref::<Return>() {
            return Ok(return_obj.value.clone());
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement) -> Result<Box<dyn Object>, EvalError> {
    let mut result = Box::new(Null::new()) as Box<dyn Object>;
    for stmt in &block.statements {
        result = evaluate(stmt.as_node())?;
        if let Some(_) = result.as_any().downcast_ref::<Return>() {
            return Ok(result);
        }
    }

    Ok(result)
}

pub fn evaluate(node: &dyn Node) -> Result<Box<dyn Object>, EvalError> {
    if let Some(return_node) = node.as_any().downcast_ref::<ReturnStatement>() {
        let evaluated_value = match &return_node.return_value {
            Some(expr) => evaluate(expr.as_ref().as_node())?,
            None => Box::new(Null::new()),
        };

        return Ok(Box::new(Return::new(evaluated_value)));
    }

    if let Some(program_node) = node.as_any().downcast_ref::<Program>() {
        return eval_program(program_node);
    }

    if let Some(block_statment) = node.as_any().downcast_ref::<BlockStatement>() {
        return eval_block_statement(block_statment);
    }

    if let Some(conditional_node) = node.as_any().downcast_ref::<ConditionalExpression>() {
        return eval_if_expression(conditional_node);
    }

    if let Some(prefix_node) = node.as_any().downcast_ref::<PrefixExpression>() {
        match &prefix_node.right {
            Some(expr) => {
                let node_ref: &dyn Node = expr.as_ref().as_node();
                let right = evaluate(node_ref).unwrap();
                return eval_prefix_expression(prefix_node.operator.clone(), right);
            }
            None => Err(EvalError::GenericError)?,
        }
    }

    if let Some(grouped_node) = node.as_any().downcast_ref::<GroupedExpression>() {
        return match &grouped_node.value {
            Some(grouped_expression) => evaluate(grouped_expression.as_ref().as_node()),
            _ => Err(EvalError::GenericError),
        };
    }

    if let Some(infix_node) = node.as_any().downcast_ref::<InfixExpression>() {
        return match (&infix_node.left, &infix_node.right) {
            (Some(left_expr), Some(right_expr)) => {
                let left = evaluate(left_expr.as_ref().as_node())?;
                let right = evaluate(right_expr.as_ref().as_node())?;
                return eval_infix_expression(infix_node.operator.clone(), left, right);
            }
            _ => Err(EvalError::GenericError),
        };
    }

    if let Some(boolean_node) = node.as_any().downcast_ref::<BooleanExpression>() {
        let value: bool = boolean_node.value;

        return Ok(Box::new(Boolean::new(value)));
    }

    if let Some(int_node) = node.as_any().downcast_ref::<IntegerExpression>() {
        let value: i64 = int_node.value;
        return Ok(Box::new(Integer::new(value)));
    }

    if let Some(expression_statement) = node.as_any().downcast_ref::<ExpressionStatement>() {
        if let Some(expression) = &expression_statement.expression {
            return evaluate(expression.as_ref().as_node());
        }
    }

    Err(EvalError::GenericError)
}

fn eval_statements<'a>(stmts: &Vec<Box<dyn Statement>>) -> Result<Box<dyn Object>, EvalError> {
    let mut result: Box<dyn Object> = Box::new(Null {});

    for stmt in stmts {
        result = evaluate(stmt.as_node())?;

        if let Some(return_obj) = result.as_any().downcast_ref::<Return>() {
            return Ok(return_obj.value.clone());
        }
    }

    Ok(result)
}

#[cfg(test)]

mod test {

    use crate::{lexer::lexer::Lexer, object::boolean::Boolean, parser::parser::Parser};

    use super::*;

    fn test_eval(input: String) -> Result<Box<dyn Object>, EvalError> {
        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);
        let mut p = Program::new(parser);
        p.parse_program();

        return evaluate(&p);
    }

    fn test_integer_object(object: Box<dyn Object>, expected: i64) {
        let int_object = object
            .as_any()
            .downcast_ref::<Integer>()
            .expect("should be an integer object");
        assert_eq!(int_object.value, expected);
    }

    fn test_boolean_object(object: Box<dyn Object>, expected: bool) {
        let bool_object = object
            .as_any()
            .downcast_ref::<Boolean>()
            .expect("should be an Boolean object");
        assert_eq!(bool_object.value, expected);
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
        let tests: Vec<(String, bool)> = vec![
            ("true;".to_string(), true),
            ("false".to_string(), false),
            ("true == false".to_string(), false),
            ("true != false".to_string(), true),
            ("(1 < 2) == true".to_string(), true),
            ("(1 < 2) == false".to_string(), false),
            ("(1 > 2) == true".to_string(), false),
            ("(1 > 2) == false".to_string(), true),
        ];

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

    #[test]
    fn test_if_else_staement() {
        let tests: Vec<(String, Box<dyn Object>)> = vec![
            ("if (true) {10};".to_string(), Box::new(Integer::new(10))),
            ("if (false) {10};".to_string(), Box::new(Null {})),
            ("if (1) {10};".to_string(), Box::new(Integer::new(10))),
            ("if (1 < 2) {10};".to_string(), Box::new(Integer::new(10))),
            ("if (1 > 2) {10};".to_string(), Box::new(Null {})),
            (
                "if (1 > 2) {10} else { 20 };".to_string(),
                Box::new(Integer::new(20)),
            ),
            (
                "if (1 < 2) {10} else { 20 };".to_string(),
                Box::new(Integer::new(10)),
            ),
        ];

        for t in tests {
            let (input, output) = t;
            let evaluated = test_eval(input).unwrap();
            match evaluated.as_any() {
                e if e.is::<Null>() => {
                    let _ = e.downcast_ref::<Null>().expect("should become null");
                }
                e if e.is::<Integer>() => e
                    .downcast_ref::<Integer>()
                    .map(|x| {
                        let o = output.as_any().downcast_ref::<Integer>().expect("Omg");
                        assert_eq!(x.value, o.value);
                    })
                    .expect(""),
                _ => panic!(),
            };
        }
    }

    fn test_null_object(object: Box<dyn Object>) -> bool {
        let _ = object
            .as_any()
            .downcast_ref::<Null>()
            .expect("should be an null object");
        return true;
    }

    #[test]
    fn test_return_objecr() {
        let tests: Vec<(&str, i64)> = vec![
            ("return 10;", 10),
            ("return 10; 9", 10),
            ("return 2 * 5;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }",
                10,
            ),
        ];

        for test in tests {
            let (input, expected) = test;
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated.unwrap(), expected);
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected_output) in tests {}
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "
            if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
                return 1;
            }
            ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar;", "identifier not found: foobar"),
        ];

        for (input, expected_message) in tests {
            let evaluated = test_eval(input.to_string());

            match evaluated {
                Ok(obj) => {
                    panic!(
                        "expected error, got successful result: type={} value={:?}",
                        obj.get_type(),
                        obj
                    );
                }
                Err(e) => {
                    let actual = e.to_string();
                    assert_eq!(actual, expected_message, "unexpected error message");
                }
            }
        }
    }
}
