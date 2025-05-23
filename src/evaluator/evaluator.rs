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
    object::{Boolean, Integer, Null, Object, Return},
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
    return match operator.as_str() {
        "+" => Box::new(Integer::new(left + right)),
        "-" => Box::new(Integer::new(left - right)),
        "*" => Box::new(Integer::new(left * right)),
        "/" => Box::new(Integer::new(left / right)),
        "<" => Box::new(Boolean::new(left < right)),
        ">" => Box::new(Boolean::new(left > right)),
        "==" => Box::new(Boolean::new(left == right)),
        "!=" => Box::new(Boolean::new(left != right)),
        _ => Box::new(Null {}),
    };
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
    }
    if let (Some(left_bool), Some(right_bool)) = (
        left.as_any().downcast_ref::<Boolean>(),
        right.as_any().downcast_ref::<Boolean>(),
    ) {
        if operator == "==" {
            return Some(Box::new(Boolean::new(left_bool.value == right_bool.value)));
        } else if operator == "!=" {
            return Some(Box::new(Boolean::new(left_bool.value != right_bool.value)));
        }
    }

    return Some(Box::new(Null {}));
}

fn is_truthy(condition: Box<dyn Object>) -> bool {
    if let Some(cond) = condition.as_any().downcast_ref::<Null>() {
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

fn eval_if_expression(node: &ConditionalExpression) -> Box<dyn Object> {
    let condition = evaluate(node.condition.as_node());
    if let Some(cond) = condition {
        if is_truthy(cond) {
            return evaluate(&node.consequence).expect("this might need to be fixed in the future");
        } else if node.alternative.is_none() {
            match &node.alternative {
                Some(n) => return evaluate(n).expect("this should not fail"),
                None => return Box::new(Null {}),
            }
        }
    }

    return Box::new(Null {});
}

fn eval_program(program: &Program) -> Box<dyn Object> {
    let mut result = Box::new(Null::new()) as Box<dyn Object>;

    for stmt in &program.statements {
        result = evaluate(stmt.as_node()).unwrap_or_else(|| Box::new(Null::new()));

        if let Some(return_obj) = result.as_any().downcast_ref::<Return>() {
            return return_obj.value.clone();
        }
    }

    result
}

fn eval_block_statement(block: &BlockStatement) -> Box<dyn Object> {
    let mut result = Box::new(Null::new()) as Box<dyn Object>;
    for stmt in &block.statements {
        result = evaluate(stmt.as_node()).unwrap_or_else(|| Box::new(Null::new()));
        if let Some(return_obj) = result.as_any().downcast_ref::<Return>() {
            return result;
        }
    }

    result
}

pub fn evaluate(node: &dyn Node) -> Option<Box<dyn Object>> {
    if let Some(return_node) = node.as_any().downcast_ref::<ReturnStatement>() {
        let evaluated_value = return_node
            .return_value
            .as_ref()
            .and_then(|expr| evaluate(expr.as_ref().as_node()))
            .unwrap_or_else(|| Box::new(Null::new()));

        return Some(Box::new(Return::new(evaluated_value)));
    }

    if let Some(program_node) = node.as_any().downcast_ref::<Program>() {
        return Some(eval_program(program_node));
    }

    if let Some(block_statment) = node.as_any().downcast_ref::<BlockStatement>() {
        return Some(eval_block_statement(block_statment));
    }

    if let Some(conditional_node) = node.as_any().downcast_ref::<ConditionalExpression>() {
        return Some(eval_if_expression(conditional_node));
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
    let mut result: Box<dyn Object> = Box::new(Null {});

    for stmt in stmts {
        result = evaluate(stmt.as_node()).unwrap_or_else(|| Box::new(Null::new()));

        if let Some(return_obj) = result.as_any().downcast_ref::<Return>() {
            return return_obj.value.clone();
        }
    }
    result
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
                    let x = e.downcast_ref::<Null>().expect("should become null");
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
        let null_object = object
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
}
