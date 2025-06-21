use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{Error, Result, anyhow};

use crate::parser::{
    AbstractSyntaxTree, ArrayLiteral, BlockStatement, BooleanLiteral, CallLiteral, Expression,
    ExpressionStatement, FunctionLiteral, HashLiteral, IdentifierLiteral, IfLiteral, IndexLiteral,
    InfixLiteral, IntegerLiteral, LetStatement, PrefixLiteral, ReturnStatement, Statement,
    StringLiteral,
};

use super::{
    Object,
    object::{Environment, FuctionObject},
};

type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Clone)]
pub enum Node<'a> {
    StatementNode(&'a Statement),
    ExpressionNode(&'a Expression),
    ProgramNode(&'a AbstractSyntaxTree),
}

pub fn eval(node: Node<'_>, mut env: Env) -> Result<Object, Error> {
    match node {
        Node::ExpressionNode(expr) => eval_expression(expr, Rc::clone(&env)),
        Node::StatementNode(stmt) => eval_statement(stmt, Rc::clone(&env)),
        Node::ProgramNode(abstract_syntax_tree) => {
            eval_program(abstract_syntax_tree, Rc::clone(&env))
        }
    }
}

fn eval_program(ast: &AbstractSyntaxTree, env: Env) -> Result<Object, Error> {
    let mut result = Object::Null;
    for stmt in &ast.statements {
        result = eval(Node::StatementNode(stmt), Rc::clone(&env))?;
        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}

fn eval_block_statement(block_statment: &BlockStatement, env: Env) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in block_statment.statements.iter() {
        let statement: &Statement = &*stmt;
        result = eval(Node::StatementNode(stmt), Rc::clone(&env))?;

        if matches!(result, Object::Return(_)) {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_let_statement(let_statement: &LetStatement, mut env: Env) -> Result<Object, Error> {
    let value = eval(
        Node::ExpressionNode((&let_statement.value)),
        Rc::clone(&env),
    )?;

    env.try_borrow_mut()
        .map_err(|e| anyhow!("failed to borrow environment : {:?}", e))?
        .set(&let_statement.identifier.string_literal(), value.clone());

    Ok(value)
}

fn eval_return_statement(return_statement: &ReturnStatement, env: Env) -> Result<Object, Error> {
    let value = eval(
        Node::ExpressionNode(&return_statement.return_value),
        Rc::clone(&env),
    )?;
    Ok(Object::Return(Box::new(value)))
}

fn eval_expression_statement(
    expression_statment: &ExpressionStatement,
    env: Env,
) -> Result<Object, Error> {
    eval(
        Node::ExpressionNode(&*expression_statment.expression),
        Rc::clone(&env),
    )
}

fn eval_statement(stmt: &Statement, env: Env) -> Result<Object, Error> {
    let object = match stmt {
        Statement::LetStatement(ls) => eval_let_statement(ls, Rc::clone(&env))?,
        Statement::ReturnStatement(rs) => eval_return_statement(rs, Rc::clone(&env))?,
        Statement::ExpressionStatement(es) => eval_expression_statement(es, Rc::clone(&env))?,
        Statement::BlockStatement(bs) => eval_block_statement(bs, Rc::clone(&env))?,
    };

    Ok(object)
}

fn eval_expression(expr: &Expression, env: Env) -> Result<Object, Error> {
    let object = match expr {
        Expression::IdentiferExpression(ie) => evaluate_identifier_literal(ie, Rc::clone(&env))?,
        Expression::IntegerExpression(ie) => evaluate_integer_literal(ie, Rc::clone(&env))?,
        Expression::PrefixExpression(pl) => evaluate_prefix_literal(pl, Rc::clone(&env))?,
        Expression::InfixExpression(il) => evaluate_infix_literal(il, Rc::clone(&env))?,
        Expression::BooleanExpression(be) => evaluate_boolean_literal(be, Rc::clone(&env))?,
        Expression::IfExpression(ie) => eval_if_literal(ie, Rc::clone(&env))?,
        Expression::FunctionExpression(fe) => evaluate_function_literal(fe, Rc::clone(&env))?,
        Expression::CallExpression(ce) => evaluate_call_expression(ce, Rc::clone(&env))?,
        Expression::StringExpression(se) => evaluate_string_expression(se, Rc::clone(&env))?,
        Expression::ArrayExpression(ae) => evalutate_array_expression(ae, Rc::clone(&env))?,
        Expression::IndexExpression(ie) => evaluate_index_expression(ie, Rc::clone(&env))?,
        Expression::HashExpression(he) => evaluate_hash_expression(he, Rc::clone(&env))?,
    };

    Ok(object)
}

fn evaluate_hash_expression(hi: &HashLiteral, env: Env) -> Result<Object, Error> {
    // my parirs are a vec<Box<Expresion>, Box<Expression>>

    let mut map: HashMap<Object, Object> = HashMap::new();

    for (key, value) in &hi.pairs {
        let key: Object = eval_expression(&*key, Rc::clone(&env))?;
        let value: Object = eval_expression(&*value, Rc::clone(&env))?;

        map.insert(key, value);
    }

    Ok(Object::HashMap(map))
}

fn evaluate_index_expression(ie: &IndexLiteral, env: Env) -> Result<Object, Error> {
    // let array_object = eval(Node::ExpressionNode(&ie.left), Rc::clone(&env))?;
    let array_object = eval_expression(&ie.left, Rc::clone(&env))?;
    let index_object = eval_expression(&ie.index, Rc::clone(&env))?;

    match (array_object, index_object) {
        (Object::Array(arr), Object::Integer(index)) => {
            let max_index: i64 = (arr.len() - 1) as i64;

            if index < 0 || index > max_index {
                return Ok(Object::Null);
            }

            return Ok(*arr[index as usize].clone());
        }
        (Object::HashMap(hash), key) => match hash.get(&key) {
            Some(value) => Ok(value.clone()),
            None => Ok(Object::Null),
        },
        (a, b) => {
            return Err(anyhow!(
                "Did not get an array and an integer in the index expression evaluation (Object my fiends) got {:?} {:?} ",
                a.get_type(),
                b.get_type()
            ));
        }
    }
}

fn evalutate_array_expression(ar: &ArrayLiteral, env: Env) -> Result<Object, Error> {
    let mut objects: Vec<Box<Object>> = Vec::new();

    for expression in &ar.elements {
        let expression = *expression.clone();
        let object = eval(Node::ExpressionNode(&expression), Rc::clone(&env))?;
        objects.push(Box::new(object));
    }

    Ok(Object::Array(objects))
}

fn evaluate_string_expression(se: &StringLiteral, env: Env) -> Result<Object, Error> {
    Ok(Object::String(se.value.clone()))
}

fn evaluate_function_literal(fe: &FunctionLiteral, env: Env) -> Result<Object, Error> {
    let params = fe.parameters.clone();
    let body = fe.body.clone();
    let function_object = FuctionObject::new(params, body, env);

    return Ok(Object::Function(function_object));
}

fn eval_call_argument_expressions(
    exps: Vec<Box<Expression>>,
    env: Env,
) -> Result<Vec<Object>, Error> {
    let mut result = Vec::new();

    for exp in exps {
        let evaluated = eval(Node::ExpressionNode(&exp), Rc::clone(&env))?;

        result.push(evaluated)
    }

    Ok(result)
}

fn evaluate_call_expression(ce: &CallLiteral, env: Env) -> Result<Object, Error> {
    let function = eval(Node::ExpressionNode(&ce.function), Rc::clone(&env))?;

    let args = eval_call_argument_expressions(ce.arguments.clone(), Rc::clone(&env))?;

    apply_function(function, args)
}

fn apply_function(function: Object, args: Vec<Object>) -> Result<Object, Error> {
    match function {
        Object::Function(func) => {
            let extend_env = extend_function_env(&func, args)?;
            let evalated = eval(Node::StatementNode(&func.body), Rc::clone(&extend_env))?;
            Ok(unwrap_return_value(evalated))
        }
        Object::Builtin(built_in_fn) => built_in_fn(args),
        _ => Err(anyhow!("Not a function : {}", function.get_type())),
    }
}

fn extend_function_env(
    function: &FuctionObject,
    args: Vec<Object>,
) -> Result<Rc<RefCell<Environment>>, Error> {
    let mut env = Rc::new(RefCell::new(Environment::new_enclosed(
        function.env.clone(),
    )));

    for (param_idx, param) in function.parameters.iter().enumerate() {
        let param_name = param.string_literal();
        let arg = args
            .get(param_idx)
            .ok_or_else(|| anyhow!("Not enough arguments in the fucntion call"))?
            .clone();

        env.try_borrow_mut()
            .map_err(|e| anyhow!("borrow error: {:?}", e))?
            .set(&param_name, arg);
    }
    Ok(env)
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(return_val) => *return_val,
        _ => obj,
    }
}

fn evaluate_identifier_literal(ie: &IdentifierLiteral, env: Env) -> Result<Object, Error> {
    let val = env.borrow().get(&ie.string_literal());

    match val {
        Some(obj) => return Ok(obj.clone()),
        None => Err(anyhow!(
            "Could not find the literal {:?}",
            ie.string_literal()
        )),
    }
}

fn evaluate_integer_literal(ie: &IntegerLiteral, env: Env) -> Result<Object, Error> {
    Ok(Object::Integer(ie.get_value()))
}

fn evaluate_boolean_literal(be: &BooleanLiteral, env: Env) -> Result<Object, Error> {
    Ok(Object::Boolean(be.get_value()))
}

fn evaluate_prefix_literal(prefix_literal: &PrefixLiteral, env: Env) -> Result<Object, Error> {
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

fn evaluate_infix_literal(infix_literal: &InfixLiteral, env: Env) -> Result<Object, Error> {
    let left_expression_node = Node::ExpressionNode(&*infix_literal.left);
    let right_expression_node = Node::ExpressionNode(&*infix_literal.right);

    let left_object = eval(left_expression_node, Rc::clone(&env))?;
    let right_object = eval(right_expression_node, Rc::clone(&env))?;

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
        (Object::String(s), "+", Object::String(r)) => Ok(Object::String(format!("{}{}", s, r))),
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

fn eval_if_literal(if_literal: &IfLiteral, env: Env) -> Result<Object, Error> {
    let condition_expression: &Expression = &*if_literal.condition;
    let condition: Object = eval(Node::ExpressionNode(condition_expression), Rc::clone(&env))?;
    if is_truthy(&condition) {
        let consequence_statement: &Statement = &*if_literal.consequence;
        return eval(Node::StatementNode(consequence_statement), Rc::clone(&env));
    } else if let Some(ref alternative) = if_literal.alternative {
        let alternative_statement: &Statement = &*alternative;
        return eval(Node::StatementNode(alternative_statement), Rc::clone(&env));
    } else {
        return Ok(Object::Null);
    }
}

#[cfg(test)]
mod tests {

    use crate::{Lexer, evaluator, parser::parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Result<Object, Error> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let mut env = Environment::new();

        let program = parser
            .parse_program()
            .map_err(|errs| anyhow!("Parser errors : {:?}", errs))?;

        let mut env = Rc::new(RefCell::new(Environment::new()));
        eval(Node::ProgramNode(&program), Rc::clone(&env))
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(value) => assert_eq!(*value, expected, "object has wrong value"),
            _ => panic!("object is not Integer. got={:?}", obj),
        }
    }

    #[test]
    fn test_hash_index_expressions() -> Result<(), Error> {
        struct TestCase<'a> {
            input: &'a str,
            expected: ExpectedValue,
        }

        enum ExpectedValue {
            Int(i64),
            Null,
        }

        let tests = vec![
            TestCase {
                input: r#"{"foo": 5}["foo"]"#,
                expected: ExpectedValue::Int(5),
            },
            TestCase {
                input: r#"{"foo": 5}["bar"]"#,
                expected: ExpectedValue::Null,
            },
            TestCase {
                input: r#"let key = "foo"; {"foo": 5}[key]"#,
                expected: ExpectedValue::Int(5),
            },
            TestCase {
                input: r#"{}["foo"]"#,
                expected: ExpectedValue::Null,
            },
            TestCase {
                input: r#"{5: 5}[5]"#,
                expected: ExpectedValue::Int(5),
            },
            TestCase {
                input: r#"{true: 5}[true]"#,
                expected: ExpectedValue::Int(5),
            },
            TestCase {
                input: r#"{false: 5}[false]"#,
                expected: ExpectedValue::Int(5),
            },
        ];

        for tt in tests {
            let result = test_eval(tt.input)?;

            match tt.expected {
                ExpectedValue::Int(expected) => {
                    if let Object::Integer(actual) = result {
                        assert_eq!(actual, expected, "Expected {}, got {}", expected, actual);
                    } else {
                        panic!("Expected integer {}, got {:?}", expected, result);
                    }
                }
                ExpectedValue::Null => {
                    assert!(
                        matches!(result, Object::Null),
                        "Expected null, got {:?}",
                        result
                    );
                }
            }
        }

        Ok(())
    }
    #[test]
    fn test_hash_literals() -> Result<(), Error> {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
    "#;

        let evaluated = test_eval(input)?;

        match evaluated {
            Object::HashMap(pairs) => {
                // Create expected keys and values
                let expected = vec![
                    (Object::String("one".to_string()), Object::Integer(1)),
                    (Object::String("two".to_string()), Object::Integer(2)),
                    (Object::String("three".to_string()), Object::Integer(3)),
                    (Object::Integer(4), Object::Integer(4)),
                    (Object::Boolean(true), Object::Integer(5)),
                    (Object::Boolean(false), Object::Integer(6)),
                ];

                // Check length matches
                assert_eq!(
                    pairs.len(),
                    expected.len(),
                    "Hash has wrong number of pairs"
                );

                // Check each expected pair
                for (key, expected_value) in expected {
                    match pairs.get(&key) {
                        Some(value) => assert_eq!(*value, expected_value),
                        None => panic!("No pair found for key: {:?}", key),
                    }
                    //
                }
            }
            _ => panic!("Eval didn't return Hash. got={:?}", evaluated),
        }

        Ok(())
    }
    #[test]
    fn test_array_index_expressions() -> Result<()> {
        struct TestCase<'a> {
            input: &'a str,
            expected: Object, // None for nil/null, Some(val) for int
        }

        let tests = vec![
            TestCase {
                input: "[1, 2, 3][0]",
                expected: Object::Integer(1),
            },
            TestCase {
                input: "[1, 2, 3][1]",
                expected: Object::Integer(2),
            },
            TestCase {
                input: "[1, 2, 3][2]",
                expected: Object::Integer(3),
            },
            TestCase {
                input: "let i = 0; [1][i];",
                expected: Object::Integer(1),
            },
            TestCase {
                input: "[1, 2, 3][1 + 1];",
                expected: Object::Integer(3),
            },
            TestCase {
                input: "let myArray = [1, 2, 3]; myArray[2];",
                expected: Object::Integer(3),
            },
            TestCase {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                expected: Object::Integer(6),
            },
            TestCase {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                expected: Object::Integer(2),
            },
            TestCase {
                input: "[1, 2, 3][3]",
                expected: Object::Null,
            },
            TestCase {
                input: "[1, 2, 3][-1]",
                expected: Object::Null,
            },
        ];

        for tt in tests {
            let result = test_eval(tt.input)?;

            match (&result, &tt.expected) {
                (Object::Integer(actual), Object::Integer(expected)) => {
                    assert_eq!(actual, expected)
                }
                (Object::Null, Object::Null) => {}

                _ => {
                    println!(
                        "array_object: {:?}, index_object: {:?}",
                        result, tt.expected
                    );
                    panic!("test ans can't be anything but null or int at the moment")
                }
            }
        }

        Ok(())
    }

    #[test]
    fn test_array_literals() -> Result<(), Error> {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input)?;

        match evaluated {
            Object::Array(vec) => {
                test_integer_object(&vec[0], 1);
                test_integer_object(&vec[1], 4);
                test_integer_object(&vec[2], 6);
            }
            _ => panic!(),
        }

        Ok(())
    }
    #[test]
    fn test_builtin_functions() -> Result<(), Error> {
        struct TestCase {
            input: &'static str,
            expected: ExpectedValue,
        }

        enum ExpectedValue {
            Int(i64),
            Arr(Vec<Object>),
            Str(&'static str), // For error messages
        }

        let tests = vec![
            TestCase {
                input: r#"len("")"#,
                expected: ExpectedValue::Int(0),
            },
            TestCase {
                input: r#"len("four")"#,
                expected: ExpectedValue::Int(4),
            },
            TestCase {
                input: r#",len("hello world")"#,
                expected: ExpectedValue::Int(11),
            },
            TestCase {
                input: "let a = [1,2,3,4];
                first(a);",
                expected: ExpectedValue::Int(1),
            },
            TestCase {
                input: "let a = [1,2,3,4];
                last(a);",
                expected: ExpectedValue::Int(4),
            },
            TestCase {
                input: "let a = [1,2,3,4];
                rest(a)",
                expected: ExpectedValue::Arr(vec![
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ]),
            },
            TestCase {
                input: "let a = [2,3,4];
                push(a, 3)",
                expected: ExpectedValue::Arr(vec![
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(3),
                ]),
            },
        ];

        for tt in tests {
            let result = test_eval(tt.input)?;

            match result {
                Object::Integer(x) => match tt.expected {
                    ExpectedValue::Int(y) => assert_eq!(x, y),
                    _ => panic!(),
                },
                Object::Array(arr) => match tt.expected {
                    ExpectedValue::Arr(expected_arr) => {
                        println!("{:?}, {:?}", arr, expected_arr);
                    }
                    _ => panic!(),
                },
                _ => {
                    println!("{:?}", result);
                    panic!()
                }
            }
        }

        Ok(())
    }

    #[test]
    fn test_string_concatenation() -> Result<(), anyhow::Error> {
        let input = "\"Hello\" + \" \" + \"World!\"";

        let evaluated = test_eval(input)?;

        match evaluated {
            Object::String(s) => {
                assert_eq!(s, "Hello World!")
            }
            _ => panic!("Not a string object type"),
        }

        Ok(())
    }

    #[test]
    fn test_string_literal_object() -> Result<(), anyhow::Error> {
        let input = "\"Hello World!\"";

        let evaluated = test_eval(input)?;

        match evaluated {
            Object::String(s) => {
                assert_eq!(s, "Hello World!")
            }
            _ => panic!("Not a string object type"),
        }
        Ok(())
    }

    #[test]
    fn test_closures() -> Result<(), anyhow::Error> {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);
    "#;

        let evaluated = test_eval(input)?;

        match evaluated {
            Object::Integer(value) => {
                assert_eq!(value, 4, "Expected closure result to be 4");
                Ok(())
            }
            other => Err(anyhow::anyhow!(
                "Expected integer result from closure, got {:?}",
                other
            )),
        }
    }
    #[test]
    fn test_function_Object() -> Result<()> {
        let input = "fn(x) {x + 2;}";
        let evaluated = test_eval(input)?;
        let function_object = match evaluated {
            Object::Function(fe) => {
                assert_eq!(fe.parameters.len(), 1);
                assert_eq!(fe.parameters[0].string_literal(), "x");
                let body = fe.body;
                let body_string = body.string_representation();
                assert_eq!(body_string, "(x + 2)");
            }
            _ => panic!(""),
        };
        Ok(())
    }

    #[test]
    fn test_function_application() -> Result<()> {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input)?;
            test_integer_object(&evaluated, expected);
        }

        Ok(())
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
