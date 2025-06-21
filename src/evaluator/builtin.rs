// The file keeps all the varioud built in function
// that I'm going to implement.

use anyhow::{Error, Result, anyhow};

use super::Object;

pub fn len(args: Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(anyhow!(
            "wrong number of arguments expected one got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        Object::Array(arr) => Ok(Object::Integer(arr.len() as i64)),
        _ => Err(anyhow!(
            "len only supported on String type type got {}",
            args[0].get_type()
        )),
    }
}

pub fn first(args: Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(anyhow!(
            "wrong number of arguments expected one got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            let first = arr.first();

            match first {
                Some(obj) => return Ok(*obj.clone()),
                None => return Ok(Object::Null),
            }
        }
        _ => Err(anyhow!("Not an array object can't call first")),
    }
}

pub fn last(args: Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(anyhow!(
            "wrong number of arguments expected one got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            let last = arr.last();

            match last {
                Some(obj) => return Ok(*obj.clone()),
                None => return Ok(Object::Null),
            }
        }
        _ => Err(anyhow!("Not an array object can't call first")),
    }
}

pub fn rest(args: Vec<Object>) -> Result<Object, Error> {
    if args.len() != 1 {
        return Err(anyhow!(
            "wrong number of arguments expected one got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            let length = arr.len();
            if length > 0 {
                let new_elements = arr[1..].to_owned();
                return Ok(Object::Array(new_elements));
            } else {
                return Ok(Object::Null);
            }
        }
        _ => Err(anyhow!("Not an array object can't call first")),
    }
}

pub fn push(args: Vec<Object>) -> Result<Object, Error> {
    if args.len() != 2 {
        return Err(anyhow!(
            "wrong number of arguments expected one got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            let mut new_arr: Vec<Box<Object>> = Vec::new();
            arr.iter()
                .for_each(|value| new_arr.push(Box::new(*value.clone())));
            new_arr.push(Box::new(args[1].clone()));
            return Ok(Object::Array(new_arr));
        }
        _ => Err(anyhow!("Not an array object can't call push")),
    }
}

pub fn puts(args: Vec<Object>) -> Result<Object, Error> {
    for object in args {
        println!("{}", object.inspect());
    }
    Ok(Object::Null)
}
