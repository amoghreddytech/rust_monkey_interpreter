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
        _ => Err(anyhow!(
            "len only supported on String type type got {}",
            args[0].get_type()
        )),
    }
}
