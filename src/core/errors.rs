use std::fmt;

/**
 * Error definitions
 */
#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}


#[macro_export]
macro_rules! runtime_error {
    ($($arg:tt)*) => (
        return Err(RuntimeError { message: format!($($arg)*)})
    )
}

/*
fn runtime_error(arg :String) {
    return Err(RuntimeError { message: arg});
}
*/

#[macro_export]
macro_rules! shift_or_error {
    ($list:expr, $($arg:tt)*) => (
        try!(
            match $list.shift() {
                Some((car, cdr)) => Ok((car, cdr)),
                None => Err(RuntimeError { message: format!($($arg)*)})
            }
        )
    )
}