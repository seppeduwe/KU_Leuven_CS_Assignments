/// Use a type-alias to declare that when we write `Id`, we actually mean
/// `String`.
type Id = String;

/// An expression in a simple language with arithmetic and variables. An
/// expression is any of the following:
///
///     Literal, e.g., 3 (u32)
///     Variable, e.g., x (Id)
///     Assignment, e.g., x = y + 2
///     Application of a binary operator, e.g., x + 3
///     A sequence of two expressions, e.g., x = 1; x + 3
enum Expr {
    Lit(u32), // TODO add missing constructors
}

// Bring the constructors of `Expr` in scope, see
// http://rustbyexample.com/custom_types/enum/enum_use.html
use self::Expr::*;
// With this we can write:
//
//     match e {
//         Lit(n) => ...
//         ...
//     }
//
// Without this, we would have to write:
//
//     match e {
//         Expr::Lit(n) => ...
//         ...
//     }

/// The environment maps `Id`s to `u32`s.
struct Env {
    // TODO replace u32 with the right type
    vars: u32,
}

impl Env {
    /// Create a new empty environment.
    fn new() -> Env {
        unimplemented!()
    }
    /// Look up `id` in the environment.
    fn lookup(&self, id: &Id) -> Option<u32> {
        unimplemented!()
    }
    /// Map `id` to `val` in the environment.
    fn assign(&mut self, id: Id, val: u32) {
        unimplemented!()
    }
}

impl Expr {
    /// Evaluate an expression using the given environment to look up and
    /// assign variables in. An `Err` can only occur when a variable is used
    /// before it is assigned.
    fn eval(&self, env: &mut Env) -> Result<u32, String> {
        unimplemented!()
    }

    /// Evaluate an expression using an empty environment.
    fn eval_(&self) -> Result<u32, String> {
        unimplemented!()
    }
}


fn main() {
    // Try out some examples by replacing the ? with an expression
    // match ?.eval_() {
    //     Ok(n)  => println!("Ok: {}", n),
    //     Err(e) => println!("Err: {}", e)
    // }
}
