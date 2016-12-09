/// We can use a `Vec` as a stack because it already has a `push` and a `pop`
/// method.
///
/// You are not allowed to add methods to types that you did not define
/// yourself (they could conflict with other people's methods and/or cause
/// ambiguities), so we define a new struct `Stack` that wraps a `Vec`.
/// Because we defined `Stack` ourselves, we can add methods to it.
///
/// Because this is a struct containing a single field (a /newtype/), Rust
/// optimises this wrapper struct away. When the program is executed, it will
/// be as if there were no extra wrapper around the `Vec`. So no memory is
/// wasted.
///
/// Note the type parameter `<T>` indicating that a `Stack` is generic, just
/// like the `Vec` it wraps.
struct Stack<T> {
    vec: Vec<T>
}

/// You can use an `impl`-block to add methods to a type. Note the `<T>` after
/// `impl`, this is required because the methods are generic over `T` too.
impl<T> Stack<T> {
    /// A static method that creates a new empty `Stack` struct.
    fn new() -> Stack<T> {
        Stack { vec: Vec::new() }
    }

    /// Wraps the `push` method of the underlying `Vec`.
    fn push(&mut self, x: T) {
        self.vec.push(x);
    }

    /// Wraps the `pop` method of the underlying `Vec`. When the `Stack` is
    /// empty, return an `Err` with "no element on the stack" as error
    /// message, otherwise `Ok(x)` where `x` is the element that was last
    /// added.
    fn pop(&mut self) -> Result<T, String> {
        self.vec.pop().ok_or("no element on the stack".to_owned())
    }

    /// Pop two elements of the `Stack` using `pop`.
    ///
    /// Return `Ok((before_last, last))` when there are at least two elements
    /// on the `Stack`.
    ///
    /// When there is only one element on the `Stack`, return an `Err` with
    /// "only one element on the stack" as error message.
    ///
    /// When there are no elements on the `Stack`, return an `Err` with "no
    /// element on the stack" as error message.
    fn pop2(&mut self) -> Result<(T, T), String> {
        let last = try!(self.pop());
        let before_last = try!(self.pop().map_err(|_| {
            "only one element on the stack".to_owned()
        }));
        Ok((before_last, last))

        // Alternative implementation:
        /*
        self.pop().and_then(|last| {
            self.pop().map_err(|_| "only one element on the stack".to_owned())
                .map(|before_last| (before_last, last))
        })
         */

    }

    /// Helper function that uses `pop2` to pop two operands from the `Stack`,
    /// uses them to run the given `op`, and finally pushes the result of the
    /// function call back on the `Stack`.
    ///
    /// If `pop2` failed, its error message is returned, otherwise `()` (unit).
    fn op2<F>(&mut self, op: F) -> Result<(), String>
        where F: FnOnce(T, T) -> T {

        self.pop2().map(|(x, y)| self.push(op(x, y)))
    }

    /// Pop the only element on the `Stack`. Typically used to retrieve the
    /// final result of the calculation.
    ///
    /// When there are no elements on the `Stack`, return an `Err` with "no
    /// element on the stack" as error message.
    ///
    /// When there is more than one element on the `Stack`, return an `Err`
    /// with "more than one element on the stack" as error message.
    fn pop_only(&mut self) -> Result<T, String> {
        let last = try!(self.pop());
        if self.vec.is_empty() {
            Ok(last)
        } else {
            Err("more than one element on the stack".to_owned())
        }
        // Alternative implementation:
        /*
        self.pop().and_then(|last| {
            if self.vec.is_empty() {
                Ok(last)
            } else {
                Err("more than one element on the stack".to_owned())
            }
        })
         */
    }
}

/// Interpret the given string as a RPN calculation.
///
/// Return `Ok(last_element_on_stack)` when the RPN calculation succeeded,
/// otherwise an `Err` containing an error message.
pub fn rpn_calc(s: &str) -> Result<f32, String> {
    let mut stack = Stack::new();

    for token in s.split_whitespace() {
        match token.parse() {
            Ok(n) => stack.push(n),
            Err(_) => match token {
                "+" => try!(stack.op2(|x, y| x + y)),
                "*" => try!(stack.op2(|x, y| x * y)),
                "-" => try!(stack.op2(|x, y| x - y)),
                "/" => try!(stack.op2(|x, y| x / y)),
                s   => return Err(format!("unknown operator: {}", s)),
            }
        }
    }
    stack.pop_only()
}

fn main() {
    match rpn_calc("1 2 + 10 * +") {
        Ok(n)  => println!("Ok: {}", n),
        Err(e) => println!("Err: {}", e),
    }
}

#[test]
fn tests() {
    assert_eq!(Ok(1.0),  rpn_calc("1"));
    assert_eq!(Ok(-7.5), rpn_calc("3 -2.5 *"));
    assert_eq!(Ok(30.0), rpn_calc("1 2 + 10 *"));
    assert_eq!(Ok(4.0),  rpn_calc("9 7 - 4 + 2 * 3 /"));
    assert_eq!(Err("only one element on the stack".to_owned()),  rpn_calc("1 2 + +"));
    assert_eq!(Err("more than one element on the stack".to_owned()),  rpn_calc("1 2 3 +"));
    assert_eq!(Err("no element on the stack".to_owned()),  rpn_calc(""));
    assert_eq!(Err("unknown operator: $".to_owned()),  rpn_calc("1 2 $"));
}
