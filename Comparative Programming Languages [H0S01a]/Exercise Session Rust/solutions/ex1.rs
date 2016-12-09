pub fn fizzbuzz1() {
    for i in 1..100 {
        if i % 3 == 0 && i % 5 == 0 {
            println!("FizzBuzz");
        } else if i % 3 == 0 {
            println!("Fizz");
        } else if i % 5 == 0 {
            println!("Buzz");
        } else {
            println!("{}", i.to_string());
        }
    }
}

pub fn fizzbuzz2() {
    for i in 1..100 {
        let mut printed = false;
        if i % 3 == 0 {
            print!("Fizz");
            printed = true;
        }
        if i % 5 == 0 {
            print!("Buzz");
            printed = true;
        }
        if printed {
            println!("");
        } else {
            println!("{}", i.to_string());
        }
    }
}

// Alternative implementation using a String buffer.
pub fn fizzbuzz3() {
    for i in 1..100 {
        let mut output = String::new();
        if i % 3 == 0 {
            output.push_str("Fizz");
        }
        if i % 5 == 0 {
            output.push_str("Buzz");
        }
        if output.is_empty() {
            output.push_str(&i.to_string());
        }
        println!("{}", output)
    }
}
