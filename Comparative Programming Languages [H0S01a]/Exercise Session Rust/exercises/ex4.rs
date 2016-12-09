/// The width of the trunk of the Christmas tree.
const TRUNK_WIDTH: usize = 3;

/// A struct used to iterate over the lines used to print a Christmas tree.
struct ChristmasTree {
    /// The character used to print the top of the tree
    top: char,
    // TODO add fields
}

impl ChristmasTree {
    /// Create a new `ChrismasTree` that uses `top` as the top of the tree and
    /// `height` as the total number of lines to print (excluding the trunk).
    fn new(top: char, height: usize) -> ChristmasTree {
        unimplemented!()
    }
}


impl Iterator for ChristmasTree {
    /// The iterator generates `String`s
    type Item = String;

    /// Generate the next line of the Christmas tree to print.
    fn next(&mut self) -> Option<String> {
        unimplemented!()
    }
}

fn main() {
    // Play around with '$' and 6
    for i in ChristmasTree::new('$', 6) {
        println!("{}", i);
    }
}
