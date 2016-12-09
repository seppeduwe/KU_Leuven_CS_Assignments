/// The width of the trunk of the Christmas tree.
const TRUNK_WIDTH: usize = 3;

/// A struct used to iterate over the lines used to print a Christmas tree.
struct ChristmasTree {
    /// The character used to print the top of the tree
    top: char,
    /// The height of the tree.
    height: usize,
    /// The current line we're printing, where 0 is the top.
    current_line: usize,
}

impl ChristmasTree {
    /// Create a new `ChrismasTree` that uses `top` as the top of the tree and
    /// `height` as the total number of lines to print (excluding the trunk).
    fn new(top: char, height: usize) -> ChristmasTree {
        ChristmasTree {
            top: top,
            height: height,
            current_line: 0,
        }
    }
}

impl Iterator for ChristmasTree {
    /// The iterator generates `String`s
    type Item = String;

    /// Generate the next line of the Christmas tree to print.
    fn next(&mut self) -> Option<String> {
        let blank_ch = ' ';
        let width = 2 * self.height - 1;

        if self.current_line <= self.height {
            // A line, could be the top (when current_line == 0).
            let (ch, blank) = if self.current_line == 0 {
                (self.top, self.height - 1)
            } else {
                ('*', self.height - self.current_line)
            };
            let mut line = String::new();

            for _ in 0..blank {
                line.push(blank_ch);
            }
            for _ in 0..(width - 2 * blank) {
                line.push(ch);
            }
            self.current_line += 1;
            Some(line)
        } else if self.current_line == self.height + 1 {
            // The trunk
            let mut trunk = String::new();
            let blank = (width - TRUNK_WIDTH) / 2;
            for _ in 0..blank {
                trunk.push(blank_ch);
            }
            for _ in 0..TRUNK_WIDTH {
                trunk.push('|');
            }
            self.current_line += 1;
            Some(trunk)
        } else {
            // Done
            None
        }
    }
}

pub fn main() {
    // Play around with '$' and 6
    for i in ChristmasTree::new('$', 6) {
        println!("{}", i);
    }
}
