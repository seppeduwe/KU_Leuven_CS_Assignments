//! Root file of your solution
//!
//! # This file
//!
//! There are two types of projects or *crates* in Rust: libraries and
//! executables. The difference should be obvious. The root of a library
//! project is `src/lib.rs` (like this file) and the root of an executable
//! project is `src/main.rs`.
//!
//! The root file contains some important things:
//!
//! 1. It provides some documentation for the crate. You are reading it right
//!    now.
//! 2. It contains annotations that are true for the whole crate. See the
//!    source code of this file for an example.
//! 3. It declares dependencies on external crates. Again, see the source
//!    code.
//! 4. It declares the modules of which the crate consists using the `mod`
//!    keyword. Don't confuse this with the `use` keyword. If you add any
//!    modules to this project, you have to declare them at the end of this
//!    file using the `mod` keyword. Unless they are submodules of another
//!    module, in which case you have to declare them at the top of that
//!    module. In case you want them to be public (and visible in the
//!    documentation too), use `pub mod` instead of `mod`.
//!
//! See the chapter on [Crates and Modules] in the Rust
//! book for more information.
//!
//! [Crates and Modules]: https://doc.rust-lang.org/book/crates-and-modules.html
//!
//! The source code of this file contains more information.
//!
//! If you are looking at the documentation of this file, the list on the left
//! side contains all the crates that this crate transitively depends on.
//!
//! The next thing to look at is the [`a_fullscreen_wm`
//! module](a_fullscreen_wm/index.html).
//!
//! # Rust Version
//!
//! **TODO**: indicate which version of Rust you are using. If you are using
//! 1.12.1, you don't have to do anything. Otherwise, replace the version
//! below with the output of `rustc --version`.
//!
//! VERSION: rustc 1.13.0 (2c6933acc 2016-11-07)
//!
//! If you are using a nightly as opposed to a stable version of Rust, explain
//! why below:
//!
//! ...
//!

// This line forces you to write documentation for all important things.
#![deny(missing_docs)]
// Note that the documentation starts with three slashes instead of two!
// See https://doc.rust-lang.org/book/documentation.html

// We depend on the cplwm_api crate (defined in the ../api folder) for the
// definition of the traits and the basic types.
extern crate cplwm_api;

// We depend on the rustc_serialize crate for the `Decodable` and `Encodable`
// traits.
extern crate rustc_serialize;

// Add any dependencies below:
/// Custom Error
pub mod wm_error;
// macro_rules! get(
// ($e:expr) => (match $e { Some(e) => e, None => return None })
// );
/// My custom trait
pub mod custom_trait;
// Declare the modules of which this project consists:

// Mandatory assignments
pub mod a_fullscreen_wm;
pub mod b_tiling_wm;
pub mod c_floating_windows;
pub mod d_minimising_windows;
// / Optional assignments
pub mod e_fullscreen_windows;
pub mod f_gaps;
pub mod g_multiple_workspaces;
pub mod h_different_tiling_layout;

// Declare additional modules below or declare them in other modules.
