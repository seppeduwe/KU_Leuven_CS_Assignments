//! Fullscreen Window Manager
//!
//! Implement the [`WindowManager`] trait by writing a simple window manager
//! that displays every window fullscreen. When a new window is added, the
//! last window that was visible will become invisible.
//!
//! [`WindowManager`]: ../../cplwm_api/wm/trait.WindowManager.html
//!
//! Now have a look at the source code of this file, it contains a tutorial to
//! help you write the fullscreen window manager.
//!
//! You are free to remove the documentation in this file that is only part of
//! the tutorial or no longer matches the code after your changes.
//!
//! # Status
//!
//! **TODO**: Replace the question mark below with YES, NO, or PARTIAL to
//! indicate the status of this assignment. If you want to tell something
//! about this assignment to the grader, e.g., you have a bug you can't fix,
//! or you want to explain your approach, write it down after the comments
//! section.
//!
//! COMPLETED: YES
//!
//! COMMENTS: Because at the start of the assignment my Rust skills
//! were poor, the implementation could be better. If I knew then what I know
//! now I would do it totally different. I would make 2 vec's and a focus variable,
//! all of type WindowWithInfo , 1 vec for the windows above the focus variable
//! and one vec for the windows under the focus variable (maybe in reverse order).
//!
//! I used a different file for the tests.

// Because not all methods are implemented yet, some arguments are unused,
// which generates warnings. The annotation below disables this warning.
// Remove this annotation when you have implemented all methods, so you get
// warned about variables that you did not use by mistake.

// We import std::error and std::format so we can say error::Error instead of
// std::error::Error, etc.
use std::error;
use std::fmt;
use std::collections::VecDeque;
// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{FloatOrTile, PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;

/// You are free to choose the name for your window manager. As we will use
/// automated tests when grading your assignment, indicate here the name of
/// your window manager data type so we can just use `WMName` instead of
/// having to manually figure out your window manager name.
pub type WMName = FullscreenWM;


/// The FullscreenWM struct
///
/// The first thing to do when writing a window manager, is to define a struct
/// (or enum) that will contain the state of the window manager, e.g. the
/// managed windows along with their geometries, the focused window, etc.
///
/// Depending on the layout and the functionality the window manager provides,
/// this can vary from simple `Vec`s to trees, hashmaps, etc. You can have a
/// look at the [collections](https://doc.rust-lang.org/std/collections/) Rust
/// provides.
///
/// Remember that you are free to add additional dependencies to your project,
/// e.g., for another type of data structure. But this is certainly not
/// required. For more information, see the Hints & Tricks section of the
/// assignment.
///
/// # Example Representation
///
/// The fullscreen window manager that we are implementing is very simple: it
/// just needs to keep track of all the windows that were added and remember
/// which one is focused. It is not even necessary to remember the geometries
/// of the windows, as they will all be resized to the size of the screen.
///
/// A possible data structure to keep track of the windows is a simple `Vec`:
/// the last element in the vector is the window on top, which is also the
/// only window to display. Why not the first element? Because it is easier to
/// add an element to the end of a vector. This is convenient, as adding a new
/// window should also put it on top of the other windows.
///
/// Another thing we need to keep track of is the `Screen`, because we must
/// resize the windows the size of the screen. A `Screen` is passed via the
/// `new` method of the trait and the `resize_screen` method of the trait
/// updates the screen with a new one.
///
/// These two fields are enough to get started, which does not mean that they
/// are enough to correctly implement this window manager. As you will notice
/// in a short while, there is a problem with this representation. Feel free
/// to add/replace/remove fields.
///
/// To understand the `#derive[(..)]` line before the struct, read the
/// [Supertraits] section of the `WindowManager` trait.
///
/// [Supertraits]: ../../cplwm_api/wm/trait.WindowManager.html#supertraits
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct FullscreenWM {
    /// A vector of windows, the first one is on the bottom, the last one is
    /// on top, and also the only visible window.
    pub windows: VecDeque<Window>,
    /// We need to know which size the fullscreen window must be.
    pub screen: Screen,
    /// Window that is focused
    pub focused_window: Option<Window>,
}

/// The errors that this window manager can return.
///
/// For more information about why you need this, read the documentation of
/// the associated [Error] type of the `WindowManager` trait.
///
/// In the code below, we would like to return an error when we are asked to
/// do something with a window that we do not manage, so we define an enum
/// `FullscreenWMError` with one variant: `UnknownWindow`.
///
/// Feel free to add or remove variants from this enum. You may also replace
/// it with a type or struct if you wish to do so.
///
/// [Error]: ../../cplwm_api/wm/trait.WindowManager.html#associatedtype.Error
#[derive(Debug)]
pub enum FullscreenWMError {
    /// This window is not known by the window manager.
    UnknownWindow(Window),
    /// Window Already Managed
    WindowAlreadyManaged(Window),
}

// This code is explained in the documentation of the associated [Error] type
// of the `WindowManager` trait.
impl fmt::Display for FullscreenWMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FullscreenWMError::UnknownWindow(ref window) => write!(f, "Unknown window: {}", window),
            FullscreenWMError::WindowAlreadyManaged(ref window) => {
                write!(f, "Window already managed: {}", window)
            }
        }
    }
}

// This code is explained in the documentation of the associated [Error] type
// of the `WindowManager` trait.
impl error::Error for FullscreenWMError {
    fn description(&self) -> &'static str {
        match *self {
            FullscreenWMError::UnknownWindow(_) => "Unknown window",
            FullscreenWMError::WindowAlreadyManaged(_) => "Window Already Managed",
        }
    }
}

// Now we start implementing our window manager
impl WindowManager for FullscreenWM {
    /// We use `FullscreenWMError` as our `Error` type.
    type Error = FullscreenWMError;

    /// The constructor is straightforward.
    ///
    /// Track the given screen and make a new empty `Vec`.
    fn new(screen: Screen) -> FullscreenWM {
        FullscreenWM {
            windows: VecDeque::new(),
            screen: screen,
            focused_window: None,
        }
    }

    /// The `windows` field contains all the windows we manage.
    ///
    /// Why do we need a `clone` here?
    fn get_windows(&self) -> Vec<Window> {
        self.windows.clone().into_iter().collect()
    }

    /// The last window in the list is the focused one.
    ///
    /// Note that the `last` method of `Vec` returns an `Option`.
    fn get_focused_window(&self) -> Option<Window> {
        self.focused_window
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    ///
    /// We could choose to return an error when the window is already managed
    /// by the window manager, but in this case we just do nothing. You are
    /// free to define another error to handle this case.
    ///
    /// Note that we completely ignore the information that comes along with
    /// the info, this *could* lead to issues in later assignments.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        if !self.is_managed(window_with_info.window) {
            self.windows.push_back(window_with_info.window);
            self.focus_window(Some(window_with_info.window))
        } else {
            Err(FullscreenWMError::WindowAlreadyManaged(window_with_info.window))
        }
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    ///
    /// First we look up the position (or index) of the window in `windows`,
    /// and then remove it unless the window does not occur in the `Vec`, in
    /// which case we return an error.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        match self.windows.iter().position(|w| *w == window) {
            None => Err(FullscreenWMError::UnknownWindow(window)),
            Some(i) => {
                self.windows.remove(i);
                let last_window = self.windows.back().map(|w| *w);
                self.focus_window(last_window)
                // if self.get_focused_window() == Some(window) {
                //    self.focus_window(None);
            }
        }
    }

    /// Now the most important part: calculating the `WindowLayout`.
    ///
    /// First we build a `Geometry` for a fullscreen window using the
    /// `to_geometry` method: it has the same width and height as the screen.
    ///
    /// Then we look at the last window, remember that the `last()` method of
    /// `Vec` returns an `Option`.
    ///
    /// * When the `Option` contains `Some(w)`, we know that there was at
    ///   least one window, and `w`, being the last window in the `Vec` should
    ///   be focused. As the other windows will not be visible, the `windows`
    ///   field of `WindowLayout` can just be a `Vec` with one element: the
    ///   one window along with the fullscreen `Geometry`.
    ///
    /// * When the `Option` is `None`, we know that there are no windows, so
    ///   we can just return an empty `WindowLayout`.
    ///
    fn get_window_layout(&self) -> WindowLayout {
        let fullscreen_geometry = self.screen.to_geometry();
        match self.windows.back() {
            // If there is at least one window.
            Some(w) => {
                WindowLayout {
                    // The last window is focused ...
                    focused_window: self.get_focused_window(),
                    // ... and should fill the screen. The other windows are
                    // simply hidden.
                    windows: vec![(*w, fullscreen_geometry)],
                }
            }
            // Otherwise, return an empty WindowLayout
            None => WindowLayout::new(),
        }
    }

    // Before you continue any further, first have a look at the bottom of
    // this file, where we show you how to write unit tests.

    /// Try this yourself
    ///
    /// Don't forget that when the argument is `None`, i.e. no window should
    /// be focused, `get_focused_window()` must return `None` afterwards. The
    /// `focused_window` field of the `WindowLayout` must also be `None`.
    ///
    /// You will probably have to change the code above (method
    /// implementations as well as the `FullscreenWM` struct) to achieve this.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        // self.focused_window = window;
        match window {
            Some(i_window) => {
                match self.windows.iter().position(|w| *w == i_window) {
                    None => Err(FullscreenWMError::UnknownWindow(i_window)),
                    Some(i) => {
                        // Set window to front
                        self.windows.remove(i);
                        self.windows.push_back(i_window);
                        self.focused_window = Some(i_window);
                        Ok(())
                    }
                }
            }
            None => {
                self.focused_window = None;
                Ok(())
            }
        }
    }

    /// Try this yourself
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        // You will probably notice here that a `Vec` is not the ideal data
        // structure to implement this function. Feel free to replace the
        // `Vec` with another data structure.

        // Do nothing when there are no windows.
        if self.windows.is_empty() {
            return ();
        }
        // if self.get_focused_window() == None {
        //
        match dir {
            PrevOrNext::Prev => {
                let last_window = self.windows.pop_back().unwrap();
                self.windows.push_front(last_window);
            }
            PrevOrNext::Next => {
                let first_window = self.windows.pop_front().unwrap();
                self.windows.push_back(first_window);
            }
        }
        // When there is only one window,
        // focus it if currently no window is focused, otherwise do nothing.
        // When no window is focused, any window may become focused.
        let window = self.windows.back().map(|w| *w);
        match self.focus_window(window) {
            Ok(_) => {}
            Err(e) => println!("Error focus_window {}", e),
        }
        return ();
    }

    /// Try this yourself
    // It should reflect the current state (location/size, floating or tiled,
    // fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        let fullscreen_geometry = self.screen.to_geometry();
        if self.is_managed(window) {
            Ok(WindowWithInfo {
                window: window,
                geometry: fullscreen_geometry,
                float_or_tile: FloatOrTile::Tile,
                fullscreen: true,
            })
        } else {
            Err(FullscreenWMError::UnknownWindow(window))
        }
    }

    /// Try this yourself
    fn get_screen(&self) -> Screen {
        self.screen
    }

    /// Try this yourself
    fn resize_screen(&mut self, screen: Screen) {
        self.screen = screen
    }
}

#[cfg(test)]
mod a_fullscreen_wm_tests {
    include!("a_fullscreen_wm_tests.rs");
}
