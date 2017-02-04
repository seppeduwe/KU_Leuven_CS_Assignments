//! Optional: Fullscreen Windows
//!
//! Extend your window manager with support for fullscreen windows, i.e. the
//! ability to temporarily make a window take up the whole screen, thereby
//! obscuring all other windows. See the documentation of the
//! [`FullscreenSupport`] trait for the precise requirements. Don't confuse
//! this with the first assignment, in which you built a window manager that
//! displayed all windows fullscreen.
//!
//! Like in the previous assignments, either make a copy of, or define a
//! wrapper around your previous window manager to implement the
//! [`FullscreenSupport`] trait as well. Note that this window manager must
//! still implement all the traits from previous assignments.
//!
//! [`FullscreenSupport`]: ../../cplwm_api/wm/trait.FullscreenSupport.html
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
//! COMMENTS: Using the minimised window manager. The tests are in a
//! separated file.
//! ...
//!

// Add imports here


// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::wm::MinimiseSupport;
use cplwm_api::wm::FullscreenSupport;
use cplwm_api::types::Geometry;
use cplwm_api::wm::FloatSupport;
use custom_trait::CustomSupport;
use wm_error::WMError;
use d_minimising_windows::MinimisingWS;

/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = FullscreenWS;

/// Struct FullscreenWS
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct FullscreenWS {
    /// Use minimisingWS
    pub minimising_ws: MinimisingWS,
}

impl WindowManager for FullscreenWS {
    type Error = WMError;

    /// Used the previous window manager
    fn new(screen: Screen) -> FullscreenWS {
        FullscreenWS { minimising_ws: MinimisingWS::new(screen) }
    }

    /// The `windows` field contains all the windows we manage.
    fn get_windows(&self) -> Vec<Window> {
        self.minimising_ws.get_windows()
    }

    /// Get focused window.
    fn get_focused_window(&self) -> Option<Window> {
        self.minimising_ws.get_focused_window()
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        if let Some(window) = self.get_fullscreen_window() {
            try!(self.toggle_fullscreen(window));
        }
        self.minimising_ws.add_window(window_with_info)
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        self.minimising_ws.remove_window(window)
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        match self.get_fullscreen_window() {
            Some(window) => {
                WindowLayout {
                    focused_window: self.get_fullscreen_window(),
                    windows: vec![(window, self.get_screen().to_geometry())],
                }
            }
            None => self.minimising_ws.get_window_layout(),
        }
    }

    /// Focus the given window, or when passed None, focus nothing.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        // If the fullscreen window is not the same as the window in focus
        // than toggle_fullscreen and focus window.
        if let Some(i_window) = self.get_fullscreen_window() {
            if Some(i_window) != window {
                try!(self.toggle_fullscreen(i_window));
            }
        }
        self.minimising_ws.focus_window(window)
    }

    /// Focus the previous or next window.
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        if let Some(window) = self.get_fullscreen_window() {
            let _ = self.toggle_fullscreen(window);
        }
        self.minimising_ws.cycle_focus(dir)
    }

    /// It should reflect the current state (location/size, floating or tiled,
    /// fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        let mut info_window = try!(self.minimising_ws.get_window_info(window));
        if Some(window) == self.get_fullscreen_window() {
            info_window.geometry = self.get_screen().to_geometry();
        }
        return Ok(info_window);
    }

    /// Return the screen managed by the window manager.
    fn get_screen(&self) -> Screen {
        self.minimising_ws.get_screen()
    }
    /// Resize the screen according to the given Screen.
    fn resize_screen(&mut self, screen: Screen) {
        self.minimising_ws.resize_screen(screen)
    }
}
impl CustomSupport for FullscreenWS {
    /// Set window
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.minimising_ws.set_window(window_with_info)
    }
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize> {
        self.minimising_ws.get_window_index(window)
    }
}

impl TilingSupport for FullscreenWS {
    /// Return the window displayed in the master tile.
    fn get_master_window(&self) -> Option<Window> {
        self.minimising_ws.get_master_window()
    }

    /// Swap the given window with the window in the master tile.
    fn swap_with_master(&mut self, window: Window) -> Result<(), Self::Error> {
        self.minimising_ws.swap_with_master(window)
    }

    /// Swap the focused window with the one in the next or previous tile.
    fn swap_windows(&mut self, dir: PrevOrNext) {
        self.minimising_ws.swap_windows(dir)
    }
}

impl FloatSupport for FullscreenWS {
    /// Return a vector of all the visible floating windows.
    fn get_floating_windows(&self) -> Vec<Window> {
        self.minimising_ws.get_floating_windows()
    }

    /// If the given window is floating, let it *sink*, if it is not floating,
    /// let it *float*.
    fn toggle_floating(&mut self, window: Window) -> Result<(), Self::Error> {
        self.minimising_ws.toggle_floating(window)
    }

    /// Resize/move the given floating window according to the given geometry.
    fn set_window_geometry(&mut self,
                           window: Window,
                           new_geometry: Geometry)
                           -> Result<(), Self::Error> {
        if self.get_fullscreen_window() == Some(window) {
            try!(self.toggle_fullscreen(window));
        }
        self.minimising_ws.set_window_geometry(window, new_geometry)
    }
}


/// A window manager that supports (un)minimising windows.
impl MinimiseSupport for FullscreenWS {
    /// Return a vector of all the minimised windows.
    fn get_minimised_windows(&self) -> Vec<Window> {
        self.minimising_ws.get_minimised_windows()
    }

    /// Minimise the given window, or when it is already minimised, unminise
    /// it.
    fn toggle_minimised(&mut self, window: Window) -> Result<(), Self::Error> {
        self.minimising_ws.toggle_minimised(window)
    }
}

/// A window manager that supports fullscreen windows.
///
/// Users wishing to watch a video fullscreen, to play a game fullscreen, or
/// to view any application fullscreen, can make any window become fullscreen
/// using this trait.
///
/// There can at most one window be fullscreen at a time.
///
/// The backend will detect that some windows want to be fullscreen from the
/// start and will set the `fullscreen` field of `WindowWithInfo` to `true` in
/// these cases. A window manager implementing this trait should implement the
/// `add_window` method of `WindowManager` such that these windows will be
/// fullscreen after adding them.
///
/// A user can make any window fullscreen by using the `toggle_fullscreen`.
///
/// Think carefully about the interaction between a fullscreen window and the
/// other traits: when an action is performed on a fullscreen window, should
/// it stop being fullscreen? What when an action is performed on another
/// window which is not the fullscreen one? In some cases it might make sense
/// to keep the window fullscreen (e.g., when another window is removed), in
/// some cases maybe not (e.g., when a non-fullscreen window is added). Try to
/// make reasonable and consistent choices (keep the invariants of this *and*
/// the other traits in mind). Document the choices and write test cases for
/// them.
impl FullscreenSupport for FullscreenWS {
    /// Return the current fullscreen, if any.
    ///
    /// **Invariant**: if `get_fullscreen_window() == Some(w)`, then
    /// `is_managed(w) == true`.
    ///
    /// **Invariant**: if `get_fullscreen_window() == Some(w)`, then
    /// `get_focused_window() == Some(w)`.
    fn get_fullscreen_window(&self) -> Option<Window> {
        self.get_windows()
            .iter()
            .filter(|w| self.minimising_ws.get_window_info(**w).unwrap().fullscreen == true)
            .map(|m| *m)
            .last()
    }

    /// Make the given window fullscreen, or when it is already fullscreen,
    /// undo it.
    ///
    /// When called on a window that is already fullscreen, it should restore
    /// the window to the state before, e.g. float at the same place.
    /// **Hint**: you could use the `float_or_tile` field of `WindowWithInfo`.
    ///
    /// **Invariant**: if calling `toggle_fullscreen(w)` with a window `w`
    /// that is not yet fullscreen, `w` should be the only visible window
    /// according to `get_window_layout`, its geometry should be the same size
    /// as the screen, and `get_fullscreen_window(w) == Some(w)`.
    ///
    /// The window layout before and after calling `toggle_fullscreen` twice
    /// with the currently focused should be the same. This cannot hold for a
    /// window manager that implements
    /// [`TilingSupport`](trait.TilingSupport.html). Try to figure out why.
    fn toggle_fullscreen(&mut self, window: Window) -> Result<(), Self::Error> {
        let mut window_info = try!(self.minimising_ws.get_window_info(window));
        window_info.fullscreen = !window_info.fullscreen;
        if window_info.fullscreen {
            try!(self.focus_window(Some(window)));
        }
        self.set_window(window_info)
    }
}
#[cfg(test)]
mod b_tiling_wm_tests {
    include!("b_tiling_wm_tests.rs");
}
#[cfg(test)]
mod c_floating_windows_tests {
    include!("c_floating_windows_tests.rs");
}
#[cfg(test)]
mod d_minimising_windows_tests {
    include!("d_minimising_windows_tests.rs");
}
#[cfg(test)]
mod e_fullscreen_windows_tests {
    include!("e_fullscreen_windows_tests.rs");
}
