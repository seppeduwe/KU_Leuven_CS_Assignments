//! Minimising Windows
//!
//! Extend your window manager with support for (un)minimising windows. i.e.
//! the ability to temporarily hide windows and to reveal them again later.
//! See the documentation of the [`MinimiseSupport`] trait for the precise
//! requirements.
//!
//! Either make a copy of the tiling window manager with support for floating
//! windows you developed in the previous assignment and let it implement the
//! [`MinimiseSupport`] trait as well, or implement this trait by building a
//! wrapper around the previous window manager. Note that this window manager
//! must still implement all the traits from previous assignments.
//!
//! [`MinimiseSupport`]: ../../cplwm_api/wm/trait.MinimiseSupport.html
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
//! COMMENTS: Using 2 floating window managers, one for visible windows and
//! one for minimised windows. The tests are in a separated file.
//!
//! ...
//!

use cplwm_api::types::{PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::wm::MinimiseSupport;
use cplwm_api::types::Geometry;
use cplwm_api::wm::FloatSupport;
use wm_error::WMError;
use c_floating_windows::FloatingWS;
use custom_trait::CustomSupport;
/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = MinimisingWS;

/// Struct MinimisingWS
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct MinimisingWS {
    /// FloatingWS for visible windows
    pub visible_wm: FloatingWS,
    /// FloatingWS for minimised windows
    pub minimised_wm: FloatingWS,
}

impl WindowManager for MinimisingWS {
    type Error = WMError;

    /// Use 2 floating window managers
    fn new(screen: Screen) -> MinimisingWS {
        MinimisingWS {
            visible_wm: FloatingWS::new(screen),
            minimised_wm: FloatingWS::new(screen),
        }
    }

    /// The `windows` field contains all the windows we manage.
    fn get_windows(&self) -> Vec<Window> {
        let mut temp = self.visible_wm.get_windows();
        temp.extend(self.minimised_wm.get_windows());
        temp
    }

    /// Get focused window.
    fn get_focused_window(&self) -> Option<Window> {
        self.visible_wm.get_focused_window()
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.visible_wm.add_window(window_with_info)
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        if self.is_minimised(window) {
            self.minimised_wm.remove_window(window)
        } else {
            self.visible_wm.remove_window(window)
        }
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        self.visible_wm.get_window_layout()
    }

    /// Note: methods of other traits like focus_window, focus_window, toggle_fullscreen, ...
    /// called with a minimised window as argument should first unminimise the window.

    /// Focus the given window, or when passed None, focus nothing.
    /// This is called when the user clicks or hovers on(to) a window,
    /// or changes the focus using the keyboard.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        match window {
            Some(i_window) => {
                if self.is_minimised(i_window) {
                    try!(self.toggle_minimised(i_window));
                }
                self.visible_wm.focus_window(window)
            }
            None => self.visible_wm.focus_window(window),
        }
    }

    /// Focus the previous or next window.
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        let mut iter_windows = self.get_windows().into_iter().cycle();
        let window: Option<Window>;
        match self.get_focused_window() {
            Some(focus_window) => {
                let len_tiles = self.get_windows().len();
                match iter_windows.position(|w| w == focus_window) {
                    Some(_) => {
                        match dir {
                            PrevOrNext::Prev => {
                                let next_position: usize =
                                    (((len_tiles as i32 - 2)) % (len_tiles as i32)) as usize;
                                window = iter_windows.nth(next_position);
                            }
                            PrevOrNext::Next => {
                                window = iter_windows.next();
                            }
                        }
                    }
                    None => {
                        window = None;
                    }
                }
            }
            None => {
                // When there is only one window,
                // focus it if currently no window is focused, otherwise do nothing.
                // When no window is focused, any window may become focused.
                window = self.get_windows().pop();
                // Do nothing when there are no windows.
            }
        }
        let _ = self.focus_window(window);
    }

    /// It should reflect the current state (location/size, floating or tiled,
    /// fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        if self.is_minimised(window) {
            // side effect for tiled windows, get other geometry
            self.minimised_wm.get_window_info(window)
        } else {
            self.visible_wm.get_window_info(window)
        }
    }

    /// Return the screen managed by the window manager.
    fn get_screen(&self) -> Screen {
        self.visible_wm.get_screen()
    }
    /// Resize the screen according to the given Screen.
    fn resize_screen(&mut self, screen: Screen) {
        self.visible_wm.resize_screen(screen)
    }
}
impl CustomSupport for MinimisingWS {
    /// toggle fullscreen
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.visible_wm
            .set_window(window_with_info)
            .or(self.minimised_wm.set_window(window_with_info))
    }
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize> {
        self.get_windows().iter().position(|w| *w == window)
    }
}
impl TilingSupport for MinimisingWS {
    /// Return the window displayed in the master tile.
    fn get_master_window(&self) -> Option<Window> {
        self.visible_wm.get_master_window()
    }

    /// Swap the given window with the window in the master tile.
    ///
    /// After the function has succeeded, the master window should be focused.
    ///
    /// If the given window is already in the master tile, no windows have to
    /// be swapped, but the master window should be focused.
    fn swap_with_master(&mut self, window: Window) -> Result<(), Self::Error> {
        if self.is_minimised(window) {
            try!(self.toggle_minimised(window));
        }
        self.visible_wm.swap_with_master(window)
    }

    /// Swap the focused window with the one in the next or previous tile.
    ///
    /// Do nothing when there are no windows, when there is only one window,
    /// or when no window is focused.
    ///
    /// If there were two tiles and the swap happened, the same window will be
    /// focused, but the other tile will be focused.
    fn swap_windows(&mut self, dir: PrevOrNext) {
        let mut iter_windows = self.get_windows().into_iter().cycle();
        let swap_with: usize;
        if let Some(focus_window) = self.get_focused_window() {
            let len_tiles = self.get_windows().len();
            if let Some(i) = iter_windows.position(|w| w == focus_window) {
                match dir {
                    PrevOrNext::Prev => {
                        swap_with = (i + len_tiles - 1) % len_tiles;
                    }
                    PrevOrNext::Next => {
                        swap_with = (i + len_tiles + 1) % len_tiles;
                    }
                }
                let tile_i = self.visible_wm.tiling_wm.tiles[i];
                if swap_with >= self.visible_wm.get_windows().len() {
                    self.visible_wm.tiling_wm.tiles[i] =
                        self.minimised_wm.tiling_wm.tiles[swap_with -
                                                          self.visible_wm.get_windows().len()];
                    self.minimised_wm.tiling_wm.tiles[swap_with -
                                                      self.visible_wm.get_windows().len()] = tile_i;
                    // If swapped with minimised window => focus = None
                    let _ = self.focus_window(None);
                } else {
                    self.visible_wm.tiling_wm.tiles[i] = self.visible_wm.tiling_wm.tiles[swap_with];
                    self.visible_wm.tiling_wm.tiles[swap_with] = tile_i;
                }
            }
        }

    }
}

impl FloatSupport for MinimisingWS {
    /// Return a vector of all the visible floating windows.
    fn get_floating_windows(&self) -> Vec<Window> {
        self.visible_wm.get_floating_windows()
    }

    /// If the given window is floating, let it *sink*, if it is not floating,
    /// let it *float*.
    fn toggle_floating(&mut self, window: Window) -> Result<(), Self::Error> {
        self.visible_wm.toggle_floating(window)
    }

    /// Resize/move the given floating window according to the given geometry.
    fn set_window_geometry(&mut self,
                           window: Window,
                           new_geometry: Geometry)
                           -> Result<(), Self::Error> {
        // Is this a requirement?
        // If window is not visible, make it visible
        // if !self.is_floating(window) {
        //    try!(self.toggle_floating(window));
        self.visible_wm.set_window_geometry(window, new_geometry)
    }
}


/// A window manager that supports (un)minimising windows.
///
/// Remember that a tiling window manager displays each window in a different
/// tile. So when you have opened ten applications, the screen will be split
/// in ten tiles. Sometimes you want to open an application without having to
/// look at it the whole time
/// ([`MultiWorkspaceSupport`](trait.MultiWorkspaceSupport.html) is also
/// useful for this scenario). In that case, *minimising* the window is the
/// solution: you can temporarily hide the window and regain the screen space
/// without having to close the window.
///
/// Naturally, there is also a way to reveal these windows again by
/// *unminimising* them.
///
/// **Note**: methods of other traits like `focus_window`, `focus_window`,
/// `toggle_fullscreen`, ... called with a minimised window as argument should
/// first unminimise the window.
///
/// **Hint**: you can use `remove_window` and `add_window` to hide and reveal
/// windows.
impl MinimiseSupport for MinimisingWS {
    /// Return a vector of all the minimised windows.
    ///
    /// The order of the windows in the vector *does* matter.
    ///
    /// The windows must occur in the order they were minimised: the window
    /// that was minimised first must occur first in the vector, the window
    /// that was minimised last must occur last. This makes it easy to define
    /// a function that unminimises the last minimised window.
    fn get_minimised_windows(&self) -> Vec<Window> {
        self.minimised_wm.get_windows()
    }

    /// Minimise the given window, or when it is already minimised, unminise
    /// it.
    ///
    /// When a minimised floating window is unminimised, it should float again
    /// and have the same geometry as before. Hint: you could use the
    /// `float_or_tile` field of `WindowWithInfo`. Analogously for fullscreen
    /// windows.
    ///
    /// if calling `toggle_minimised(w)` with an unminimised
    /// window `w` succeeds, `w` may no longer be visible according to
    /// `get_window_layout` and `is_minimised(w)` must return `true`.
    ///
    /// if calling `toggle_minimised(w)` with an already
    /// minimised window `w` succeeds, `w` must be visible according to
    /// `get_window_layout` and `is_minimised(w)` must return `false`.
    ///
    /// The window layout before and after minimising and directly
    /// unminimising the currently focused window should be the same. This
    /// cannot hold for a window manager that implements
    /// [`TilingSupport`](trait.TilingSupport.html). Try to figure out why.
    fn toggle_minimised(&mut self, window: Window) -> Result<(), Self::Error> {
        let window_detail = try!(self.get_window_info(window));
        if self.is_minimised(window) {
            try!(self.visible_wm.add_window(window_detail));
            self.minimised_wm.remove_window(window)
        } else {
            try!(self.minimised_wm.add_window(window_detail));
            self.visible_wm.remove_window(window)
        }
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
