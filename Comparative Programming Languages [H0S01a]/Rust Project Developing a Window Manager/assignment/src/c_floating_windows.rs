//! Floating Windows
//!
//! Extend your window manager with support for floating windows, i.e. windows
//! that do not tile but that you move around and resize with the mouse. These
//! windows will *float* above the tiles, e.g. dialogs, popups, video players,
//! etc. See the documentation of the [`FloatSupport`] trait for the precise
//! requirements.
//!
//! Either make a copy of the tiling window manager you developed in the
//! previous assignment and let it implement the [`FloatSupport`] trait as
//! well, or implement the [`FloatSupport`] trait by building a wrapper around
//! your tiling window manager. This way you won't have to copy paste code.
//! Note that this window manager must still implement the [`TilingSupport`]
//! trait.
//!
//! [`FloatSupport`]: ../../cplwm_api/wm/trait.FloatSupport.html
//! [`TilingSupport`]: ../../cplwm_api/wm/trait.TilingSupport.html
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
//! COMMENTS: I implemented the [`FloatSupport`] trait by building a wrapper around
//! the tiling window manager, in this way I don't have to write the same functionality
//! over and over again. The tests are in a separated file.
//!
//! ...
//!


// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{FloatOrTile, PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::types::Geometry;
use cplwm_api::wm::FloatSupport;
use custom_trait::CustomSupport;
use b_tiling_wm::TilingWM;
use wm_error::WMError;
/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = FloatingWS;
/// Struct FloatingWS
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct FloatingWS {
    /// Use the previous window manager
    pub tiling_wm: TilingWM,
}

impl WindowManager for FloatingWS {
    type Error = WMError;

    /// Track the given screen and make a new empty `Vec`.
    fn new(screen: Screen) -> FloatingWS {
        FloatingWS { tiling_wm: TilingWM::new(screen) }
    }

    /// The `windows` field contains all the windows we manage.
    fn get_windows(&self) -> Vec<Window> {
        self.tiling_wm.get_windows()
    }

    /// Get focused window.
    fn get_focused_window(&self) -> Option<Window> {
        self.tiling_wm.get_focused_window()
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.tiling_wm.add_window(window_with_info)
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    ///
    /// First we look up the position (or index) of the window in `windows`,
    /// and then remove it unless the window does not occur in the `Vec`, in
    /// which case we return an error.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        self.tiling_wm.remove_window(window)
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        // First get all Tiled windows
        let mut tiling_wm = TilingWM::new(self.tiling_wm.screen);
        tiling_wm.tiles = self.tiling_wm
            .tiles
            .iter()
            .filter(|w| w.float_or_tile == FloatOrTile::Tile)
            .map(|w| *w)
            .collect();
        let mut window_layout = tiling_wm.get_window_layout();
        // Add the Floating windows
        for i in self.tiling_wm
            .tiles
            .iter()
            .filter(|w| w.float_or_tile == FloatOrTile::Float)
            .map(|w| *w) {
            window_layout.windows.push((i.window, i.geometry));
        }
        // Set the focused window
        window_layout.focused_window = self.tiling_wm.focused_window;

        return window_layout;
    }

    /// Focus the given window, or when passed None, focus nothing.
    /// This is called when the user clicks or hovers on(to) a window,
    /// or changes the focus using the keyboard.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        match window {
            Some(i_window) => {
                let window_detail = try!(self.get_window_info(i_window));
                if self.is_floating(i_window) {
                    try!(self.remove_window(i_window));
                    try!(self.add_window(window_detail));
                    Ok(())
                } else {
                    self.tiling_wm.focus_window(window)
                }
            }
            None => {
                self.tiling_wm.focused_window = None;
                Ok(())
            }
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
                                // next_position = (i + len_tiles + 1) % len_tiles;
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
                window = iter_windows.last();
                // Do nothing when there are no windows.
            }
        }
        let _ = self.focus_window(window);

    }

    /// It should reflect the current state (location/size, floating or tiled,
    /// fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        if self.is_managed(window) {
            let window_layout = self.get_window_layout();
            match window_layout.windows.iter().filter(|w| w.0 == window).map(|w| *w).last() {
                None => Err(WMError::UnknownWindow(window)),
                Some(i) => {
                    match self.tiling_wm
                        .tiles
                        .iter()
                        .filter(|w| w.window == window)
                        .map(|w| *w)
                        .last() {
                        Some(mut window_detail) => {
                            window_detail.geometry = i.1;
                            Ok(window_detail)
                        }
                        None => Err(WMError::UnknownWindow(window)),
                    }
                }
            }
        } else {
            Err(WMError::UnknownWindow(window))
        }
    }

    /// Return the screen managed by the window manager.
    fn get_screen(&self) -> Screen {
        self.tiling_wm.get_screen()
    }
    /// Resize the screen according to the given Screen.
    /// This is called whenever the resolution of the screen is changed.
    /// Note that we do not support multiple monitors.
    /// Invariant: after resize_screen is called with a screen, get_screen()
    /// must return the same screen.
    fn resize_screen(&mut self, screen: Screen) {
        self.tiling_wm.resize_screen(screen)
    }
}
impl CustomSupport for FloatingWS {
    /// toggle fullscreen
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.tiling_wm.set_window(window_with_info)
    }
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize> {
        self.tiling_wm.get_window_index(window)
    }
}
impl TilingSupport for FloatingWS {
    /// Return the window displayed in the master tile.
    ///
    /// If there are no windows, return `None`.
    ///
    /// **Invariant**: `get_master_window() == Some(w)`, then `w` must occur
    /// in the vector returned by `get_windows()`.
    ///
    /// **Invariant**: if the vector returned by `get_windows()` is empty =>
    /// `get_master_window() == None`. The other direction of the arrow must
    /// not hold, e.g., there could floating windows (see `FloatSupport`), but
    /// no tiled windows.
    ///
    fn get_master_window(&self) -> Option<Window> {
        self.tiling_wm
            .tiles
            .iter()
            .filter(|w| w.float_or_tile == FloatOrTile::Tile)
            .map(|i| i.window)
            .nth(0)
    }

    /// Swap the given window with the window in the master tile.
    ///
    /// After the function has succeeded, the master window should be focused.
    ///
    /// If the given window is already in the master tile, no windows have to
    /// be swapped, but the master window should be focused.
    ///
    ///
    /// **Invariant**: if `swap_with_master(w)` succeeds, `get_master_window()
    /// == Some(w)`.
    ///
    /// This function is *allowed* to return an appropriate error when the
    /// window is not managed by the window manager.
    fn swap_with_master(&mut self, window: Window) -> Result<(), Self::Error> {
        match self.tiling_wm.get_window_index(window) {
            Some(i) => {
                match self.get_master_window() {
                    Some(master_window) => {
                        let index_master_window =
                            self.tiling_wm.get_window_index(master_window).unwrap();
                        self.tiling_wm
                            .tiles
                            .swap(index_master_window, i);
                        self.tiling_wm.tiles[i].float_or_tile =
                            self.tiling_wm.tiles[index_master_window].float_or_tile;
                        self.tiling_wm.tiles[index_master_window].float_or_tile = FloatOrTile::Tile;
                        Ok(())
                        // self.tiling_wm.focus_window(Some(window))
                    }
                    None => Err(WMError::UnknownWindow(window)),
                }
            }
            None => Err(WMError::UnknownWindow(window)),
        }
    }

    /// Swap the focused window with the one in the next or previous tile.
    ///
    /// Do nothing when there are no windows, when there is only one window,
    /// or when no window is focused.
    ///
    /// If there were two tiles and the swap happened, the same window will be
    /// focused, but the other tile will be focused.
    ///
    /// **Invariant**: calling `swap_windows(dir)` for any `dir` will not
    /// change the focused window, even if no window was focused.
    ///
    /// **Invariant**: calling `swap_windows(dir)` and then
    /// `swap_windows(dir.opposite())` will not change the window layout.
    fn swap_windows(&mut self, dir: PrevOrNext) {
        self.tiling_wm.swap_windows(dir)
    }
}

impl FloatSupport for FloatingWS {
    /// Return a vector of all the visible floating windows.
    ///
    /// The order of the windows in the vector does not matter.
    fn get_floating_windows(&self) -> Vec<Window> {
        self.tiling_wm
            .tiles
            .iter()
            .filter(|w| w.float_or_tile == FloatOrTile::Float)
            .map(|w| w.window)
            .collect()
    }



    /// If the given window is floating, let it *sink*, if it is not floating,
    /// let it *float*.
    ///
    /// When a non-floating window starts to float, its original geometry
    /// (passed to `add_window`) should be restored.
    ///
    /// **Invariant**: if calling `toggle_floating(w)` with a tiled window `w`
    /// succeeds, `is_floating(w)` must return `true`.
    ///
    /// **Invariant**: if calling `toggle_floating(w)` with a floating window
    /// `w` succeeds, `is_floating(w)` must return `false`.
    ///
    /// **Invariant**: the result of `is_floating(w)` must be the same before
    /// and after calling `toggle_floating(w)` twice.
    ///
    /// This function is *allowed* to return an appropriate error when the
    /// window is not managed by the window manager.
    fn toggle_floating(&mut self, window: Window) -> Result<(), Self::Error> {
        match self.tiling_wm.get_window_index(window) {
            Some(index_window) => {
                if self.is_floating(window) {
                    self.tiling_wm.tiles[index_window].float_or_tile = FloatOrTile::Tile
                } else {
                    self.tiling_wm.tiles[index_window].float_or_tile = FloatOrTile::Float
                }
                Ok(())
            }
            None => Err(WMError::UnknownWindow(window)),
        }
    }

    /// Resize/move the given floating window according to the given geometry.
    ///
    /// This function is called when the user moves or resizes a window using
    /// the mouse, but can also be called by custom user commands.
    ///
    /// The window layout should reflect the geometry change of the floating
    /// window.
    ///
    /// This function is *allowed* to return an appropriate error when the
    /// window is not managed by the window manager *or* when the window is
    /// not floating.
    fn set_window_geometry(&mut self,
                           window: Window,
                           new_geometry: Geometry)
                           -> Result<(), Self::Error> {
        match self.tiling_wm.get_window_index(window) {
            Some(index_window) => {
                if self.is_floating(window) {
                    self.tiling_wm.tiles[index_window].geometry = new_geometry;
                    Ok(())
                } else {
                    Err(WMError::UnknownWindow(window))
                }
            }
            None => Err(WMError::UnknownWindow(window)),
        }

    }
}
#[cfg(test)]
mod tests_tiles {
    include!("b_tiling_wm_tests.rs");
}
#[cfg(test)]
mod tests_float {
    include!("c_floating_windows_tests.rs");
}
