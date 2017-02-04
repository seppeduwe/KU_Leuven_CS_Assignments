//! Tiling Window Manager
//!
//! Write a more complex window manager that will *tile* its windows. Tiling
//! is described in the first section of the assignment. Your window manager
//! must implement both the [`WindowManager`] trait and the [`TilingSupport`]
//! trait. See the documentation of the [`TilingSupport`] trait for the
//! precise requirements and an explanation of the tiling layout algorithm.
//!
//! [`WindowManager`]: ../../cplwm_api/wm/trait.WindowManager.html
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
//! COMMENTS: Test are in a separated file.
//!
//! ...
//!

// Add imports here
/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = TilingWM;


use std::os::raw::c_uint;
// We import std::error and std::format so we can say error::Error instead of
// std::error::Error, etc.

use std::collections::VecDeque;
// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::types::Geometry;
use custom_trait::CustomSupport;
use wm_error::WMError;
/// Struct TilingWM
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct TilingWM {
    /// A vector of windows, the first one is on the bottom, the last one is
    /// on top, and also the only visible window.
    pub tiles: VecDeque<WindowWithInfo>,
    /// We need to know which size the fullscreen window must be.
    pub screen: Screen,
    /// Window that is focused
    pub focused_window: Option<Window>,
}

impl WindowManager for TilingWM {
    type Error = WMError;

    /// Track the given screen and make a new empty `Vec`.
    fn new(screen: Screen) -> TilingWM {
        TilingWM {
            tiles: VecDeque::new(),
            screen: screen,
            focused_window: None,
        }
    }

    /// The `windows` field contains all the windows we manage.
    fn get_windows(&self) -> Vec<Window> {
        self.tiles.iter().map(|w| w.window).collect()
    }

    /// Get focused window.
    fn get_focused_window(&self) -> Option<Window> {
        self.focused_window
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        if !self.is_managed(window_with_info.window) {
            self.tiles.push_back(window_with_info);
            self.focus_window(Some(window_with_info.window))
        } else {
            Err(WMError::WindowAlreadyManaged(window_with_info.window))
        }
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        match self.get_window_index(window) {
            None => Err(WMError::UnknownWindow(window)),
            Some(i) => {
                self.tiles.remove(i);
                if self.get_focused_window() == Some(window) {
                    self.focus_window(None).unwrap();
                }
                Ok(())
            }
        }
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        let fullscreen_geometry = self.screen.to_geometry();
        let mut windows: Vec<(Window, Geometry)> = Vec::new();
        match self.get_windows().len() {
            // no window
            0 => {}
            // 1 window
            1 => {
                windows = vec![(self.tiles[0].window, fullscreen_geometry)];
            }
            // multiple windows
            _ => {
                let x = self.screen.width / 2;
                let width = x;

                windows.push((self.tiles[0].window,
                              Geometry {
                                  x: 0,
                                  y: 0,
                                  width: width,
                                  height: fullscreen_geometry.height,
                              }));

                let height = self.screen.height / ((self.get_windows().len() - 1) as c_uint);
                for tile_id in 1..self.get_windows().len() {
                    let y: c_uint = ((tile_id - 1) as c_uint) * height;

                    windows.push((self.tiles[tile_id].window,
                                  Geometry {
                                      x: x as i32,
                                      y: y as i32,
                                      width: width,
                                      height: height,
                                  }));
                }
            }
        }
        WindowLayout {
            focused_window: self.get_focused_window(),
            windows: windows,
        }
    }

    /// Focus the given window, or when passed None, focus nothing.
    /// This is called when the user clicks or hovers on(to) a window,
    /// or changes the focus using the keyboard.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        match window {
            Some(i_window) => {
                if self.is_managed(i_window) {
                    self.focused_window = Some(i_window);
                    Ok(())
                } else {
                    Err(WMError::UnknownWindow(i_window))
                }
            }
            None => {
                self.focused_window = None;
                Ok(())
            }
        }
    }

    /// Focus the previous or next window.
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        // You will probably notice here that a `Vec` is not the ideal data
        // structure to implement this function. Feel free to replace the
        // `Vec` with another data structure.
        let window: Option<Window>;
        match self.get_focused_window() {
            Some(focus_window) => {
                let next_position: usize;
                let len_tiles = self.tiles.len();
                match self.get_window_index(focus_window) {
                    Some(i) => {
                        match dir {
                            PrevOrNext::Prev => {
                                next_position = (i + len_tiles - 1) % len_tiles;
                            }
                            PrevOrNext::Next => {
                                next_position = (i + len_tiles + 1) % len_tiles;
                            }
                        }
                        window = Some(self.tiles[next_position].window);
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
                window = self.tiles.back().map(|w| w.window);
                // Do nothing when there are no windows.
            }
        }
        match self.focus_window(window) {
            Ok(_) => {}
            Err(e) => println!("Error focus_window {}", e),
        }
    }

    // It should reflect the current state (location/size, floating or tiled,
    // fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        //        if self.is_managed(window) {
        //            let window_layout = self.get_window_layout();
        //            match window_layout.windows.iter().position(|w| w.0 == window) {
        //                None => Err(WMError::UnknownWindow(window)),
        //                Some(i) => {
        //                    Ok(WindowWithInfo {
        //                        window: window,
        //                        geometry: window_layout.windows[i].1,
        //                        float_or_tile: FloatOrTile::Tile,
        //                        fullscreen: false,
        //                    })
        //                }
        //            }
        //        } else {
        //            Err(WMError::UnknownWindow(window))
        //        }
        if self.is_managed(window) {
            let window_layout = self.get_window_layout();
            match window_layout.windows.iter().filter(|w| w.0 == window).map(|w| *w).last() {
                None => Err(WMError::UnknownWindow(window)),
                Some(i) => {
                    match self.tiles
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
        self.screen
    }
    /// Resize the screen according to the given Screen.
    /// This is called whenever the resolution of the screen is changed.
    /// Note that we do not support multiple monitors.
    /// Invariant: after resize_screen is called with a screen, get_screen()
    /// must return the same screen.
    fn resize_screen(&mut self, screen: Screen) {
        self.screen = screen
    }
}

impl CustomSupport for TilingWM {
    /// toggle fullscreen
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        match self.get_window_index(window_with_info.window) {
            Some(i) => {
                self.tiles[i] = window_with_info;
                Ok(())
            }
            None => Err(WMError::UnknownWindow(window_with_info.window)),
        }
    }
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize> {
        self.tiles.iter().position(|w| w.window == window)
    }
}

impl TilingSupport for TilingWM {
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
        self.tiles.get(0).map(|i| i.window)
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
        match self.get_window_index(window) {
            Some(i) => {
                self.tiles.swap(0, i);
                self.focus_window(Some(window))
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
        if let Some(focus_window) = self.get_focused_window() {
            let len_tiles = self.tiles.len();
            let swap_with: usize;
            if let Some(i) = self.get_window_index(focus_window) {
                match dir {
                    PrevOrNext::Prev => {
                        swap_with = (i + len_tiles - 1) % len_tiles;
                    }
                    PrevOrNext::Next => {
                        swap_with = (i + len_tiles + 1) % len_tiles;
                    }
                }
                let tile_i = self.tiles[i];
                self.tiles[i].float_or_tile = self.tiles[swap_with].float_or_tile;
                self.tiles[i].fullscreen = self.tiles[swap_with].fullscreen;
                self.tiles[swap_with].float_or_tile = tile_i.float_or_tile;
                self.tiles[swap_with].fullscreen = tile_i.fullscreen;
                self.tiles.swap(i, swap_with);
            }
        }
    }
}
// Declare additional modules below or declare them in other modules.
// [cfg(test)]
// One failed test, fullscreen have no notion about fullscreen
// mod a_fullscreen_wm_tests { include!("a_fullscreen_wm_tests.rs");}
#[cfg(test)]
mod b_tiling_wm_tests {
    include!("b_tiling_wm_tests.rs");
}
