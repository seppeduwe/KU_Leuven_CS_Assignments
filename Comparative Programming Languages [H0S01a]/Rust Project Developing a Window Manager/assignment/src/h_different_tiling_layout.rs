//! Optional: Different Tiling Layout
//!
//! Come up with a different tiling layout algorithm than the one you have
//! already implemented. If you are uninspired, feel free to look for one on
//! the [internet], but *don't forget to mention where you found it*. The
//! layout algorithm *may not be trivial*, e.g., not just adding tiles by
//! splitting the screen horizontally, and must be at least as complex as, but
//! different enough from the original layout algorithm you already had to
//! implement.
//!
//! Make a copy of your tiling window manager that implements the tiling
//! layout algorithm. This window manager has to implement the
//! [`WindowManager`] trait, but *not necessarily* the [`TilingSupport`]
//! trait, as not every layout has a master tile. Feel free to add additional
//! methods to your window manager that can be used to manipulate its layout.
//! You are not required to let this window manager implement all the previous
//! traits.
//!
//! [internet]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html
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
//! COMMENTS: As the basis I used the Tiling window manager and changed it,
//! so I got a spiral of windows
//!
//! +---------------------+
//! |                     |
//! |                     |
//! |          1          |
//! |                     |
//! |                     |
//! +---------------------+
//!
//! When a second window is added, the screen is split in two tiles: a left
//! tile for window 1 and a right tile for window 2.
//!
//! +----------+----------+
//! |          |          |
//! |          |          |
//! |    1     |    2     |
//! |          |          |
//! |          |          |
//! +----------+----------+
//!
//! +----------+----------+
//! |          |    2     |
//! |          +          +
//! |    1     |----------|
//! |          +    3     +
//! |          |          |
//! +----------+----------+
//!
//! +----------+----------+
//! |          |    2     |
//! |          +          +
//! |    1     |----------|
//! |          +  4  |  3 +
//! |          |     |    |
//! +----------+----------+
//!
//! +----------+----------+
//! |          |    2     |
//! |          +          +
//! |          |          |
//! |    1     |----------|
//! |          +  5  |    +
//! |          |-----|  3 |
//! |          |  4  |    |
//! +----------+----------+
//!
//! +----------+----------+
//! |          |    2     |
//! |          +          +
//! |          |          |
//! |    1     |----------|
//! |          + 5|6 |    +
//! |          |-----|  3 |
//! |          |  4  |    |
//! +----------+----------+
//! ... And it continous in spiral form.
//! Inspiration from this image: https://static.charlieharvey.org.uk/graphics/geekery/xmonad-1.png
//!


/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = DiffTilingLayout;

use std::os::raw::c_int;
use cplwm_api::types::{PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::types::Geometry;
use b_tiling_wm::TilingWM;
use wm_error::WMError;
/// Struct DiffTilingLayout
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct DiffTilingLayout {
    /// Based on TilingWM
    pub tiling_wm: TilingWM,
}

impl WindowManager for DiffTilingLayout {
    type Error = WMError;

    /// Use the TilingWM.
    fn new(screen: Screen) -> DiffTilingLayout {
        DiffTilingLayout { tiling_wm: TilingWM::new(screen) }
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
        let mut windows: Vec<(Window, Geometry)> = Vec::new();
        let n = self.get_windows().len() as u32;
        if n != 0 {
            let mut x: c_int = 0;
            let mut y: c_int = 0;
            let mut width = self.tiling_wm.screen.width;
            let mut height = self.tiling_wm.screen.height;
            for (window, i) in self.get_windows().into_iter().enumerate() {
                if (window as u32) < n - 1 {
                    if window % 2 == 0 {
                        width /= 2;
                    } else {
                        height /= 2;
                    }
                    if (window % 4) == 2 {
                        x += width as c_int;
                    } else if (window % 4) == 3 {
                        y += height as c_int;
                    }
                }
                if (window % 4) == 0 {
                    y -= height as c_int;
                } else if (window % 4) == 1 {
                    x += width as c_int
                } else if (window % 4) == 2 {
                    y += height as c_int;
                } else if (window % 4) == 3 {
                    x -= width as c_int;
                }
                if window == 0 {
                    if n != 1 {
                        width = self.tiling_wm.screen.width / 2;
                    }
                    y = 0;
                } else if window == 1 {
                    width = self.tiling_wm.screen.width - width;
                }
                windows.push((i,
                              Geometry {
                                  x: x as c_int,
                                  y: y as c_int,
                                  width: width as u32,
                                  height: height as u32,
                              }));
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
        self.tiling_wm.focus_window(window)
    }

    /// Focus the previous or next window.
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        self.tiling_wm.cycle_focus(dir)
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

impl TilingSupport for DiffTilingLayout {
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
        self.tiling_wm.get_master_window()
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
        self.tiling_wm.swap_with_master(window)
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
#[cfg(test)]
mod b_tiling_wm_tests {
    // include!("b_tiling_wm_tests.rs");
}

#[cfg(test)]
mod h_different_tiling_layout_tests {
    include!("h_different_tiling_layout_tests.rs");
}
