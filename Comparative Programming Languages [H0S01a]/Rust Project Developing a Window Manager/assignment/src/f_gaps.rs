//! Optional: Gaps
//!
//! Extend your window manager with support for gaps, i.e. the ability to add
//! some space between the different tiles. See the documentation of the
//! [`GapSupport`] trait for the precise requirements.
//!
//! Make a copy of your tiling window manager from assignment B and let it
//! implement the [`GapSupport`] trait. You are not required to let this
//! window manager implement all the previous traits.
//!
//! [`GapSupport`]: ../../cplwm_api/wm/trait.GapSupport.html
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
//! COMMENTS: Using the Tiling window manager and just edit the
//! get_window_layout method. The tests are in a separated file.
//!
//! ...
//!

// Add imports here

// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{GapSize, PrevOrNext, Screen, Window, WindowLayout, WindowWithInfo};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::wm::GapSupport;
use custom_trait::CustomSupport;
use b_tiling_wm::TilingWM;
use wm_error::WMError;
/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
pub type WMName = FGaps;
// pub trait TilingSupportWrapper: TilingWM + FloatSupport {}
// extern crate TilingWM;
/// Struct TilingWM
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct FGaps {
    /// Used the previous Tiling window manager
    pub tiling_wm: TilingWM,
    /// Space between the different tiles.
    pub gap_size: GapSize,
}

impl WindowManager for FGaps {
    type Error = WMError;

    /// Track the gap size and make use of the TilingWM functionaluty.
    fn new(screen: Screen) -> FGaps {
        FGaps {
            tiling_wm: TilingWM::new(screen),
            gap_size: 0,
        }
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
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        self.tiling_wm.remove_window(window)
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        let mut old_layout = self.tiling_wm.get_window_layout();
        for i in old_layout.windows.iter_mut() {
            i.1.x += self.gap_size as i32;
            i.1.y += self.gap_size as i32;
            i.1.width -= 2 * self.gap_size;
            i.1.height -= 2 * self.gap_size;
        }
        old_layout
    }

    /// Focus the given window, or when passed None, focus nothing.
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
        let mut i = try!(self.tiling_wm.get_window_info(window));
        i.geometry.x += self.gap_size as i32;
        i.geometry.y += self.gap_size as i32;
        i.geometry.width -= 2 * self.gap_size;
        i.geometry.height -= 2 * self.gap_size;
        Ok(i)
    }

    /// Return the screen managed by the window manager.
    fn get_screen(&self) -> Screen {
        self.tiling_wm.get_screen()
    }
    /// Resize the screen according to the given Screen.
    fn resize_screen(&mut self, screen: Screen) {
        self.tiling_wm.resize_screen(screen)
    }
}
impl CustomSupport for FGaps {
    /// toggle fullscreen
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.tiling_wm.set_window(window_with_info)
    }
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize> {
        self.tiling_wm.get_window_index(window)
    }
}

impl TilingSupport for FGaps {
    /// Return the window displayed in the master tile.
    fn get_master_window(&self) -> Option<Window> {
        self.tiling_wm.get_master_window()
    }

    /// Swap the given window with the window in the master tile.
    fn swap_with_master(&mut self, window: Window) -> Result<(), Self::Error> {
        self.tiling_wm.swap_with_master(window)
    }

    /// Swap the focused window with the one in the next or previous tile.
    fn swap_windows(&mut self, dir: PrevOrNext) {
        self.tiling_wm.swap_windows(dir)
    }
}

/// A window manager that supports gaps between tiles.
///
/// The user can configure the gap size at run-time. The gaps are only shown
/// between tiled windows. Floating windows and the fullscreen window are
/// unaffected. Initially the gap is 0.
///
/// For example the user has the following tiles:
///
/// +----------+----------+
/// |          |          |
/// |          |          |
/// |    1     |    2     |
/// |          |          |
/// |          |          |
/// +----------+----------+
///
/// When the gap is set to 5, it is as if every window gets an invisible 5
/// pixel border of empty space. Windows don't share the gap in between them.
/// Every tiled window is moved 5 pixels down and to 5 pixels to the right.
/// The width and height of every tiled window is also shrunk by 10 pixels.
///
/// +---------------------+
/// |+--------+ +--------+|
/// ||        | |        ||
/// ||    1   | |   2    ||
/// ||        | |        ||
/// |+--------+ +--------+|
/// +---------------------+
///
/// Even when there is only a single tile there should be a gap around it.
///
/// You may ignore scenarios in which the gap size is so large that one of the
/// windows might become invisible.
///
/// Implementors of this trait must adapt their implementation of the
/// `get_window_layout()` method accordingly.
impl GapSupport for FGaps {
    /// Return the current gap size.
    ///
    /// Initially 0.
    fn get_gap(&self) -> GapSize {
        self.gap_size
    }

    /// Set the gap size.
    ///
    /// **Invariant**: after setting `set_gap(g)` with some gap size `g`,
    /// `get_gap() == g`.
    fn set_gap(&mut self, gap_size: GapSize) {
        self.gap_size = gap_size
    }
}
// Declare additional modules below or declare them in other modules.
#[cfg(test)]
mod b_tiling_wm_tests {
    include!("b_tiling_wm_tests.rs");
}
#[cfg(test)]
mod f_gaps_tests {
    include!("f_gaps_tests.rs");
}
