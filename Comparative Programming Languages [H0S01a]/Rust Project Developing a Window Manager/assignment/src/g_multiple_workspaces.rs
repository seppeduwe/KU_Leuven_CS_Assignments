//! Optional: Multiple Workspaces
//!
//! Extend your window manager with support for multiple workspaces. The
//! concept of workspaces is described in the first section of the assignment.
//! See the documentation of the [`MultiWorkspaceSupport`] trait for the precise
//! requirements.
//!
//! *Unlike* the previous assignments, you are not allowed to make a copy of
//! your previous window manager. You *have* to define a wrapper implementing
//! the [`MultiWorkspaceSupport`] trait. This wrapper can take any existing
//! window manager and uses it to create the different workspaces. This
//! wrapper must also implement all the traits you have implemented in the
//! other assignments, you can forward them to the window manager of the
//! current workspace.
//!
//! [`MultiWorkspaceSupport`]: ../../cplwm_api/wm/trait.MultiWorkspaceSupport.html
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
//! COMMENTS: Used the FullscreenWS as the basis for this. The test are in a
//! separated file.
//!
//! ...
//!

// Add imports here
use cplwm_api::types::{MAX_WORKSPACE_INDEX, PrevOrNext, Screen, Window, WindowLayout,
                       WindowWithInfo, WorkspaceIndex};
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::wm::MinimiseSupport;
use cplwm_api::wm::FullscreenSupport;
use cplwm_api::wm::MultiWorkspaceSupport;
use cplwm_api::types::Geometry;
use cplwm_api::wm::FloatSupport;
use wm_error::WMError;
use e_fullscreen_windows::FullscreenWS;
use std::iter::repeat;
/// **TODO**: You are free to choose the name for your window manager. As we
/// will use automated tests when grading your assignment, indicate here the
/// name of your window manager data type so we can just use `WMName` instead
/// of having to manually figure out your window manager name.
///
/// Replace `()` with the name of your window manager data type.
///
/// Replace the `WM` type parameter with the name of the window manager from
/// assignment E or the window manager of the last assignment you completed
/// (except F). E.g. use `type WMName =
/// MultiWorkspaceWM<e_fullscreen_windows::WMName>`.
pub type WMName = MultiWorkspaceWM<FullscreenWS>;
/// Use all FullscreenWS
pub type WM = FullscreenWS;
/// Struct MultiWorkspaceWM
#[derive(RustcDecodable, RustcEncodable, Debug, Clone)]
pub struct MultiWorkspaceWM<WM> {
    /// Use minimisingWS
    pub window_managers: Vec<WM>,
    /// The active workspace
    pub active_workspace: WorkspaceIndex,
}

impl WindowManager for MultiWorkspaceWM<WM> {
    type Error = WMError;

    /// Track the active workspaec, and create MAX_WORKSPACE_INDEX+1 workspaces .
    fn new(screen: Screen) -> MultiWorkspaceWM<WM> {
        MultiWorkspaceWM {
            window_managers: repeat(WM::new(screen)).take(MAX_WORKSPACE_INDEX + 1).collect(),
            active_workspace: 0,
        }
    }

    /// The `windows` field contains all the windows we manage.
    fn get_windows(&self) -> Vec<Window> {
        self.get_current_workspace().get_windows()
    }

    /// Get focused window.
    fn get_focused_window(&self) -> Option<Window> {
        self.get_current_workspace().get_focused_window()
    }

    /// To add a window, just push it onto the end the `windows` `Vec`.
    fn add_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().add_window(window_with_info)
    }

    /// To remove a window, just remove it from the `windows` `Vec`.
    fn remove_window(&mut self, window: Window) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().remove_window(window)
    }

    /// Return the current desired window layout.
    fn get_window_layout(&self) -> WindowLayout {
        self.get_current_workspace().get_window_layout()
    }

    /// Focus the given window, or when passed None, focus nothing.
    fn focus_window(&mut self, window: Option<Window>) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().focus_window(window)
    }

    /// Focus the previous or next window.
    fn cycle_focus(&mut self, dir: PrevOrNext) {
        self.get_current_workspace_mut().cycle_focus(dir)
    }

    /// It should reflect the current state (location/size, floating or tiled,
    /// fullscreen or not) of the window.
    fn get_window_info(&self, window: Window) -> Result<WindowWithInfo, Self::Error> {
        self.get_current_workspace().get_window_info(window)
    }

    /// Return the screen managed by the window manager.
    fn get_screen(&self) -> Screen {
        // Just take a one screen
        self.get_current_workspace().get_screen()
    }
    /// Resize the screen according to the given Screen.
    fn resize_screen(&mut self, screen: Screen) {
        for window_manager in self.window_managers.iter_mut() {
            window_manager.resize_screen(screen)
        }
    }
}

impl TilingSupport for MultiWorkspaceWM<WM> {
    /// Return the window displayed in the master tile.
    fn get_master_window(&self) -> Option<Window> {
        self.get_current_workspace().get_master_window()
    }

    /// Swap the given window with the window in the master tile.
    fn swap_with_master(&mut self, window: Window) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().swap_with_master(window)
    }

    /// Swap the focused window with the one in the next or previous tile.
    fn swap_windows(&mut self, dir: PrevOrNext) {
        self.get_current_workspace_mut().swap_windows(dir)
    }
}

impl FloatSupport for MultiWorkspaceWM<WM> {
    /// Return a vector of all the visible floating windows.
    fn get_floating_windows(&self) -> Vec<Window> {
        self.get_current_workspace().get_floating_windows()
    }

    /// If the given window is floating, let it *sink*, if it is not floating,
    /// let it *float*.
    fn toggle_floating(&mut self, window: Window) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().toggle_floating(window)
    }

    /// Resize/move the given floating window according to the given geometry.
    fn set_window_geometry(&mut self,
                           window: Window,
                           new_geometry: Geometry)
                           -> Result<(), Self::Error> {
        self.get_current_workspace_mut().set_window_geometry(window, new_geometry)
    }
}


/// A window manager that supports (un)minimising windows.
impl MinimiseSupport for MultiWorkspaceWM<WM> {
    /// Return a vector of all the minimised windows.
    fn get_minimised_windows(&self) -> Vec<Window> {
        self.get_current_workspace().get_minimised_windows()
    }

    /// Minimise the given window, or when it is already minimised, unminise
    /// it.
    fn toggle_minimised(&mut self, window: Window) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().toggle_minimised(window)
    }
}

/// A window manager that supports fullscreen windows.
impl FullscreenSupport for MultiWorkspaceWM<WM> {
    /// Return the current fullscreen, if any.
    fn get_fullscreen_window(&self) -> Option<Window> {
        self.get_current_workspace().get_fullscreen_window()
    }

    /// Make the given window fullscreen, or when it is already fullscreen,
    /// undo it.
    fn toggle_fullscreen(&mut self, window: Window) -> Result<(), Self::Error> {
        self.get_current_workspace_mut().toggle_fullscreen(window)
    }
}

/// A window manager that has multiple workspaces.
///
/// For a small introduction to workspaces, see the first section of the
/// assignment.
///
/// The idea is that there are a number of different workspaces
/// ([`MAX_WORKSPACE_INDEX`] + 1), each represented by a different copy of a
/// single type of window manager. Initially the first (index 0) workspace is
/// active. The user interacts with this workspace's window manager, e.g.,
/// adding/removing windows, focusing a window, etc. When the user then
/// switches to another workspace, all the windows will be hidden. The user
/// can then interact with this workspace as if it is a new one. When the user
/// switches back to the first workspace, the windows previously opened in
/// this workspace are shown again. When the user switches back to the other
/// workspace, the windows previously opened in this workspace are shown
/// again.
///
/// [`MAX_WORKSPACE_INDEX`]: ../types/static.MAX_WORKSPACE_INDEX.html
///
/// This is useful when multitasking: one workspace is for surfing the web,
/// another for working on a CPL assignment, a third one for working on your
/// thesis, etc.
///
/// Think carefully about the interaction between this trait and the others.
/// Most actions must only be executed on the current workspace, e.g.,
/// `add_window`, but some must be executed on all workspaces, e.g., `set_gap`
/// or `resize_screen`. Even the getters are non-obvious: should `get_windows`
/// return the windows of all workspaces or only the current one? What about
/// `get_floating_windows`? Try to make reasonable and consistent choices
/// (keep the invariants of this *and* the other traits in mind). Document the
/// choices and write test cases for them.
impl MultiWorkspaceWM<WM> {
    fn get_current_workspace_mut(&mut self) -> &mut WM {
        let workspace_index = self.get_current_workspace_index();
        self.get_workspace_mut(workspace_index).unwrap()
    }

    fn get_current_workspace(&self) -> &WM {
        let workspace_index = self.get_current_workspace_index();
        self.get_workspace(workspace_index).unwrap()
    }
}

impl MultiWorkspaceSupport<WM> for MultiWorkspaceWM<WM> {
    /// Return the current workspace index.
    ///
    /// When creating a new workspace this will return 0.
    ///
    /// **Invariant**: `0 <= get_current_workspace_index() <=
    /// MAX_WORKSPACE_INDEX`.
    fn get_current_workspace_index(&self) -> WorkspaceIndex {
        self.active_workspace
    }

    /// Get an immutable borrow of the workspace at the given index.
    ///
    /// This function *should* return an appropriate error when `0 <= index <=
    /// MAX_WORKSPACE_INDEX` is not true.
    fn get_workspace(&self, index: WorkspaceIndex) -> Result<&WM, Self::Error> {
        self.window_managers.get(index).ok_or(WMError::UnknownWorkspace(index))
    }

    /// Get a mutable borrow of the workspace at the given index.
    ///
    /// This function *should* return an appropriate error when `0 <= index <=
    /// MAX_WORKSPACE_INDEX` is not true.
    fn get_workspace_mut(&mut self, index: WorkspaceIndex) -> Result<&mut WM, Self::Error> {
        self.window_managers.get_mut(index).ok_or(WMError::UnknownWorkspace(index))
    }

    /// Switch to the workspace at the given index.
    ///
    /// If `index == get_current_workspace_index()`, do nothing.
    ///
    /// **Invariant**: the window layout after switching to another workspace
    /// and then switching back to the original workspace should be the same
    /// as before.
    ///
    /// This function *should* return an appropriate error when `0 <= index <=
    /// MAX_WORKSPACE_INDEX` is not true.
    fn switch_workspace(&mut self, index: WorkspaceIndex) -> Result<(), Self::Error> {
        // usize only positive numbers
        if index <= MAX_WORKSPACE_INDEX {
            self.active_workspace = index;
            Ok(())
        } else {
            Err(WMError::UnknownWorkspace(index))
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
#[cfg(test)]
mod e_fullscreen_windows_tests {
    include!("e_fullscreen_windows_tests.rs");
}
#[cfg(test)]
mod g_multiple_workspaces_tests {
    include!("g_multiple_workspaces_tests.rs");
}
