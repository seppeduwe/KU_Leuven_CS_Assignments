// We import std::error and std::format so we can say error::Error instead of
// std::error::Error, etc.
use std::error;
use std::fmt;

// Import some types and the WindowManager trait from the cplwm_api crate
// (defined in the api folder).
use cplwm_api::types::{Window, WorkspaceIndex};

/// Struct WMError
#[derive(Debug)]
pub enum WMError {
    /// This window is not known by the window manager.
    UnknownWindow(Window),
    /// Window Already Managed
    WindowAlreadyManaged(Window),
    /// This workspace is not known by the window manager.
    UnknownWorkspace(WorkspaceIndex),
}

// This code is explained in the documentation of the associated [Error] type
// of the `WindowManager` trait.
impl fmt::Display for WMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            WMError::UnknownWindow(ref window) => write!(f, "Unknown window: {}", window),
            WMError::WindowAlreadyManaged(ref window) => {
                write!(f, "Window already managed: {}", window)
            }
            WMError::UnknownWorkspace(ref workspace) => {
                write!(f, "Unknown workspace: {}", workspace)
            }

        }
    }
}

// This code is explained in the documentation of the associated [Error] type
// of the `WindowManager` trait.
impl error::Error for WMError {
    fn description(&self) -> &'static str {
        match *self {
            WMError::UnknownWindow(_) => "Unknown window",
            WMError::WindowAlreadyManaged(_) => "Window Already Managed",
            WMError::UnknownWorkspace(_) => "Unknown Workspace",
        }
    }
}
