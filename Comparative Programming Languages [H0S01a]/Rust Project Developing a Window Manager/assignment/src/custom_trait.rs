use cplwm_api::wm::WindowManager;
use cplwm_api::types::Window;
use cplwm_api::types::WindowWithInfo;
/// My trait
pub trait CustomSupport: WindowManager {
    /// Set window
    fn set_window(&mut self, window_with_info: WindowWithInfo) -> Result<(), Self::Error>;
    /// Get window index
    fn get_window_index(&self, window: Window) -> Option<usize>;
}
