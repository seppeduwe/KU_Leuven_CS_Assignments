use cplwm_api::wm::WindowManager;
use cplwm_api::wm::MultiWorkspaceSupport;
use cplwm_api::types::*;
use super::WMName;
// We define a static variable for the screen we will use in the tests.
// You can just as well define it as a local variable in your tests.
static SCREEN: Screen = Screen {
    width: 800,
    height: 600,
};

// We define a static variable for the geometry of a fullscreen window.
// Note that it matches the dimensions of `SCREEN`.
static SCREEN_GEOM: Geometry = Geometry {
    x: 0,
    y: 0,
    width: 800,
    height: 600,
};

// We define a static variable for some random geometry that we will use
// when adding windows to a window manager.
static SOME_GEOM: Geometry = Geometry {
    x: 10,
    y: 10,
    width: 100,
    height: 100,
};
#[test]
/// Return the current workspace index.
///
/// When creating a new workspace this will return 0.
///
/// **Invariant**: `0 <= get_current_workspace_index() <=
/// MAX_WORKSPACE_INDEX`.
fn test_get_current_workspace_index() {
    let mut wm = WMName::new(SCREEN);
    assert_eq!(wm.get_current_workspace_index(), 0);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();

    wm.switch_workspace(2).unwrap();
    assert_eq!(wm.get_current_workspace_index(), 2);

    assert!(wm.switch_workspace(MAX_WORKSPACE_INDEX + 1).is_err());

    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(1, SOME_GEOM)],
               },
               wm.get_window_layout());
    wm.switch_workspace(0).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(2),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 800,
                                      height: 600,
                                  }),
                                 (2,
                                  Geometry {
                                      x: 10,
                                      y: 10,
                                      width: 100,
                                      height: 100,
                                  })],
               },
               wm.get_window_layout());
}
#[test]
/// Get an immutable borrow of the workspace at the given index.
///
/// This function *should* return an appropriate error when `0 <= index <=
/// MAX_WORKSPACE_INDEX` is not true.
fn test_get_workspace() {
    let wm = WMName::new(SCREEN);
    let ws = wm.get_workspace(1).unwrap();
    assert_eq!(wm.window_managers.len(), MAX_WORKSPACE_INDEX + 1);
    assert_eq!(WindowLayout::new(), ws.get_window_layout());
    assert!(wm.get_workspace(MAX_WORKSPACE_INDEX).is_ok());
    assert!(wm.get_workspace(MAX_WORKSPACE_INDEX + 1).is_err());
}
#[test]
/// Get a mutable borrow of the workspace at the given index.
///
/// This function *should* return an appropriate error when `0 <= index <=
/// MAX_WORKSPACE_INDEX` is not true.
fn test_get_workspace_mut() {
    let mut wm = WMName::new(SCREEN);
    {
        let mut ws = wm.get_workspace_mut(1).unwrap();
        ws.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
        assert_eq!(WindowLayout {
                       focused_window: Some(1),
                       windows: vec![(1, SCREEN_GEOM)],
                   },
                   ws.get_window_layout());
    }

    {
        wm.switch_workspace(1).unwrap();
    }
    {
        assert_eq!(WindowLayout {
                       focused_window: Some(1),
                       windows: vec![(1, SCREEN_GEOM)],
                   },
                   wm.get_window_layout());
    }
    assert!(wm.get_workspace_mut(MAX_WORKSPACE_INDEX + 1).is_err());
}

#[test]
/// Switch to the workspace at the given index.
///
/// If `index == get_current_workspace_index()`, do nothing.
///
/// the window layout after switching to another workspace
/// and then switching back to the original workspace should be the same
/// as before.
///
/// This function *should* return an appropriate error when `0 <= index <=
/// MAX_WORKSPACE_INDEX` is not true.
fn test_switch_workspace() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    let window_layout = wm.get_window_layout();
    wm.switch_workspace(0).unwrap();
    assert_eq!(wm.get_current_workspace_index(), 0);
    assert_eq!(window_layout, wm.get_window_layout());

    wm.switch_workspace(1).unwrap();
    wm.switch_workspace(0).unwrap();
    assert_eq!(window_layout, wm.get_window_layout());

    assert!(wm.switch_workspace(MAX_WORKSPACE_INDEX + 1).is_err());
}
