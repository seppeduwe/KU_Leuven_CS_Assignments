use cplwm_api::wm::FloatSupport;
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::FullscreenSupport;
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
/// if `get_fullscreen_window() == Some(w)`, then
/// `is_managed(w) == true`.
///
/// if `get_fullscreen_window() == Some(w)`, then
/// `get_focused_window() == Some(w)`.
fn test_get_fullscreen_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_fullscreen(2, SOME_GEOM)).unwrap();
    assert_eq!(Some(2), wm.get_fullscreen_window());
    assert!(wm.is_managed(2));

    assert_eq!(wm.get_fullscreen_window(), wm.get_focused_window());

    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert_eq!(None, wm.get_fullscreen_window());
}
#[test]
/// When called on a window that is already fullscreen, it should restore
/// the window to the state before, e.g. float at the same place.
///
/// if calling `toggle_fullscreen(w)` with a window `w`
/// that is not yet fullscreen, `w` should be the only visible window
/// according to `get_window_layout`, its geometry should be the same size
/// as the screen, and `get_fullscreen_window(w) == Some(w)`.
///
/// TODOOOOOOOOOOOOOO!
/// The window layout before and after calling `toggle_fullscreen` twice
/// with the currently focused should be the same. This cannot hold for a
/// window manager that implements
/// [`TilingSupport`](trait.TilingSupport.html). Try to figure out why.
fn test_toggle_fullscreen() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(3, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();
    wm.toggle_fullscreen(3).unwrap();
    assert_eq!(SCREEN_GEOM, wm.get_window_info(3).unwrap().geometry);
    wm.toggle_fullscreen(3).unwrap();
    assert_eq!(SOME_GEOM, wm.get_window_info(3).unwrap().geometry);

    wm.toggle_fullscreen(4).unwrap();
    assert_eq!(vec![(4, SCREEN_GEOM)], wm.get_window_layout().windows);
    assert_eq!(wm.get_fullscreen_window(), Some(4));

    let prev_window_layout = wm.get_window_layout();
    wm.toggle_fullscreen(4).unwrap();
    wm.toggle_fullscreen(4).unwrap();
    assert_eq!(wm.get_window_layout(), prev_window_layout);
}
#[test]
/// If already a fullscreen is shown,
/// set on state before and add window
fn test_add_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_fullscreen(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(2),
                   windows: vec![(1, SCREEN_GEOM), (2, SOME_GEOM)],
               },
               wm.get_window_layout());
    assert_eq!(wm.get_fullscreen_window(), None);
}
#[test]
/// Only 1 window visible
fn test_get_window_layout() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_fullscreen(2, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(2),
                   windows: vec![(2, SCREEN_GEOM)],
               },
               wm.get_window_layout());
}
#[test]
/// If a window is fullscreen then set on state before
/// and focus window.
fn test_focus_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_fullscreen(3, SOME_GEOM)).unwrap();
    wm.focus_window(Some(1)).unwrap();

    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(2,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (3,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (1, SOME_GEOM)],
               },
               wm.get_window_layout());
}
#[test]
/// Set fullscreen mode back to the state before
fn test_cycle_focus() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_fullscreen(3, SOME_GEOM)).unwrap();
    wm.cycle_focus(PrevOrNext::Next);
    assert_eq!(wm.get_fullscreen_window(), None);
}
#[test]
/// Test when fullscreen, it gives the screen geom
fn test_get_window_info() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_fullscreen(1, SOME_GEOM)).unwrap();
    assert_eq!(SCREEN_GEOM, wm.get_window_info(1).unwrap().geometry);
}
#[test]
/// The window layout should reflect the geometry change of the floating
/// window.
fn test_set_window_geometry() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.toggle_fullscreen(1).unwrap();
    let random_geo = Geometry {
        x: 10,
        y: 20,
        width: 30,
        height: 40,
    };
    wm.set_window_geometry(1, random_geo).unwrap();
    assert_eq!(random_geo, wm.get_window_info(1).unwrap().geometry);
}
