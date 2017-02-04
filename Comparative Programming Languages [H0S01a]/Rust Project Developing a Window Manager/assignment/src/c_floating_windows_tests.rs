use cplwm_api::wm::FloatSupport;
use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
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
/// Return a vector of all the visible floating windows.
///
/// The order of the windows in the vector does not matter.
fn test_get_floating_windows() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert_eq!(vec![2], wm.get_floating_windows());
}
#[test]
/// if `is_floating(w) == true` for some window `w`, then
/// `is_managed(w) == true`.
///
/// `is_floating(w) == true` for some window `w`, iff the
/// vector returned by the `get_floating_windows` method contains `w`.
fn test_is_floating() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(!wm.is_floating(1));
    assert!(wm.is_floating(2));
    assert!(wm.get_floating_windows().contains(&2));
}
#[test]
/// If the given window is floating, let it *sink*, if it is not floating,
/// let it *float*.
///
/// When a non-floating window starts to float, its original geometry
/// (passed to `add_window`) should be restored.
///
/// if calling `toggle_floating(w)` with a tiled window `w`
/// succeeds, `is_floating(w)` must return `true`.
///
/// if calling `toggle_floating(w)` with a floating window
/// `w` succeeds, `is_floating(w)` must return `false`.
///
/// the result of `is_floating(w)` must be the same before
/// and after calling `toggle_floating(w)` twice.
fn test_toggle_floating() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();

    assert!(wm.toggle_floating(3).is_ok());
    assert!(wm.is_floating(3));

    assert!(wm.toggle_floating(1).is_ok());
    assert!(!wm.is_floating(1));

    assert!(wm.toggle_floating(2).is_ok());
    assert!(wm.toggle_floating(2).is_ok());
    assert!(wm.is_floating(2));

    assert_eq!(wm.get_window_info(3).unwrap().geometry, SOME_GEOM);

    assert_eq!(vec![2, 3], wm.get_floating_windows());

    assert!(wm.toggle_floating(4).is_err());

    assert_eq!(WindowLayout {
                   focused_window: Some(3),
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
                                  }),
                                 (3,
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
/// The window layout should reflect the geometry change of the floating
/// window.
///
/// This function is *allowed* to return an appropriate error when the
/// window is not managed by the window manager *or* when the window is
/// not floating.
fn test_set_window_geometry() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();

    let random_geo = Geometry {
        x: 50,
        y: 60,
        width: 70,
        height: 80,
    };
    assert!(wm.set_window_geometry(1, random_geo)
        .is_ok());

    assert!(wm.set_window_geometry(3, random_geo)
        .is_err());
    assert!(wm.set_window_geometry(4, random_geo)
        .is_err());
}
//------------------------------
//-New requirements for older functions
//------------------------------
#[test]
/// when focus_window succeeds, get_focused_window must return the same argument.
fn test_focus_window() {
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();

    wm.focus_window(Some(1)).unwrap();
    assert_eq!(Some(1), wm.get_focused_window());
    // Focused window, is the last.
    assert_eq!((1, SOME_GEOM),
               wm.get_window_layout().windows.last().map(|&x| x).unwrap());

    wm.focus_window(None).unwrap();
    assert_eq!(None, wm.get_focused_window());

    // Don't change order, after focus_window(None)
    assert_eq!((1, SOME_GEOM),
               wm.get_window_layout().windows.last().map(|&x| x).unwrap());
}

#[test]
/// Do nothing when there are no windows. When there is only one window,
/// focus it if currently no window is focused, otherwise do nothing.
/// When no window is focused, any window may become focused.
/// Cycling the focus back and forth shouldn't change the focused window.
fn test_cycle_focus() {
    let mut wm = WMName::new(SCREEN);
    // One window
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.cycle_focus(PrevOrNext::Next);
    assert_eq!(Some(1), wm.get_focused_window());

    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.cycle_focus(PrevOrNext::Prev);
    assert_eq!(Some(1), wm.get_focused_window());

    // Multiple windows
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(4, SOME_GEOM)).unwrap();

    let wl1 = wm.get_window_layout();
    assert_eq!(Some(4), wl1.focused_window);
    wm.cycle_focus(PrevOrNext::Next);
    let wl2 = wm.get_window_layout();
    assert_eq!(Some(1), wl2.focused_window);
    // Don't change the windows order
    assert_eq!(wl1.windows, wl2.windows);

    wm.cycle_focus(PrevOrNext::Prev);
    let wl3 = wm.get_window_layout();
    assert_eq!(Some(4), wl3.focused_window);
    assert_eq!(vec![(1,
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
                    (2, SOME_GEOM),
                    (4, SOME_GEOM)],
               wl3.windows);

    wm.cycle_focus(PrevOrNext::Prev);
    let wl4 = wm.get_window_layout();
    assert_eq!(Some(3), wl4.focused_window);
    assert_eq!(vec![(1,
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
                    (2, SOME_GEOM),
                    (4, SOME_GEOM)],
               wl3.windows);

    wm.cycle_focus(PrevOrNext::Next);
    let wl5 = wm.get_window_layout();
    assert_eq!(Some(4), wl5.focused_window);
}
#[test]
/// Do nothing when there are no windows, when there is only one window,
/// or when no window is focused.
/// If there were two tiles and the swap happened, the same window will be focused,
/// but the other tile will be focused.
/// calling swap_windows(dir) for any dir will not change the focused window,
/// even if no window was focused.
/// calling swap_windows(dir) and then swap_windows(dir.opposite()) will not change the
/// window layout.
fn test_swap_windows() {
    let mut wm = WMName::new(SCREEN);

    // One window
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    wm.swap_windows(PrevOrNext::Next);
    let wl1 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(1, SOME_GEOM)],
               },
               wl1);

    // No window focused
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.focus_window(None).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    wm.swap_windows(PrevOrNext::Next);
    let wl2 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: None,
                   windows: vec![(2, SCREEN_GEOM), (1, SOME_GEOM)],
               },
               wl2);
    // Test swap
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    let wl3 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(4),
                   windows: vec![(2,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 300,
                                  }),
                                 (3,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 400,
                                      height: 300,
                                  }),
                                 (1, SOME_GEOM)],
               },
               wl3);
    wm.swap_windows(PrevOrNext::Next);
    let wl4 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(4),
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
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 400,
                                      height: 300,
                                  }),
                                 (1, SOME_GEOM)],
               },
               wl4);
    assert_eq!(wm.get_focused_window(), Some(4));

    let dir = PrevOrNext::Next;
    wm.swap_windows(dir);
    wm.swap_windows(dir.opposite());
    assert_eq!(wl4, wm.get_window_layout());
}
#[test]
/// get_master_window() == Some(w), then w must occur in the vector returned by get_windows().
/// if the vector returned by get_windows() is empty => get_master_window() == None.
/// The other direction of the arrow must not hold, e.g., there could floating windows
/// see FloatSupport), but no tiled windows.
fn test_get_master_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_float(1, SOME_GEOM)).unwrap();
    assert_eq!(wm.get_master_window(), None);
    assert!(!wm.get_windows().is_empty());
}
