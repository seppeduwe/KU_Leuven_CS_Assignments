use cplwm_api::wm::WindowManager;
use cplwm_api::wm::TilingSupport;
use cplwm_api::wm::MinimiseSupport;
use cplwm_api::types::*;

use super::WMName;
// We define a static variable for the screen we will use in the tests.
// You can just as well define it as a local variable in your tests.
static SCREEN: Screen = Screen {
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
/// when focus_window succeeds, get_focused_window must return the same argument.
fn test_get_windows() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());

    assert_eq!(vec![1, 3, 2], wm.get_windows());
}

#[test]
/// The same window remains focused, unless the focused window has been removed.
/// is_managed must return false for the given window after remove_window was called
/// with the given window.
fn test_remove_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();

    assert!(wm.toggle_minimised(2).is_ok());
    wm.remove_window(2).unwrap();
    // It should no longer be managed by the WM.
    assert!(!wm.is_managed(2));
    // The `Vec` of windows should now just contain window 1.
    assert_eq!(vec![1, 3], wm.get_windows());
}
#[test]
/// **Note**: methods of other traits like `focus_window`, `focus_window`,
/// `toggle_fullscreen`, ... called with a minimised window as argument should
/// first unminimise the window.
/// when focus_window succeeds, get_focused_window must return the same argument.
fn test_focus_window() {
    // Let's make a new `FullscreenWM` with `SCREEN` as screen.
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();

    assert!(wm.toggle_minimised(1).is_ok());
    wm.focus_window(Some(1)).unwrap();
    assert_eq!(Some(1), wm.get_focused_window());
    assert!(!wm.is_minimised(1));
}
#[test]
/// Do nothing when there are no windows. When there is only one window,
/// focus it if currently no window is focused, otherwise do nothing.
/// When no window is focused, any window may become focused.
/// Cycling the focus back and forth shouldn't change the focused window.
fn test_cycle_focus() {
    let mut wm = WMName::new(SCREEN);

    // One window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(1).is_ok());
    wm.cycle_focus(PrevOrNext::Next);
    assert_eq!(Some(1), wm.get_focused_window());

    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(1).is_ok());
    wm.cycle_focus(PrevOrNext::Prev);
    assert_eq!(Some(1), wm.get_focused_window());

    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(1).is_ok());
    // Multiple windows
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(3).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();

    wm.cycle_focus(PrevOrNext::Next);
    let wl1 = wm.get_window_layout();
    assert_eq!(Some(1), wl1.focused_window);
    assert!(!wm.is_minimised(1));

    wm.cycle_focus(PrevOrNext::Prev);
    let wl2 = wm.get_window_layout();
    assert_eq!(Some(4), wl2.focused_window);

    wm.cycle_focus(PrevOrNext::Prev);
    let wl4 = wm.get_window_layout();
    assert_eq!(Some(2), wl4.focused_window);

    wm.cycle_focus(PrevOrNext::Next);
    let wl5 = wm.get_window_layout();
    assert_eq!(Some(4), wl5.focused_window);
}
#[test]
/// This function should return an appropriate error when the window
/// is not managed by the window manager.
fn test_get_window_info() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    let wl1 = wm.get_window_info(3);
    assert_eq!(Some(WindowWithInfo {
                   window: 3,
                   geometry: Geometry {
                       x: 400,
                       y: 0,
                       width: 400,
                       height: 600,
                   },
                   float_or_tile: FloatOrTile::Tile,
                   fullscreen: false,
               }),
               wl1.ok());
}

#[test]
/// After the function has succeeded, the master window should be focused.
/// If the given window is already in the master tile, no windows have to be swapped,
/// but the master window should be focused.
/// if swap_with_master(w) succeeds, get_master_window() == Some(w).
fn test_swap_with_master() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(1).is_ok());
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();

    wm.swap_with_master(1).unwrap();
    assert_eq!(wm.get_master_window(), Some(1));

    wm.swap_with_master(2).unwrap();
    assert_eq!(wm.get_master_window(), Some(2));
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

    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(1).is_ok());
    // No visible window
    wm.swap_windows(PrevOrNext::Prev);
    wm.swap_windows(PrevOrNext::Next);
    assert_eq!(WindowLayout::new(), wm.get_window_layout());

    // One visible window
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    let wl3 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: None,
                   windows: vec![(2,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 800,
                                      height: 600,
                                  })],
               },
               wl3);
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Next);
    let wl4 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: None,
                   windows: vec![(2,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (1,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  })],
               },
               wl4);
}

#[test]
/// The windows must occur in the order they were minimised: the window
/// that was minimised first must occur first in the vector, the window
/// that was minimised last must occur last. This makes it easy to define
/// a function that unminimises the last minimised window.
fn test_get_minimised_windows() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();

    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(3).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();

    assert_eq!(vec![2, 3], wm.get_minimised_windows());
}
#[test]
/// Return `true` if the given window is minimised.
///
/// This function must always return false when the given window is not
/// managed by the window manager.
///
/// if `is_minimised(w) == true` for some window `w`, then
/// `is_managed(w) == true`.
///
/// is_minimised(w) == true` for some window `w`, iff the
/// vector returned by the `get_minised_windows` method contains `w`.
fn test_is_minimised() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(3).is_ok());
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();

    assert!{!wm.is_minimised(5)};

    assert_eq!(wm.is_minimised(2), wm.is_managed(2));
    assert_eq!(wm.is_minimised(2), wm.get_minimised_windows().contains(&2));
}
#[test]
/// When a minimised floating window is unminimised, it should float again
/// and have the same geometry as before.
///
/// if calling `toggle_minimised(w)` with an unminimised
/// window `w` succeeds, `w` may no longer be visible according to
/// `get_window_layout` and `is_minimised(w)` must return `true`.
///
/// if calling `toggle_minimised(w)` with an already
/// minimised window `w` succeeds, `w` must be visible according to
/// `get_window_layout` and `is_minimised(w)` must return `false`.
///
/// The window layout before and after minimising and directly
/// unminimising the currently focused window should be the same. This
/// cannot hold for a window manager that implements
/// [`TilingSupport`](trait.TilingSupport.html). Try to figure out why.
fn test_toggle_minimised() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    assert!(wm.toggle_minimised(2).is_ok());
    assert!(wm.toggle_minimised(2).is_ok());
    assert_eq!(wm.get_window_info(2).unwrap().geometry, SOME_GEOM);

    assert!(wm.toggle_minimised(2).is_ok());
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
                     })],
               wm.get_window_layout().windows);
    assert!(wm.is_minimised(2));

    assert!(wm.toggle_minimised(2).is_ok());
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
                    (2, SOME_GEOM)],
               wm.get_window_layout().windows);
    assert!(!wm.is_minimised(2));

    let geometry = wm.get_window_info(3).unwrap().geometry;
    assert!(wm.toggle_minimised(3).is_ok());
    assert!(wm.toggle_minimised(3).is_ok());
    assert_eq!(wm.get_window_info(3).unwrap().geometry, geometry);
}
