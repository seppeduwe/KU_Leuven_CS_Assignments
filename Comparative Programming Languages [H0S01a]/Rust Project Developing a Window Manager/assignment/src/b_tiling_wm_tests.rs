// We have to repeat the imports we did in the super module.
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
/// Test the Tilingwm constructor
fn test_wm_constructor() {
    // Let's make a new `TilingWindowManager` with `SCREEN` as screen.
    WMName::new(SCREEN);
}

#[test]
/// This window manager follows the following the tiling layout: we start out
/// with no windows. When the first window is added, the window is maximised
/// as in the ASCII diagram below.
///
/// When a second window is added, the screen is split in two tiles: a left
/// tile for window 1 and a right tile for window 2.
///
/// When a third window is added, the right tile will split in two tiles: a
/// top tile for window 2 and a bottom tile for window 3.
///
/// The left tile will never be split, we call this the *master tile*. The
/// user typically places his/her main application in this tile, e.g., the
/// browser or editor. Additional windows, e.g. a terminal, a file manager, or
/// a chat window are displayed in the side tiles. Note that even when the
/// master tile is focused, new windows will not appear in the master tile,
/// but in a new side tile.
///
/// When a fourth window is added, an additional tile is created on the right
/// side and the new window is placed in the bottom tile.
///
/// When window 2, 3, or 4 is closed, the corresponding tile disappears and we
/// go back to the previous layout. When window 1 is closed, the first side
/// window (2) is chosen to be displayed in the master tile. The promoted
/// window's previous tile disappears.
fn test_get_window_layout() {
    let mut wm = WMName::new(SCREEN);
    // Zero windows
    wm.get_window_layout();
    // Initially the window layout should be empty.
    let wl1 = wm.get_window_layout();
    assert_eq!(WindowLayout::new(), wl1);
    // One window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    let wl1 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(1, SCREEN_GEOM)],
               },
               wl1);
    // Two windows
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    let wl2 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(2),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (2,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  })],
               },
               wl2);
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    let wl3 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(3),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (2,
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
                                  })],
               },
               wl3);
    // Focus master
    wm.focus_window(Some(1)).unwrap();
    // Four windows
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();
    let wl4 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(4),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (2,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 200,
                                  }),
                                 (3,
                                  Geometry {
                                      x: 400,
                                      y: 200,
                                      width: 400,
                                      height: 200,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 400,
                                      width: 400,
                                      height: 200,
                                  })],
               },
               wl4);
}
#[test]
/// Get_windows() must not contain duplicates.
fn test_get_windows() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert_eq!(vec![1], wm.get_windows());
    let wl = wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM));
    assert_eq!(wl.is_err(), true);
    assert_eq!(vec![1], wm.get_windows());
}
#[test]
/// is_managed must return true for the given window after add_window was
/// called with the given window.
/// after adding a window using add_window, it must be focused according to get_focused_window.
fn test_add_window() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.is_managed(1));
    assert_eq!(Some(1), wm.get_focused_window());
    let wl = wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM));
    assert_eq!(wl.is_err(), true);
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert_eq!(Some(2), wm.get_focused_window());
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
    wm.focus_window(Some(1)).unwrap();
    // Now let's remove window 2
    wm.remove_window(2).unwrap();
    // It should no longer be managed by the WM.
    assert!(!wm.is_managed(2));
    // The `Vec` of windows should now just contain window 1.
    assert_eq!(vec![1, 3], wm.get_windows());
    assert_eq!(Some(1), wm.get_focused_window());

    wm.remove_window(1).unwrap();
    assert_eq!(None, wm.get_focused_window());
}


#[test]
/// When focus_window succeeds, get_focused_window must return the same argument.
fn test_focus_window() {
    // Let's make a new `FullscreenWM` with `SCREEN` as screen.
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();

    wm.focus_window(Some(1)).unwrap();
    assert_eq!(Some(1), wm.get_focused_window());

    wm.focus_window(None).unwrap();
    assert_eq!(None, wm.get_focused_window());
}

#[test]
/// Do nothing when there are no windows. When there is only one window,
/// focus it if currently no window is focused, otherwise do nothing.
/// When no window is focused, any window may become focused.
/// Cycling the focus back and forth shouldn't change the focused window.
fn test_cycle_focus() {
    let mut wm = WMName::new(SCREEN);
    // No windows
    wm.cycle_focus(PrevOrNext::Next);
    assert_eq!(WindowLayout::new(), wm.get_window_layout());
    wm.cycle_focus(PrevOrNext::Prev);
    assert_eq!(WindowLayout::new(), wm.get_window_layout());

    // One window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.cycle_focus(PrevOrNext::Next);
    assert_eq!(Some(1), wm.get_focused_window());

    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.cycle_focus(PrevOrNext::Prev);
    assert_eq!(Some(1), wm.get_focused_window());

    // Multiple windows
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    //
    let wl1 = wm.get_window_layout();
    assert_eq!(Some(3), wl1.focused_window);
    wm.cycle_focus(PrevOrNext::Next);
    let wl2 = wm.get_window_layout();
    assert_eq!(Some(1), wl2.focused_window);

    // Don't change the windows order
    assert_eq!(wl1.windows, wl2.windows);

    wm.cycle_focus(PrevOrNext::Prev);
    let wl3 = wm.get_window_layout();
    assert_eq!(Some(3), wl3.focused_window);

    wm.cycle_focus(PrevOrNext::Prev);
    let wl4 = wm.get_window_layout();
    assert_eq!(Some(2), wl4.focused_window);

    wm.cycle_focus(PrevOrNext::Next);
    let wl5 = wm.get_window_layout();
    assert_eq!(Some(3), wl5.focused_window);
}
#[test]
/// This function should return an appropriate error when the window
/// is not managed by the window manager.
fn test_get_window_info() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    let wl1 = wm.get_window_info(2);
    assert_eq!(Some(WindowWithInfo {
                   window: 2,
                   geometry: Geometry {
                       x: 400,
                       y: 0,
                       width: 400,
                       height: 300,
                   },
                   float_or_tile: FloatOrTile::Tile,
                   fullscreen: false,
               }),
               wl1.ok());
    let x = wm.get_window_info(4);
    assert_eq!(x.is_err(), true);
}
/// Get screen info
#[test]
fn test_get_screen() {
    let wm = WMName::new(SCREEN);
    assert_eq!(wm.get_screen(), SCREEN);
}

#[test]
/// After resize_screen is called with a screen, get_screen() must return the same screen.
fn test_resize_screen() {
    let screen_resized = Screen {
        width: 1024,
        height: 1024,
    };
    // After resize_screen is called with a screen, get_screen() must return the same screen.
    let mut wm = WMName::new(SCREEN);
    wm.resize_screen(screen_resized);
    assert_eq!(wm.get_screen(), screen_resized);
}

#[test]
/// is_managed(w) == true for some window w iff the vector returned by
/// the get_windows method contains w
fn test_is_managed() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert!(wm.is_managed(1));
    assert!(!wm.is_managed(2));
}
#[test]
/// get_focused_window() == get_window_layout().focused_window.
/// get_focused_window() == Some(w) then is_managed(w) == true.
fn test_get_focused_window() {
    // Let's make a new `FullscreenWM` with `SCREEN` as screen.
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();

    wm.focus_window(Some(1)).unwrap();
    assert_eq!(wm.get_window_layout().focused_window,
               wm.get_focused_window());

    assert_eq!(wm.get_focused_window(), Some(1));
    assert!(wm.is_managed(1));
}
#[test]
/// get_master_window() == Some(w), then w must occur in the vector returned by get_windows().
/// if the vector returned by get_windows() is empty => get_master_window() == None.
/// The other direction of the arrow must not hold, e.g., there could floating windows
/// see FloatSupport), but no tiled windows.
fn test_get_master_window() {
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    assert!(wm.get_windows().is_empty());
    assert_eq!(wm.get_master_window(), None);

    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    assert_eq!(wm.get_master_window(), Some(1));
    assert!(wm.get_windows().contains(&1));
}
#[test]
/// After the function has succeeded, the master window should be focused.
/// If the given window is already in the master tile, no windows have to be swapped,
/// but the master window should be focused.
/// if swap_with_master(w) succeeds, get_master_window() == Some(w).
fn test_swap_with_master() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_float(2, SOME_GEOM)).unwrap();
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
    // No windows
    wm.swap_windows(PrevOrNext::Prev);
    wm.swap_windows(PrevOrNext::Next);
    let wl1 = wm.get_window_layout();
    assert_eq!(WindowLayout::new(), wl1);

    // One window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    wm.swap_windows(PrevOrNext::Next);
    let wl1 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(1, SCREEN_GEOM)],
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
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (2,
                                  Geometry {
                                      x: 400,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  })],
               },
               wl2);
    // Test swap
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    wm.swap_windows(PrevOrNext::Prev);
    let wl3 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(3),
                   windows: vec![(1,
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
                                 (2,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 400,
                                      height: 300,
                                  })],
               },
               wl3);
    wm.swap_windows(PrevOrNext::Next);
    let wl4 = wm.get_window_layout();
    assert_eq!(WindowLayout {
                   focused_window: Some(3),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 400,
                                      height: 600,
                                  }),
                                 (2,
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
                                  })],
               },
               wl4);
    assert_eq!(wm.get_focused_window(), Some(3));

    let dir = PrevOrNext::Next;
    wm.swap_windows(dir);
    wm.swap_windows(dir.opposite());
    assert_eq!(wl4, wm.get_window_layout());
}
