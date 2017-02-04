// We have to import `FullscreenWM` from the super module.
use super::WMName;
// We have to repeat the imports we did in the super module.
use cplwm_api::wm::WindowManager;
use cplwm_api::types::*;

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


// Now let's write our test.
//
// Note that tests are annotated with `#[test]`, and cannot take arguments
// nor return anything.
#[test]
/// Adding and removing windows
fn test_adding_and_removing_some_windows() {
    // Let's make a new `FullscreenWM` with `SCREEN` as screen.
    let mut wm = WMName::new(SCREEN);

    // Initially the window layout should be empty.
    assert_eq!(WindowLayout::new(), wm.get_window_layout());
    // `assert_eq!` is a macro that will check that the second argument,
    // the actual value, matches first value, the expected value.

    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    // Because `add_window` returns a `Result`, we use `unwrap`, which
    // tries to extract the `Ok` value from the result, but will panic
    // (crash) when it is an `Err`. You must be very careful when using
    // `unwrap` in your code. Here we can use it because we know for sure
    // that an `Err` won't be returned, and even if that were the case,
    // the panic will simply cause the test to fail.

    // The window should now be managed by the WM
    assert!(wm.is_managed(1));
    // and be present in the `Vec` of windows.
    assert_eq!(vec![1], wm.get_windows());
    // According to the window layout
    let wl1 = wm.get_window_layout();
    // it should be focused
    assert_eq!(Some(1), wl1.focused_window);
    // and fullscreen.
    assert_eq!(vec![(1, SCREEN_GEOM)], wl1.windows);

    // get_focused_window() == get_window_layout().focused_window.
    // get_focused_window() == Some(w) then is_managed(w) == true

    // Let's add another window.
    wm.add_window(WindowWithInfo::new_fullscreen(2, SOME_GEOM)).unwrap();
    // It should now be managed by the WM.
    assert!(wm.is_managed(2));
    // The `Vec` of windows should now contain both windows 1 and 2.
    assert_eq!(vec![1, 2], wm.get_windows());
    // According to the window layout
    let wl2 = wm.get_window_layout();
    // window 2 should be focused
    assert_eq!(Some(2), wl2.focused_window);
    // and fullscreen.
    assert_eq!(vec![(2, SCREEN_GEOM)], wl2.windows);

    // Now let's remove window 2
    wm.remove_window(2).unwrap();
    // It should no longer be managed by the WM.
    assert!(!wm.is_managed(2));
    // The `Vec` of windows should now just contain window 1.
    assert_eq!(vec![1], wm.get_windows());
    // According to the window layout
    let wl3 = wm.get_window_layout();
    // window 1 should be focused again
    assert_eq!(Some(1), wl3.focused_window);
    // and fullscreen.
    assert_eq!(vec![(1, SCREEN_GEOM)], wl3.windows);
}

#[test]
/// Test set and get the focus window
fn test_focus_window() {
    // Let's make a new `FullscreenWM` with `SCREEN` as screen.
    let mut wm = WMName::new(SCREEN);

    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    let wl1 = wm.get_window_layout();
    assert_eq!(Some(2), wl1.focused_window);

    wm.focus_window(Some(1)).unwrap();
    let wl2 = wm.get_window_layout();
    assert_eq!(Some(1), wl2.focused_window);

    wm.focus_window(None).unwrap();
    let wl3 = wm.get_window_layout();
    assert_eq!(None, wl3.focused_window);
    // When focus_window succeeds, get_focused_window must return the same argument.
}

#[test]
/// Test cycle focus method
fn test_cycle_focus() {
    let mut wm = WMName::new(SCREEN);
    // Let's add a window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
    let wl1 = wm.get_window_layout();
    assert_eq!(Some(3), wl1.focused_window);
    // When no window is focused, any window may become focused.

    // Cycling the focus back and forth shouldn't change the focused window.
    wm.cycle_focus(PrevOrNext::Next);
    let wl2 = wm.get_window_layout();
    assert_eq!(Some(1), wl2.focused_window);

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
/// Get window info
fn test_get_window_info() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_fullscreen(1, SOME_GEOM)).unwrap();
    let x = wm.get_window_info(1);
    assert_eq!(x.ok(),
               Some(WindowWithInfo::new(1, SCREEN_GEOM, FloatOrTile::Tile, true)));
    let x = wm.get_window_info(2);
    assert_eq!(x.is_err(), true);

}
#[test]
/// Get screen info
fn test_get_screen() {
    let wm = WMName::new(SCREEN);
    assert_eq!(wm.get_screen(), SCREEN);
}

#[test]
/// Resize the screen
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
