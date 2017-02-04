use cplwm_api::wm::WindowManager;
use cplwm_api::wm::GapSupport;
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
/// Return the current gap size.
/// Initially 0.
fn test_get_gap() {
    let wm = WMName::new(SCREEN);
    assert_eq!(wm.get_gap(), 0);
}

#[test]
/// Set the gap size.
///
/// **Invariant**: after setting `set_gap(g)` with some gap size `g`,
/// `get_gap() == g`.
fn test_set_gap() {
    let mut wm = WMName::new(SCREEN);
    wm.set_gap(5);
    assert_eq!(wm.get_gap(), 5);
}

#[test]
/// When the gap is set to 5, it is as if every window gets an invisible 5
/// pixel border of empty space. Windows don't share the gap in between them.
/// Every tiled window is moved 5 pixels down and to 5 pixels to the right.
/// The width and height of every tiled window is also shrunk by 10 pixels.
fn test_get_window_info() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
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
               wm.get_window_info(2).ok());
    wm.set_gap(5);
    assert_eq!(Some(WindowWithInfo {
                   window: 2,
                   geometry: Geometry {
                       x: 405,
                       y: 5,
                       width: 390,
                       height: 290,
                   },
                   float_or_tile: FloatOrTile::Tile,
                   fullscreen: false,
               }),
               wm.get_window_info(2).ok());
}
#[test]
/// When the gap is set to 5, it is as if every window gets an invisible 5
/// pixel border of empty space. Windows don't share the gap in between them.
/// Every tiled window is moved 5 pixels down and to 5 pixels to the right.
/// The width and height of every tiled window is also shrunk by 10 pixels.
fn test_get_window_layout() {
    let mut wm = WMName::new(SCREEN);
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
    wm.add_window(WindowWithInfo::new_tiled(3, SOME_GEOM)).unwrap();
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
               wm.get_window_layout());
    wm.set_gap(5);
    assert_eq!(WindowLayout {
                   focused_window: Some(3),
                   windows: vec![(1,
                                  Geometry {
                                      x: 5,
                                      y: 5,
                                      width: 390,
                                      height: 590,
                                  }),
                                 (2,
                                  Geometry {
                                      x: 405,
                                      y: 5,
                                      width: 390,
                                      height: 290,
                                  }),
                                 (3,
                                  Geometry {
                                      x: 405,
                                      y: 305,
                                      width: 390,
                                      height: 290,
                                  })],
               },
               wm.get_window_layout());
}
