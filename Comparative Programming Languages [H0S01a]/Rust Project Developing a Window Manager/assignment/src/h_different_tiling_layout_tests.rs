use cplwm_api::wm::WindowManager;
use cplwm_api::types::*;
use h_different_tiling_layout::*;

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
/// Get the window layout of the spiral implementation.
fn test_get_window_layout() {
    let mut wm = WMName::new(SCREEN);
    // Zero windows
    assert_eq!(WindowLayout::new(), wm.get_window_layout());
    // One window
    wm.add_window(WindowWithInfo::new_tiled(1, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(1),
                   windows: vec![(1,
                                  Geometry {
                                      x: 0,
                                      y: 0,
                                      width: 800,
                                      height: 600,
                                  })],
               },
               wm.get_window_layout());

    wm.add_window(WindowWithInfo::new_tiled(2, SOME_GEOM)).unwrap();
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
               wm.get_window_layout());
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
    wm.add_window(WindowWithInfo::new_tiled(4, SOME_GEOM)).unwrap();
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
                                      height: 300,
                                  }),
                                 (3,
                                  Geometry {
                                      x: 600,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  })],
               },
               wm.get_window_layout());
    wm.add_window(WindowWithInfo::new_tiled(5, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(5),
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
                                      x: 600,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 450,
                                      width: 200,
                                      height: 150,
                                  }),
                                 (5,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 200,
                                      height: 150,
                                  })],
               },

               wm.get_window_layout());
    wm.add_window(WindowWithInfo::new_tiled(6, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(6),
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
                                      x: 600,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 450,
                                      width: 200,
                                      height: 150,
                                  }),
                                 (5,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 100,
                                      height: 150,
                                  }),
                                 (6,
                                  Geometry {
                                      x: 500,
                                      y: 300,
                                      width: 100,
                                      height: 150,
                                  })],
               },
               wm.get_window_layout());
    wm.add_window(WindowWithInfo::new_tiled(7, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(7),
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
                                      x: 600,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 450,
                                      width: 200,
                                      height: 150,
                                  }),
                                 (5,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 100,
                                      height: 150,
                                  }),
                                 (6,
                                  Geometry {
                                      x: 500,
                                      y: 300,
                                      width: 100,
                                      height: 75,
                                  }),
                                 (7,
                                  Geometry {
                                      x: 500,
                                      y: 375,
                                      width: 100,
                                      height: 75,
                                  })],
               },
               wm.get_window_layout());
    wm.add_window(WindowWithInfo::new_tiled(8, SOME_GEOM)).unwrap();
    assert_eq!(WindowLayout {
                   focused_window: Some(8),
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
                                      x: 600,
                                      y: 300,
                                      width: 200,
                                      height: 300,
                                  }),
                                 (4,
                                  Geometry {
                                      x: 400,
                                      y: 450,
                                      width: 200,
                                      height: 150,
                                  }),
                                 (5,
                                  Geometry {
                                      x: 400,
                                      y: 300,
                                      width: 100,
                                      height: 150,
                                  }),
                                 (6,
                                  Geometry {
                                      x: 500,
                                      y: 300,
                                      width: 100,
                                      height: 75,
                                  }),
                                 (7,
                                  Geometry {
                                      x: 550,
                                      y: 375,
                                      width: 50,
                                      height: 75,
                                  }),
                                 (8,
                                  Geometry {
                                      x: 500,
                                      y: 375,
                                      width: 50,
                                      height: 75,
                                  })],
               },
               wm.get_window_layout());
}
