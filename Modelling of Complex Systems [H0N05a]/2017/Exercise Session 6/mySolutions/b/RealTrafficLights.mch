MACHINE RealLights1
SETS 
  colours={green,orange,red}
ABSTRACT_VARIABLES
  on_crossing_ns,
  on_crossing_we,
  ns_light,
  ns_turn,
  we_light
INVARIANT
    ns_turn : BOOL
  & on_crossing_we : NAT
  & on_crossing_ns : NAT
  & (on_crossing_ns = 0 or on_crossing_we = 0)
  & not(ns_light = green & we_light = green)
  & (ns_light /= red => we_light = red)
INITIALISATION
         ns_light := red;
         we_light := red;
         ns_turn := TRUE;
         on_crossing_ns := 0;
         on_crossing_we := 0

OPERATIONS
  ns_green = 
    PRE 
        ns_light = red
      & we_light = red
      & ns_turn = TRUE
& on_crossing_we = 0
    THEN 
        ns_light := green;
        ns_turn := FALSE
    END;
  
  ns_orange = 
    PRE 
         ns_light = green
    THEN 
         ns_light := orange
    END;
  
  ns_red = 
    PRE
         ns_light = orange
    THEN 
         ns_light := red
    END;
  
  we_green = 
    PRE
        ns_light = red
      & we_light = red
      & ns_turn = FALSE
      & on_crossing_ns = 0
    THEN 
        we_light := green;
	ns_turn := TRUE
    END;
  
  we_orange = 
    PRE
         we_light = green
    THEN 
         we_light := orange
    END;
  
  we_red = 
    PRE 
         we_light = orange
    THEN 
         we_light := red
    END;
  
  enter_crossing_ns = 
    PRE
         ns_light = green
       & on_crossing_ns < 5
    THEN 
         on_crossing_ns := on_crossing_ns + 1
    END;
  
  enter_crossing_we = 
    PRE 
         we_light = green
       & on_crossing_we < 5
    THEN 
         on_crossing_we := on_crossing_we + 1
    END;
  
  leave_crossing_ns = 
    PRE
         on_crossing_ns > 0
    THEN 
         on_crossing_ns := on_crossing_ns - 1
    END;
  
  leave_crossing_we = 
    PRE
         on_crossing_we > 0
    THEN 
         on_crossing_we := on_crossing_we - 1
    END
END


