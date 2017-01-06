MACHINE Jugs
SETS
 jugs = {jug_3, jug_5}
CONSTANTS max_contents
PROPERTIES
 max_contents = {jug_3 |-> 3, jug_5 |-> 5}
DEFINITIONS
 GOAL == contents(jug_5) = 4
VARIABLES contents 
INVARIANT
 contents : jugs --> NAT
 & ! x . (x : jugs => contents(x) <= max_contents(x))
INITIALISATION 
 contents := {jug_3 |-> 0, jug_5 |-> 0}
OPERATIONS

  Empty(jug) = 
   PRE 
	  jug : jugs
	& contents(jug) > 0
   THEN 
	contents := contents <+ {jug |-> 0}
   END;

  Fill(jug) = 
   PRE
	  jug : jugs
	& contents(jug) < max_contents(jug)
   THEN 
	contents := contents <+ {jug |-> max_contents(jug)}
   END;

  Transfer(jug1, jug2) = 
   PRE
	  jug1 : jugs
	& jug2 : jugs
	& jug1 /= jug2
 	& contents(jug1) > 0
        & contents(jug2) < max_contents(jug2)
   THEN
	contents := contents <+ {jug1 |-> max({0, 
			contents(jug1) - (max_contents(jug2) - contents(jug2))}),
		     jug2 |-> min({max_contents(jug2), 
			contents(jug2) + contents(jug1)})} 
   END
END