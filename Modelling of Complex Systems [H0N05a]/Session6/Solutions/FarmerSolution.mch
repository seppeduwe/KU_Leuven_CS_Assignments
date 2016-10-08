MACHINE Farmer
SETS
 Obj={farmer,fox, chicken, grain}
DEFINITIONS
  safe(s) == (!(x,y).(x:s & y:s => x|->y /: eats)); /* Can be used to check if a shore is safe, eg. safe(far)*/
  GOAL == (far=Obj)
CONSTANTS eats
PROPERTIES
 eats: Obj +-> Obj &
 eats = {fox |-> chicken, chicken |-> grain}
VARIABLES 
  near, /*Set of objects on the starting shore*/
  far /*Set of objects on the far shore*/
INVARIANT
  near<:Obj 
& far<:Obj 
& near \/ far = Obj 
& near /\ far = {}
INITIALISATION near,far := Obj,{}
OPERATIONS
  Move_far(x) = 
  PRE 
	  farmer:near 
	& x <: Obj-{farmer} 
	& x <: near
	& card(x)<2 
	& safe(far) 
  THEN
      	near := (near - {farmer}) - x;
	far :=  far \/ {farmer} \/ x
  END;

  Move_near(x) = 
  PRE 
	  farmer:far 
	& x <: Obj-{farmer} 
	& x <: far
	& card(x)<2 & safe(near) 
  THEN
      	far := (far - {farmer}) - x;
	near := near \/ {farmer} \/ x
  END;

  YouLose = 
  PRE 
	  (farmer:near 
	& not(safe(far))) or
            (farmer:far  & not(safe(near))) 
  THEN 
	skip 
  END
END

