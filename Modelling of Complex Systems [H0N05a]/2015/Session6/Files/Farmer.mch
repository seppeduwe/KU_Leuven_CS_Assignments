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
  	x : {0} /* -> Remove */
	/*TODO*/
  THEN
      	skip /* -> Remove */
	/*TODO*/
  END;

  Move_near(x) = 
  PRE 
	x : {0} /* -> Remove */
	/*TODO*/
  THEN
      	skip /* -> Remove */
	/*TODO*/
  END;


/*Do not change this operation!*/
  YouLoose = 
  PRE 
	  (farmer:near 
	& not(safe(far))) or
            (farmer:far  & not(safe(near))) 
  THEN 
	skip 
  END
END

