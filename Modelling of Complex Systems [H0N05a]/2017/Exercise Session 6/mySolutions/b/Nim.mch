MACHINE Nim
SETS 
  winners={player1,player2, no}
CONSTANTS
  players,
  next_player,
  heaps
VARIABLES
  turn_counter,
  current_player,
  heap_count,
  winner
PROPERTIES
    next_player = {player1 |-> player2,player2 |-> player1}
  & players = winners - {no}
  & heaps = {1,2,3,4}
INVARIANT
    winner : winners 
  & turn_counter : NAT
  & current_player : players
  & !(x).(x : heaps => heap_count(x) >= 0)
  & (heap_count(4) > 0 => winner = no)
  & ( winner /= no => heap_count(4) = 0)
INITIALISATION
         heap_count := {(1|->1),(2|->3),(3|->5),(4|->7)};
         winner := no;
         current_player := player1;
	 turn_counter := 0
OPERATIONS
  turn(nb, heap) = 
    PRE 
	winner = no
	& 0 < nb
	& heap : heaps
   & !x . (x : heaps & x < heap => heap_count(x) = 0)
      & !x . (x : heaps & x > heap => heap_count(x) > 0)
      & nb <= heap_count(heap)
    THEN 
	heap_count := heap_count <+ {heap |->  heap_count(heap) - nb};
	current_player := next_player(current_player);
	 turn_counter := turn_counter + 1
    END;
  
  game_over = 
    PRE 
	heap_count(4) = 0
    THEN 
	winner := current_player
    END
END

