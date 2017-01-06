MACHINE SingleProcess
SETS 
  states={busy,ready}
ABSTRACT_VARIABLES
  request,
  state
INVARIANT
	  state : states
	& request : BOOL
INITIALISATION
    state := ready;
    request :: {FALSE,TRUE}
OPERATIONS
  accept_request(next_request) = 
    PRE
	state = ready
      & request = TRUE
    THEN 
         state := busy;
         request := next_request
    END;
  
  do_not_accept_request(next_request, next_state) = 
    PRE
	 state = busy or request = FALSE
    THEN 
         state := next_state;
         request := next_request
    END
END

