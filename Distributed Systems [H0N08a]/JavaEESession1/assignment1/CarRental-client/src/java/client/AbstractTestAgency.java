package client;

import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import rental.Reservation;

/**
 * 
 * This class supports the following script commands
 * (note that additional commands can be supported by extending this class) :  
 * 
 * BA <from> <until> 
 * Check availability of car types from <from> until <until>
 * 
 * BB <from> <until> <carType>    (command)
 * Create a quote for <client> for a car of type <carType> from <from> until <until>
 * 
 * BF
 * Finalize the quote made for client <client>
 * 
 * BM <type:nr>*
 * Assess total number of reservations comparing it to <type:nr>*
 * 
 * BMR
 * Print all reservations of a client
 * 
 * BS 
 * Create a reservation session  
 *
 * c 
 * Modifier to indicate that the according command will fail
 */
public abstract class AbstractTestAgency<ReservationSession, ManagerSession> extends AbstractTesting {
    protected Map<String, ReservationSession> sessions = new HashMap<String, ReservationSession>();

    /**
     * Create a new reservation session for the user with the given name.
     *
     * @param name name of the client (renter) owning this session
     * @return the new reservation session
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract ReservationSession getNewReservationSession(String name) throws Exception;

    /**
     * Create a new manager session for the user with the given name.
     *
     * @param name name of the user (i.e. manager) using this session
     * @param carRentalName name of the rental company managed by this session
     * @return the new manager session
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract ManagerSession getNewManagerSession(String name, String carRentalName) throws Exception;

    /**
     * Check which car types are available in the given period and print them.
     *
     * @param session the session to do the request from
     * @param start start time of the period
     * @param end end time of the period
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract void checkForAvailableCarTypes(ReservationSession session, Date start, Date end) throws Exception;

    /**
     * Add a quote for a given car type to the session.
     *
     * @param session the session to add the reservation to
     * @param name the name of the client owning the session
     * @param start start time of the reservation
     * @param end end time of the reservation
     * @param carType type of car to be reserved
     * @param carRentalName name of the rental company by which the reservation
     * should be done
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract void addQuoteToSession(ReservationSession session, String name,
            Date start, Date end, String carType, String carRentalName) throws Exception;

    /**
     * Confirm the quotes in the given session.
     *
     * @param session the session to finalize
     * @param name the name of the client owning the session
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract List<Reservation> confirmQuotes(ReservationSession session, String name) throws Exception;

    /**
     * Get the number of reservations made by the given renter (across whole
     * rental agency).
     *
     * @param	ms manager session
     * @param clientName name of the renter
     * @return	the number of reservations of the given client (across whole
     * rental agency)
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract int getNumberOfReservationsBy(ManagerSession ms, String clientName) throws Exception;

    /**
     * Get the number of reservations for a particular car type.
     *
     * @param ms manager session
     * @param carRentalName name of the rental company managed by this session
     * @param carType name of the car type
     * @return number of reservations for this car type
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract int getNumberOfReservationsForCarType(ManagerSession ms, String carRentalName, String carType) throws Exception;    

    public AbstractTestAgency(String scriptFile) {
        super(scriptFile);
    }

	protected void processLine(String name, String cmd, List<Character> flags, StringTokenizer scriptLineTokens) throws ApplicationException {
		//
		// Pre processing command 
		//
		Date startDate = null, endDate = null;		
		if (cmd.equals("BA") || cmd.equals("BB")) {
			try {
				startDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
				endDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
			} catch (ParseException e) {
				throw new IllegalArgumentException(e);
			}
		}
		
        ReservationSession session = null;
        if (cmd.equals("BA") || cmd.equals("BB") || cmd.equals("BF")) {
        	session = sessions.get(name);
	        if (session == null) {
	            throw new IllegalArgumentException("No session");
	        }
        }
		
		//
		// Processing command
		//
		if (cmd.equals("BA")) {
			try {
				checkForAvailableCarTypes(session, startDate, endDate);
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BB")){
            String type = scriptLineTokens.nextToken();
            String agent = scriptLineTokens.nextToken();
            try {
                addQuoteToSession(session, name, startDate, endDate, type, agent);
   			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BF")){
            try {
                confirmQuotes(session, name);
   			} catch (Exception e) { throw new ApplicationException(e); }          
		} else if (cmd.equals("BM")){
			try {
                assessTotalReservations(name, scriptLineTokens);
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BMR")){
			try{
				ManagerSession aMgmtSession = getNewManagerSession("CarRent", name);
	            System.out.println("Number of reservations by " + name + ":\t" + getNumberOfReservationsBy(aMgmtSession, name));
	            System.out.println();
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BS")) {
            try {
            	sessions.put(name, getNewReservationSession(name));
			} catch (Exception e) { throw new ApplicationException(e); }
		} else {
			throw new IllegalArgumentException("Unknown command");
		}
	}

    

    private void assessTotalReservations(String name, StringTokenizer scriptReader) throws Exception {
        ManagerSession rental = getNewManagerSession(name, name);
        while (scriptReader.hasMoreTokens()) {
            String pars = scriptReader.nextToken();
            String[] pair = pars.split(":");
            int nr = getNumberOfReservationsForCarType(rental, name, pair[0]);
            if (Integer.parseInt(pair[1]) == nr) {
                System.out.println(name + " has correct totals " + pars + " " + nr);
            } else {
                System.err.println(name + " has wrong totals " + pars + " " + nr);
            }
        }
    }
}