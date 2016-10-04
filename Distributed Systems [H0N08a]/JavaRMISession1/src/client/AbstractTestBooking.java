package client;

import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import rental.Quote;
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
 * c 
 * Modifier to indicate that the according command will fail
 */
public abstract class AbstractTestBooking extends AbstractTesting {
	//open sessions
	private Map<String, Quote> singleQuoteSessions = new HashMap<String, Quote>();

	
	/**
	 * Print available car types for a given period.
	 *
	 * @param 	start
	 * 			start time of the period
	 * @param 	end
	 * 			end time of the period
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	protected abstract void checkForAvailableCarTypes(Date start, Date end) throws Exception;
	
	/**
	 * Retrieve a quote for a given car type (tentative reservation).
	 * 
	 * @param	clientName 
	 * 			name of the client 
	 * @param 	start 
	 * 			start time for the quote
	 * @param 	end 
	 * 			end time for the quote
	 * @param 	carType 
	 * 			type of car to be reserved
	 * @return	the newly created quote
	 *  
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	protected abstract Quote createQuote(String clientName, Date start, Date end, String carType) throws Exception;
	
	/**
	 * Confirm the given quote to receive a final reservation of a car.
	 * 
	 * @param 	quote 
	 * 			the quote to be confirmed
	 * @return	the final reservation of a car
	 * 
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	protected abstract Reservation confirmQuote(Quote quote) throws Exception;
	
	/**
	 * Get all reservations made by the given client.
	 *
	 * @param 	renter
	 * 			name of the client
	 * @return	the list of reservations of the given client
	 * 
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	protected abstract List<Reservation> getReservationsByRenter(String renter) throws Exception;

	/**
	 * Get the number of reservations for a particular car type.
	 * 
	 * @param 	carType 
	 * 			name of the car type
	 * @return 	number of reservations for the given car type
	 * 
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	protected abstract int getNumberOfReservationsForCarType(String carType) throws Exception;
	
	
	public AbstractTestBooking(String scriptFile) {		
		super(scriptFile);
	}
	

	final protected void processLine(String name, String cmd, List<Character> flags, StringTokenizer scriptLineTokens) throws ApplicationException {
		Date startDate = null, endDate = null;		
		if (cmd.equals("BA") || cmd.equals("BB")) {
			try {
				startDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
				endDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
			} catch (ParseException e) {
				throw new IllegalArgumentException(e);
			}
		}
		
		if (cmd.equals("BA")) {
			try {
				checkForAvailableCarTypes(startDate, endDate);
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BB")){
			String type = scriptLineTokens.nextToken();
			try { 
				singleQuoteSessions.put(name, createQuote(name, startDate, endDate, type));
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BF")){
			Quote reservation = singleQuoteSessions.get(name);
			if(reservation==null) 
				throw new IllegalArgumentException("No quote");	
			try {
				Reservation r = confirmQuote(reservation);
				System.out.println("Reservation succeeded: "+r.toString());
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BM")){
			try {
				assessTotalReservations(name,scriptLineTokens);
			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("BMR")){
			System.out.println("Reservations by "+name+":\n");
			int i = 1;
			try {
				for(Reservation r : getReservationsByRenter(name)) {
					System.out.println(i++ +") "+r.toString()+"\n");
				}
			} catch (Exception e) { throw new ApplicationException(e); }
		} else {
			throw new IllegalArgumentException("Unknown command");
		}
	}
		
	private void assessTotalReservations(String name, StringTokenizer scriptReader) throws Exception {
		while(scriptReader.hasMoreTokens()){
			String pars = scriptReader.nextToken();
			String[] pair = pars.split(":");
			int nr = getNumberOfReservationsForCarType(pair[0]);
			if (Integer.parseInt(pair[1]) == nr) {
				System.out.println(name + " has correct totals " + pars + " " + nr);
			} else {
				System.err.println(name + " has wrong totals " + pars +" " + nr );
			}
		}
	}
	
}