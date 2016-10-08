package client;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import client.AbstractTesting.ApplicationException;
import rental.CarType;
import rental.Reservation;

/**
 * This class forwards all commands starting with B to AbstractTestAgency and 
 * supports the following script commands
 * (note that additional commands can be supported by extending this class) :  
 * 
 * MB  
 * <client> is the best client (has highest absolute number of finalized reservations)
 * 
 * MF <carType>(/<carType>)* 
 * One or most most popular <carType>s with the CRC <client> (have the most finalized reservations over both)
 * 
 * MA <from> <until> <carType>(/<carType>)* 
 * One or more cheapest <carTypes> in the given period (over all CRCs)
 *
 * MC
 * <client> is the most popular CRC
 *
 */
public abstract class AbstractTestManagement<ReservationSession, ManagerSession> extends AbstractTestAgency<ReservationSession, ManagerSession> {

    /**
     * Get the (list of) best clients, i.e. clients that have highest number of
     * reservations (across all rental agencies).
     *
     * @param ms manager session
     * @return set of best clients
     * @throws Exception if things go wrong, throw exception
     */
	//protected abstract Set<String> getBestClients(ManagerSession ms) throws Exception;

    /**
     * Find a cheapest car type that is available in the given period.
     *
     * @param session the session to do the request from
     * @param start start time of the period
     * @param end end time of the period
     *
     * @return name of a cheapest car type for the given period
     *
     * @throws Exception if things go wrong, throw exception
     */
    protected abstract String getCheapestCarType(ReservationSession session, Date start,
            Date end) throws Exception;

    /**
     * Get the most popular car type in the given car rental company.
     *
     * @param ms manager session
     * @param	carRentalCompanyName The name of the car rental company.
     * @return the most popular car type in the given car rental company
     *
     * @throws Exception if things go wrong, throw exception
     */
    //protected abstract CarType getMostPopularCarTypeIn(ManagerSession ms, String carRentalCompanyName) throws Exception;

    protected abstract String getMostPopularCarRentalCompany(ManagerSession ms) throws Exception;

    
    public AbstractTestManagement(String scriptFile) {
        super(scriptFile);
    }

    ReservationSession managerResSession = null;
	protected void processLine(String name, String cmd, List<Character> flags, StringTokenizer scriptLineTokens) throws ApplicationException {
		if (cmd.startsWith("B")) {
			super.processLine(name, cmd, flags, scriptLineTokens);
			return;
		}
		
		if (cmd.equals("MB")) {
//            Set<String> bestClientsShouldBe = new HashSet<String>(Arrays.asList(name.split("/")));
//            Set<String> bestClientsAre = null;
//			try {
//				ManagerSession aMgmtSession = getNewManagerSession("CarRent", name);	            
//	            bestClientsAre = new HashSet<>(getBestClients(aMgmtSession));
//			} catch (Exception e) { throw new ApplicationException(e); }
//
//            if (bestClientsShouldBe.equals(bestClientsAre)) {
//                System.out.println("Correct list of best clients: "
//                        + joinToString(bestClientsAre, ','));
//            } else {
//                System.err.println("Incorrect list of best clients: "
//                        + joinToString(bestClientsAre, ','));
//            }
		} else if (cmd.equals("MF")) {
//            try {
//				checkPopularCarType(name, scriptLineTokens);
//			} catch (Exception e) { throw new ApplicationException(e); }
		} else if (cmd.equals("MA")) {
            Date startDate = null, endDate = null;
			try {
	            startDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
	            endDate = DATE_FORMAT.parse(scriptLineTokens.nextToken());
			} catch(ParseException pe) {
				throw new IllegalArgumentException(pe);
			}
			
			if (managerResSession == null) {
				try {
					managerResSession = getNewReservationSession("Manager");
				} catch (Exception e1) { throw new ApplicationException(e1); }
			}
            
            List<String> typeNameShoudBe = Arrays.asList(scriptLineTokens
                    .nextToken().toLowerCase().split("/"));
            
            String typeNameIs = null;
			try {
				typeNameIs = getCheapestCarType(managerResSession, startDate, endDate);
			} catch (Exception e) { throw new ApplicationException(e); }
			
            if (typeNameIs != null && typeNameShoudBe.contains(typeNameIs.toLowerCase())) {
                System.out.println("A cheapest car type is: " + typeNameIs);
            } else {
                System.err.println("Wrong cheapest car type: " + typeNameIs);
            }
		} else if (cmd.equals("MC")) {
			try {
				this.checkPopularCrc(name);
			} catch (Exception e) { throw new ApplicationException(e); }
		} else {
			throw new IllegalArgumentException("Unknown command");
		}
	}

//	private void checkPopularCarType(String name, StringTokenizer scriptReader) throws Exception {
//        ManagerSession rental = getNewManagerSession(name, name);
//        while (scriptReader.hasMoreTokens()) {
//            String pars = scriptReader.nextToken();
//            String favorite = null;
//            List<String> favorites = Arrays.asList(pars.split("/"));
//            CarType ct = getMostPopularCarTypeIn(rental, name);
//            if (ct != null) {
//                favorite = ct.getName();
//                if (favorites.contains(favorite)) {
//                    System.out.println(name + " has correct favorite car type: " + pars + " " + favorite);
//                } else {
//                    System.err.println(name + " has wrong favorite car type: " + pars + " " + favorite);
//                }
//            } else {
//                System.err.println(name + " has wrong favorite car type: " + pars + " [CarType is NULL]" );
//            }
//        }
//    }
	

    private void checkPopularCrc(String mostPopularCrcName) throws Exception {
        ManagerSession rental = getNewManagerSession("someName", "someCrc");
        String favorite = getMostPopularCarRentalCompany(rental);
        if(mostPopularCrcName.equals(favorite)) {
            System.out.println("Correct most popular car rental company: "+ favorite);
        } else {
            System.err.println("Incorrect most popular car rental company: "+ favorite);
        } 
    }

}