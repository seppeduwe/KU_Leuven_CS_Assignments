package client;

import java.util.Date;
import java.util.List;
import java.util.Set;

import rental.Car;
import rental.CarRentalCompany;
import rental.CarType;
import rental.Quote;
import rental.RemoteCarRentalCompany;
import rental.Reservation;
import rental.ReservationConstraints;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Client extends AbstractTestBooking {
	/********
	 * MAIN *
	 ********/
	public RemoteCarRentalCompany remoteCarRentalCompany = null;
	
	public static void main(String[] args) throws Exception {
		String carRentalCompanyName = "Hertz";
		// An example reservation scenario on car rental company 'Hertz' would be...
		Client client = new Client("simpleTrips", carRentalCompanyName);
		client.run();
	}

	/***************
	 * CONSTRUCTOR *
	 ***************/
	
	public Client(String scriptFile, String carRentalCompanyName) {
		super(scriptFile);
		  try {
	            Registry registry = LocateRegistry.getRegistry(null);
	            remoteCarRentalCompany = (RemoteCarRentalCompany) registry.lookup("carRentalCompany");
	        } catch (Exception e) {
	            System.err.println("Client exception: " + e.toString());
	            e.printStackTrace();
	        }
	}
	
	/**
	 * Check which car types are available in the given period
	 * and print this list of car types.
	 *
	 * @param 	start
	 * 			start time of the period
	 * @param 	end
	 * 			end time of the period
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	@Override
	protected void checkForAvailableCarTypes(Date start, Date end) throws Exception {
		 Set<CarType> carTypes = remoteCarRentalCompany.getAvailableCarTypes(start,end);
		 /*for (CarType carType : carTypes) {
			    System.out.println(carType);
		 }*/
	}

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
	@Override
	protected Quote createQuote(String clientName, Date start, Date end,
			String carType) throws Exception {
		ReservationConstraints reservationConstraints = new ReservationConstraints(start,end,carType);
		 //System.out.println(reservationConstraints);
		return remoteCarRentalCompany.createQuote(reservationConstraints, clientName);
	}

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
	@Override
	protected Reservation confirmQuote(Quote quote) throws Exception {
		return remoteCarRentalCompany.confirmQuote(quote);
	}
	
	/**
	 * Get all reservations made by the given client.
	 *
	 * @param 	clientName
	 * 			name of the client
	 * @return	the list of reservations of the given client
	 * 
	 * @throws 	Exception
	 * 			if things go wrong, throw exception
	 */
	@Override
	protected List<Reservation> getReservationsByRenter(String clientName) throws Exception {
		List<Reservation> reservations = remoteCarRentalCompany.getReservationsByRenter(clientName);
		/*for (Reservation reservation : reservations) {
		    System.out.println(reservation);
		}*/
		return reservations;
	}

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
	@Override
	protected int getNumberOfReservationsForCarType(String carType) throws Exception {
		return remoteCarRentalCompany.getNumberOfReservationsForCarType(carType);
	}
}