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
import session.ManagerSession;
import session.RemoteManagerSession;
import session.RemoteReservationSession;
import session.RemoteSessionFactory;
import session.ReservationSession;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Client extends AbstractTestManagement<RemoteReservationSession, RemoteManagerSession> {
	/********
	 * MAIN *
	 ********/
	public RemoteSessionFactory remoteSessionFactory = null;
	
	public static void main(String[] args) throws Exception {
		Client client = new Client("trips");
		client.run();
	}

	/***************
	 * CONSTRUCTOR *
	 ***************/
	
	public Client(String scriptFile) {
		super(scriptFile);
		  try {
	            Registry registry = LocateRegistry.getRegistry(null);
	    		remoteSessionFactory = (RemoteSessionFactory) registry.lookup("carRentalAgency");
	    		
	        } catch (Exception e) {
	            System.err.println("Client exception: " + e.toString());
	            e.printStackTrace();
	        }
	}

	@Override
	protected String getCheapestCarType(RemoteReservationSession session,
			Date start, Date end) throws Exception {
		return session.getCheapestCarType(start, end);
	}

	@Override
	protected String getMostPopularCarRentalCompany(RemoteManagerSession ms)
			throws Exception {
		return ms.getMostPopularCarRentalCompany();
	}

	@Override
	protected RemoteReservationSession getNewReservationSession(String name)
			throws Exception {
		RemoteReservationSession rentalSession = remoteSessionFactory.getReservationSession(name);
    	return rentalSession;
	}

	@Override
	protected RemoteManagerSession getNewManagerSession(String name,
			String carRentalName) throws Exception {
		RemoteManagerSession managerSession = remoteSessionFactory.getManagerSession(name);
		return managerSession;
	}

	@Override
	protected void checkForAvailableCarTypes(RemoteReservationSession session,
			Date start, Date end) throws Exception {
		for(CarType carType : session.getAvailableCarTypes(start, end)){
            System.out.println(carType.toString());
        }
	}

	@Override
	protected void addQuoteToSession(RemoteReservationSession session,
			String name, Date start, Date end, String carType,
			String carRentalName) throws Exception {
		ReservationConstraints constraints = new ReservationConstraints(start, end, carType);
		session.createQuote(constraints, carRentalName);
	}

	@Override
	protected List<Reservation> confirmQuotes(RemoteReservationSession session,
			String name) throws Exception {
		return session.confirmQuotes();
	}

	@Override
	protected int getNumberOfReservationsBy(RemoteManagerSession ms,
			String clientName) throws Exception {
		return ms.getNuberOfReservationsByCarRenter(clientName);
	}

	@Override
	protected int getNumberOfReservationsForCarType(RemoteManagerSession ms,
			String carRentalName, String carType) throws Exception {
		return ms.getNumberOfReservationsByCarType(carRentalName, carType);
	}

	
}