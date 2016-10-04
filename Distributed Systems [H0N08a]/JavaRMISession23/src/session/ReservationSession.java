package session;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import rental.CarType;
import rental.Quote;
import rental.RemoteCarRentalCompany;
import rental.Reservation;
import rental.ReservationConstraints;
import rental.ReservationException;

public class ReservationSession extends Session implements RemoteReservationSession{

	private List<Quote> quotes = new ArrayList<Quote>();

	
	public ReservationSession(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	@Override
	public Set<String> getAllRentalCompanies() throws RemoteException {
		return NameServer.getCompanies();
	}

	@Override
	public void createQuote(ReservationConstraints constraints, String carRentalName)
			throws ReservationException, RemoteException {
		Quote quote = NameServer.getCompany(carRentalName).createQuote(constraints, super.getName());
		quotes.add(quote);
		
	}

	@Override
	public List<Quote> getCurrentQuotes() throws RemoteException {
		return quotes;
	}

	@Override
	public synchronized List<Reservation> confirmQuotes() throws ReservationException,
			RemoteException {
		List<Reservation> reservations = new ArrayList<Reservation>();
        try {
            for(Quote quote : quotes){
                    reservations.add(NameServer.getCompany(quote.getRentalCompany()).confirmQuote(quote));
            }
        } catch (ReservationException ex) {
            //Logger.getLogger(ReservationSession.class.getName()).log(Level.SEVERE, null, ex);
            for(Reservation reservation: reservations){
            	NameServer.getCompany(reservation.getRentalCompany()).cancelReservation(reservation);
            }
            throw new ReservationException("Quote cannot be finalized: "+ex.getMessage());
        }
        return reservations;
	}

	@Override
	public Set<CarType> getAvailableCarTypes(Date start, Date end)
			throws RemoteException {
		HashSet<CarType> carTypes = new HashSet<CarType>();
		for(RemoteCarRentalCompany company : NameServer.getCarRentalCompanies()){
			carTypes.addAll(company.getAvailableCarTypes(start, end));
		}
		return carTypes;
	}

	@Override
	public void setClientName(String name) throws RemoteException {
		super.setName(name);
	}

	@Override
	public String getCheapestCarType(Date start, Date end)
			throws RemoteException, Exception {
		CarType cheapest = null;
		double cheapestPrice = new Double(Double.MAX_VALUE);;
		for(RemoteCarRentalCompany company : NameServer.getCarRentalCompanies()) {
			if(company.getCheapestType(start, end).getRentalPricePerDay() < cheapestPrice) {
				cheapest = company.getCheapestType(start, end);
				cheapestPrice = company.getCheapestType(start, end).getRentalPricePerDay();
			}	
		}
		if(cheapest == null) {
			throw new Exception("No cheapest car type found!");
		}
		return cheapest.getName();
	}

}
