package ds.gae;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.persistence.EntityManager;
import javax.persistence.EntityTransaction;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;

import com.google.appengine.api.datastore.Key;

import ds.gae.entities.Car;
import ds.gae.entities.CarRentalCompany;
import ds.gae.entities.CarType;
import ds.gae.entities.FailedQuote;
import ds.gae.entities.Quote;
import ds.gae.entities.Reservation;
import ds.gae.entities.ReservationConstraints;

@NamedQueries({	
	@NamedQuery(name="getFailedQuotesByRenter", query
			= "SELECT failedQuote "
			+ "FROM FailedQuote failedQuote "
			+ "WHERE failedQuote.carRenter = :carRenter"
			)
})

public class CarRentalModel {
	private static CarRentalModel instance;

	private static Logger logger = Logger.getLogger(CarRentalCompany.class.getName());
	
	public static CarRentalModel get() {
		if (instance == null)
			instance = new CarRentalModel();
		return instance;
	}

	/**
	 * Get the car types available in the given car rental company.
	 *
	 * @param 	crcName
	 * 			the car rental company
	 * @return	The list of car types (i.e. name of car type), available
	 * 			in the given car rental company.
	 */
	public Set<String> getCarTypesNames(String crcName) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			return getCarTypesNames(em, crcName);
		} finally {
			em.close();
		}

	}
	public Set<String> getCarTypesNames(EntityManager em, String crcName) {
		Collection<CarType> carTypes = getCarTypesOfCarRentalCompany(em,crcName);
		Set<String> carTypeNames = new HashSet<String>();
		for (CarType cartype : carTypes) {
			carTypeNames.add(cartype.getName());
		}
		return carTypeNames;
	}

	/**
	 * Get all registered car rental companies
	 *
	 * @return	the list of car rental companies
	 */
	public Collection<String> getAllRentalCompanyNames() {
		EntityManager em = EMF.get().createEntityManager();
		try {
			return getAllRentalCompanyNames(em);
		} finally {
			em.close();
		}
	}

	public Collection<String> getAllRentalCompanyNames(EntityManager em) {
		Collection<String> names = em.createNamedQuery("getAllRentalCompanyNames",String.class)
				.getResultList();
		return names;
	}

	public CarRentalCompany getRentalCompany(String company) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			return getRentalCompany(em, company);
		} finally {
			em.close();
		}
	}

	public CarRentalCompany getRentalCompany(EntityManager em,String company) {
		return em.find(CarRentalCompany.class, company);	
	}


	public void addRentalCompany(CarRentalCompany company) {
		EntityManager em = EMF.get().createEntityManager();
		try{
			addRentalCompany(em,company);
		} finally {
			em.close();
		}
	}

	public void addRentalCompany(EntityManager em, CarRentalCompany company) {
		em.persist(company);
	}

	/**
	 * Create a quote according to the given reservation constraints (tentative reservation).
	 * 
	 * @param	company
	 * 			name of the car renter company
	 * @param	renterName 
	 * 			name of the car renter 
	 * @param 	constraints
	 * 			reservation constraints for the quote
	 * @return	The newly created quote.
	 *  
	 * @throws ReservationException
	 * 			No car available that fits the given constraints.
	 */
	public Quote createQuote(String company, String renterName, ReservationConstraints constraints) throws ReservationException {
		EntityManager em = EMF.get().createEntityManager();
		try{
			CarRentalCompany crc = getRentalCompany(em,company);
			if (crc != null) {
				return crc.createQuote(constraints, renterName);
			} else {
				throw new ReservationException("CarRentalCompany not found.");    	
			}
		}
		finally {
			em.close();
		}
	}

	/**
	 * Confirm the given quote.
	 *
	 * @param 	q
	 * 			Quote to confirm
	 * 
	 * @throws ReservationException
	 * 			Confirmation of given quote failed.	
	 */
	public void confirmQuote(Quote q) throws ReservationException {
		EntityManager em = EMF.get().createEntityManager();
		try {
			confirmQuote(em,q);
		} finally {
			em.close();
		}

	}

	public void confirmQuote(EntityManager em,Quote q) throws ReservationException {
		CarRentalCompany crc = getRentalCompany(em, q.getRentalCompany());
		crc.confirmQuote(q);
	}

	/**
	 * Confirm the given list of quotes
	 * 
	 * @param 	quotes 
	 * 			the quotes to confirm
	 * @return	The list of reservations, resulting from confirming all given quotes.
	 * 
	 * @throws 	ReservationException
	 * 			One of the quotes cannot be confirmed. 
	 * 			Therefore none of the given quotes is confirmed.
	 */
	public List<Reservation> confirmQuotes(List<Quote> quotes) throws ReservationException {    
		Map<String, List<Quote>> groupedQuotes = groupQuotesByCompany(quotes);
		List<Reservation> reservations = new ArrayList<Reservation>();
		for (List<Quote> group : groupedQuotes.values()) {
			try {
				reservations.addAll(confirmQuotesInCompany(group));
			} catch (ReservationException e) {
				logger.log(Level.INFO, e.getMessage());
				for (Quote quote: group){
					addFailedQuote(new FailedQuote(quote,new Date()));
				}
			}
		}
		return reservations;
	}

	public List<Reservation> confirmQuotesInCompany(List<Quote> quotes) throws ReservationException {    
		List<Reservation> reservations = new ArrayList<Reservation>();	
		EntityManager em = EMF.get().createEntityManager();   	
		EntityTransaction t = em.getTransaction();
		try{
			t.begin();
			for(Quote q: quotes){
				CarRentalCompany company = getRentalCompany(em,q.getRentalCompany());
				Reservation res = company.confirmQuote(q);
				reservations.add(res);
			}
			t.commit();
		}catch(Exception e) {
			if (t.isActive()){
				t.rollback();
			}
			throw new ReservationException(e.toString());
		}
		finally{
			em.close();
		}
		return reservations;
	}

	protected Map<String, List<Quote>> groupQuotesByCompany(Collection<Quote> quotes) {
		Map<String, List<Quote>> groupedQuotes = new HashMap<>();
		for (Quote quote : quotes) {
			List<Quote> group = groupedQuotes.get(quote.getRentalCompany());
			if (group == null) {
				group = new ArrayList<>();
				groupedQuotes.put(quote.getRentalCompany(), group);
			}
			group.add(quote);
		}
		return groupedQuotes;
	}


	public void cancelReservation(Reservation res) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			cancelReservation(em, res);
		} finally {
			em.close();
		}
	}

	protected void cancelReservation(EntityManager em, Reservation res) {
		CarRentalCompany crc = getRentalCompany(em, res.getRentalCompany());
		crc.cancelReservation(res);
	}

	/**
	 * Get all reservations made by the given car renter.
	 *
	 * @param 	renter
	 * 			name of the car renter
	 * @return	the list of reservations of the given car renter
	 */
	public List<Reservation> getReservations(String renter) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			return getReservations(em,renter);
		} finally {
			em.close();
		}

	}

	public List<Reservation> getReservations(EntityManager em,String renter) {
		return em.createNamedQuery("getReservationsOfRenter",Reservation.class)
				.setParameter("renter", renter)
				.getResultList();
	}

	/**
	 * Get the car types available in the given car rental company.
	 *
	 * @param 	crcName
	 * 			the given car rental company
	 * @return	The list of car types in the given car rental company.
	 */
	public Collection<CarType> getCarTypesOfCarRentalCompany(String crcName) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			return getCarTypesOfCarRentalCompany(em,crcName);
		} finally {
			em.close();
		}
	}

	public Key addCarType(CarType carType) {
		EntityManager em = EMF.get().createEntityManager();
		try {     
			em.persist(carType);    
		}
		finally {
			em.close();
		}
		return carType.getKey();
	}
	
	public Collection<CarType> getCarTypesOfCarRentalCompany(EntityManager em,String crcName) {
		return ( em.createNamedQuery("getCarTypesOfCarRentalCompany",Map.class)
				.setParameter("crcName", crcName)
				.getSingleResult()).values();
	}

	/**
	 * Get the list of cars of the given car type in the given car rental company.
	 *
	 * @param	crcName
	 * 			name of the car rental company
	 * @param 	carType
	 * 			the given car type
	 * @return	A list of car IDs of cars with the given car type.
	 */
	public Collection<Integer> getCarIdsByCarType(String crcName, CarType carType) {
		Collection<Integer> out = new ArrayList<Integer>();
		for (Car c : getCarsByCarType(crcName, carType)) {
			out.add(c.getId());
		}
		return out;
	}

	/**
	 * Get the amount of cars of the given car type in the given car rental company.
	 *
	 * @param	crcName
	 * 			name of the car rental company
	 * @param 	carType
	 * 			the given car type
	 * @return	A number, representing the amount of cars of the given car type.
	 */
	public int getAmountOfCarsByCarType(String crcName, CarType carType) {
		return this.getCarsByCarType(crcName, carType).size();
	}

	/**
	 * Get the list of cars of the given car type in the given car rental company.
	 *
	 * @param	crcName
	 * 			name of the car rental company
	 * @param 	carType
	 * 			the given car type
	 * @return	List of cars of the given car type
	 */
	private List<Car> getCarsByCarType(String crcName, CarType carType) {	
		EntityManager em = EMF.get().createEntityManager();
		try{
			return getCarsByCarType(em,crcName,carType);
		} finally {
			em.close();
		}
	}

	private List<Car> getCarsByCarType(EntityManager em,String crcName, CarType carType) {	
		Set<Car> cars = new HashSet<Car>(em.createNamedQuery("getCarsByCarType",Set.class)
				.setParameter("company", crcName)
				.getSingleResult());

		ArrayList<Car> result = new ArrayList<Car>();
		for(Car c : cars) {
			if(c.getType().equals(carType)) {
				result.add(c);
			}
		}
		return result;
	}

	/**
	 * Check whether the given car renter has reservations.
	 *
	 * @param 	renter
	 * 			the car renter
	 * @return	True if the number of reservations of the given car renter is higher than 0.
	 * 			False otherwise.
	 */
	public boolean hasReservations(String renter) {
		return this.getReservations(renter).size() > 0;		
	}

	/***************
	 * FAILED QUOTES *
	 ***************/

	/**
	 * Add a failed quote.
	 * 
	 * @param	failedQuote
	 * 			Object failedQuote
	 * 
	 */
	public void addFailedQuote(FailedQuote failedQuote) {
		EntityManager em = EMF.get().createEntityManager();
		try {
			addFailedQuote(em, failedQuote);
		} finally {
			em.close();
		}
	}

	private void addFailedQuote(EntityManager em, FailedQuote failedQuote) {
		em.persist(failedQuote);
	}

	public List<FailedQuote> getFailedQuoteByRenter(String renter) {	
		EntityManager em = EMF.get().createEntityManager();
		try{
			return getFailedQuoteByRenter(em,renter);
		} finally {
			em.close();
		}
	}

	private List<FailedQuote> getFailedQuoteByRenter(EntityManager em,String renter){
		return em.createNamedQuery("getFailedQuotesByRenter",FailedQuote.class)
				.setParameter("carRenter", renter)
				.getResultList();
	}
}