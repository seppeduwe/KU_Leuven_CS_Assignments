package session;

import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import javax.ejb.EJBException;
import javax.ejb.Stateful;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import rental.CarRentalCompany;
import rental.CarType;
import rental.Quote;
import rental.Reservation;
import rental.ReservationConstraints;
import rental.ReservationException;

@Stateful
public class CarRentalSession implements CarRentalSessionRemote {

    @PersistenceContext
    EntityManager em;

    private String renter;
    private List<Quote> quotes = new LinkedList<Quote>();

    @Override
    public Set<String> getAllRentalCompanies() {
        return new HashSet<String>(em.createNamedQuery("getAllCompanies").getResultList());
    }

    @Override
    public List<CarType> getAvailableCarTypes(Date start, Date end) {
        return em.createNamedQuery("getAvailableCarTypes", CarType.class)
                .setParameter("givenStartDate", start)
                .setParameter("givenEndDate", end)
                .getResultList();
    }

    @Override
    public Quote createQuote(String company, ReservationConstraints constraints) throws ReservationException {
        try {
            CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, company);
            Quote out = carRentalCompany.createQuote(constraints, renter);
            quotes.add(out);
            return out;
        } catch (Exception e) {
            throw new ReservationException(e);
        }
    }

    @Override
    public List<Quote> getCurrentQuotes() {
        return quotes;
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    public List<Reservation> confirmQuotes() throws ReservationException {

        List<Reservation> done = new LinkedList<Reservation>();
        try {
            for (Quote quote : quotes) {
                CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, quote.getRentalCompany());
                done.add(carRentalCompany.confirmQuote(quote));
            }
        } catch (Exception e) {
            throw new EJBException("Transaction failed: " + e.getMessage());
            //throw new ReservationException(e);
        }

        return done;
    }

    @Override
    public void setRenterName(String name) {
        if (renter != null) {
            throw new IllegalStateException("name already set");
        }
        renter = name;
    }

    @Override
    public String getCheapestCarType(Date start, Date end) {
        CarType carType = em.createNamedQuery("getCheapestCarType", CarType.class)
                .setParameter("givenStartDate", start)
                .setParameter("givenEndDate", end)
                .setMaxResults(1).getSingleResult();
        return carType.getName();
    }
}