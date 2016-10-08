package session;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.Stateful;
import rental.CarRentalCompany;
import rental.CarType;
import rental.Quote;
import rental.RentalStore;
import rental.Reservation;
import rental.ReservationConstraints;
import rental.ReservationException;

@Stateful
public class CarRentalSession implements CarRentalSessionRemote {

    static List<Quote> quotes = new ArrayList<Quote>();
    
    @Override
    public Set<String> getAllRentalCompanies() {
        return new HashSet<String>(RentalStore.getRentals().keySet());
    }
    
    @Override
    public void addQuoteToSession(ReservationConstraints constraints, String guest, String carRentalName) throws ReservationException {
        quotes.add(RentalStore.getRentals().get(carRentalName).createQuote(constraints, guest));
        
    }
    
    @Override
    public Quote createQuote(ReservationConstraints constraints, String guest) throws ReservationException{
        Quote quote = null;
        for (CarRentalCompany carRentalCompany : RentalStore.getRentals().values()) {
            try {
                quote = carRentalCompany.createQuote(constraints, guest);
                break;
            } catch (ReservationException ex) {
                Logger.getLogger(CarRentalSession.class.getName()).log(Level.SEVERE, carRentalCompany.getName(), ex);
            }
        }
        if (quote != null) {
            quotes.add(quote);
        } else {
            throw new ReservationException("No cars available to satisfy the given constraints.");
        }
        return quote;
        
        /*Quote quote = null;
        Iterator it = RentalStore.getRentals().entrySet().iterator();
        while (it.hasNext()) {
            try {
                Map.Entry<String, CarRentalCompany> pair = (Map.Entry)it.next();
                quote = pair.getValue().createQuote(constraints, guest);
                it.remove(); // avoids a ConcurrentModificationException
            } catch (ReservationException ex) {
                Logger.getLogger(CarRentalSession.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        if (quote==null) {
            throw new ReservationException("No cars available");
        } else {
            quotes.add(quote);
        }
        return quote;*/
    }

    @Override
    public List<Quote> getCurrentQuotes() {
        return quotes;
    }

    @Override
    public List<Reservation> confirmQuotes(String name) throws ReservationException  {
        List<Reservation> reservations = new ArrayList<Reservation>();
        try {
            for(Quote quote : quotes){
                if(quote.getCarRenter().equals(name))
                    reservations.add(RentalStore.getRentals().get(quote.getRentalCompany()).confirmQuote(quote));
            }
        } catch (ReservationException ex) {
            Logger.getLogger(CarRentalSession.class.getName()).log(Level.SEVERE, null, ex);
            for(Reservation reservation: reservations){
                RentalStore.getRentals().get(reservation.getRentalCompany()).cancelReservation(reservation);
            }
            throw new ReservationException("Quote cannot be finalized: "+ex.getMessage());
        }
        return reservations;
    }  

    @Override
    public Set<CarType> checkForAvailableCarTypes(Date start, Date end) {
        Set<CarType> availableCarTypes = new HashSet<CarType>();
        for(CarRentalCompany rental : RentalStore.getRentals().values()){
            availableCarTypes.addAll(rental.getAvailableCarTypes(start, end));
        }
        return availableCarTypes;
    }
}
