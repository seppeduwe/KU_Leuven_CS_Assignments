package rental;

public class ReservationException extends Exception {

    public ReservationException(String string) {
        super(string);
    }
    
    public ReservationException(Throwable t) {
        super(t);
    }
}