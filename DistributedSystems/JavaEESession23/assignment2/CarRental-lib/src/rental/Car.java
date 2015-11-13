package rental;

import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import javax.persistence.CascadeType;
import static javax.persistence.CascadeType.MERGE;
import static javax.persistence.CascadeType.PERSIST;
import static javax.persistence.CascadeType.REFRESH;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

@Entity
public class Car implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @ManyToOne(cascade = {PERSIST, MERGE, REFRESH}, fetch = FetchType.EAGER)
    private CarType type;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private Set<Reservation> reservations;

    public Car() {
    }

    /**
     * *************
     * CONSTRUCTOR *
     **************
     */
    public Car(int uid, CarType type) {
        this.id = uid;
        this.type = type;
        this.reservations = new HashSet<Reservation>();
    }

    /**
     * ****
     * ID *
     *****
     */
    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    /**
     * **********
     * CAR TYPE *
     ***********
     */
    public CarType getType() {
        return type;
    }

    public void setType(CarType type) {
        this.type = type;
    }

    /**
     * **************
     * RESERVATIONS *
     ***************
     */
    public boolean isAvailable(Date start, Date end) {
        if (!start.before(end)) {
            throw new IllegalArgumentException("Illegal given period");
        }

        for (Reservation reservation : reservations) {
            if (reservation.getEndDate().before(start) || reservation.getStartDate().after(end)) {
                continue;
            }
            return false;
        }
        return true;
    }

    public void addReservation(Reservation res) {
        reservations.add(res);
    }

    public void removeReservation(Reservation reservation) {
        // equals-method for Reservation is required!
        reservations.remove(reservation);
    }

    public Set<Reservation> getReservations() {
        return reservations;
    }
}