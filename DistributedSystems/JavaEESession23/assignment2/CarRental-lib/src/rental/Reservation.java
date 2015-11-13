package rental;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Reservation extends Quote {

    private int carId;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private int id;

    public Reservation() {
    }

    /**
     * *************
     * CONSTRUCTOR *
     **************
     */
    public Reservation(Quote quote, int carId) {
        super(quote.getCarRenter(), quote.getStartDate(), quote.getEndDate(),
                quote.getRentalCompany(), quote.getCarType(), quote.getRentalPrice());
        this.carId = carId;
    }

    /**
     * ****
     * ID *
     *****
     */
    public int getCarId() {
        return carId;
    }

    public void setCarId(int carId) {
        this.carId = carId;
    }

    /**
     * ***********
     * TO STRING *
     ************
     */
    @Override
    public String toString() {
        return String.format("Reservation for %s from %s to %s at %s\nCar type: %s\tCar: %s\nTotal price: %.2f",
                getCarRenter(), getStartDate(), getEndDate(), getRentalCompany(), getCarType(), getCarId(), getRentalPrice());
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}