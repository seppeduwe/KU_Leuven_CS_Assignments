package rental;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.persistence.CascadeType;
import static javax.persistence.CascadeType.DETACH;
import static javax.persistence.CascadeType.MERGE;
import static javax.persistence.CascadeType.PERSIST;
import static javax.persistence.CascadeType.REFRESH;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;


@NamedQueries({
    @NamedQuery(name = "getAllCompanies", query = "SELECT c FROM CarRentalCompany c"),

    @NamedQuery(name = "getAvailableCarTypes", query
            = "SELECT DISTINCT c.type "
            + "FROM Car c "
            + "WHERE c.id NOT IN( "
            + "     SELECT r.carId "
            + "     FROM Reservation r "
            + "     WHERE (r.startDate <= :givenStartDate AND r.endDate >= :givenStartDate) "
            + "            OR (r.startDate <= :givenEndDate AND r.endDate >= :givenEndDate)) " // Optimize with Between
    ),

    @NamedQuery(name = "getNumberOfReservationsByType", query
            = "SELECT COUNT(c.reservations) "
            + "FROM CarRentalCompany crc, IN (crc.cars) c "
            + "WHERE crc.name = :company AND c.type.name = :type"
    ),

    @NamedQuery(name = "getNumberOfReservationsByRenter", query
            = "SELECT COUNT(r) "
            + "FROM Reservation r "
            + "WHERE r.carRenter = :renter"
    ),

    @NamedQuery(name = "getNumberOfReservationsByCarId", query
            = "SELECT COUNT(r) "
            + "FROM Reservation r "
            + "WHERE r.carId = :id"
    ),

    @NamedQuery(name = "getCarTypes", query
            = "SELECT crc.carTypes "
            + "FROM CarRentalCompany crc "
            + "WHERE crc.name = :company"
    ),

    @NamedQuery(name = "getCheapestCarType", query
            = "SELECT c.type "
            + "FROM Car c "
            + "WHERE c.id NOT IN( "
            + "     SELECT r.carId "
            + "     FROM Reservation r "
            + "     WHERE (r.startDate <= :givenStartDate AND r.endDate >= :givenStartDate) "
            + "            OR (r.startDate <= :givenEndDate AND r.endDate >= :givenEndDate)) "
            + "ORDER BY c.type.rentalPricePerDay asc "
    ),

    //Removed -> use em.find(...)
    //@NamedQuery(name="getCarRentalCompany", query="SELECT c FROM CarRentalCompany c WHERE c.name = :name"),
    
    @NamedQuery(name = "getMostPopularCarRentalCompany", query
            = "SELECT crc "
            + "FROM CarRentalCompany crc, IN (crc.cars) c, IN (c.reservations) r "
            + "GROUP BY crc "
            + "ORDER BY COUNT(r) desc"
    )
})
@Entity
public class CarRentalCompany implements Serializable {

    private static Logger logger = Logger.getLogger(CarRentalCompany.class.getName());

    private String name;

    private List<Car> cars;

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    public List<Car> getCars() {
        return cars;
    }

    public void setCars(List<Car> cars) {
        this.cars = cars;
    }
    private Set<CarType> carTypes = new HashSet<CarType>();

    @ManyToMany(cascade = {PERSIST, MERGE, REFRESH, DETACH}, fetch = FetchType.LAZY)
    public Set<CarType> getCarTypes() {
        return carTypes;
    }

    public void setCarTypes(Set<CarType> carTypes) {
        this.carTypes = carTypes;
    }

    public void setCarType(CarType carType){
        carTypes.add(carType);
    }
    /**
     * *************
     * CONSTRUCTOR *
     *************
     */
    public CarRentalCompany() {
    }
    
    public CarRentalCompany(String name) {
        this(name, new LinkedList<Car>());
    }
    
    public CarRentalCompany(String name, List<Car> cars) {
        logger.log(Level.INFO, "<{0}> Car Rental Company {0} starting up...", name);
        setName(name);
        this.cars = cars;
        for (Car car : cars) {
            carTypes.add(car.getType());
        }
    }

    /**
     * ******
     * NAME * ******
     */
    @Id
    public String getName() {
        return name;
    }

    private void setName(String name) {
        this.name = name;
    }

    /**
     * ***********
     * CAR TYPES * ***********
     */
    public Collection<CarType> getAllTypes() {
        return carTypes;
    }

    public CarType getType(String carTypeName) {
        for (CarType type : carTypes) {
            if (type.getName().equals(carTypeName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("<" + carTypeName + "> No cartype of name " + carTypeName);
    }

    public boolean isAvailable(String carTypeName, Date start, Date end) {
        logger.log(Level.INFO, "<{0}> Checking availability for car type {1}", new Object[]{name, carTypeName});
        return getAvailableCarTypes(start, end).contains(getType(carTypeName));
    }

    public Set<CarType> getAvailableCarTypes(Date start, Date end) {
        Set<CarType> availableCarTypes = new HashSet<CarType>();
        for (Car car : cars) {
            if (car.isAvailable(start, end)) {
                availableCarTypes.add(car.getType());
            }
        }
        return availableCarTypes;
    }

    /**
     * *******
     * CARS * *******
     */
    public Car getCar(int uid) {
        for (Car car : cars) {
            if (car.getId() == uid) {
                return car;
            }
        }
        throw new IllegalArgumentException("<" + name + "> No car with uid " + uid);
    }

    public Set<Car> getCars(CarType type) {
        Set<Car> out = new HashSet<Car>();
        for (Car car : cars) {
            if (car.getType().equals(type)) {
                out.add(car);
            }
        }
        return out;
    }

    public Set<Car> getCars(String type) {
        Set<Car> out = new HashSet<Car>();
        for (Car car : cars) {
            if (type.equals(car.getType().getName())) {
                out.add(car);
            }
        }
        return out;
    }
    
    public void setCar(Car car) {
        cars.add(car);
    }
        
    private List<Car> getAvailableCars(String carType, Date start, Date end) {
        List<Car> availableCars = new LinkedList<Car>();
        for (Car car : cars) {
            if (car.getType().getName().equals(carType) && car.isAvailable(start, end)) {
                availableCars.add(car);
            }
        }
        return availableCars;
    }

    /**
     * **************
     * RESERVATIONS * **************
     */
    public Quote createQuote(ReservationConstraints constraints, String guest)
            throws ReservationException {
        logger.log(Level.INFO, "<{0}> Creating tentative reservation for {1} with constraints {2}",
                new Object[]{name, guest, constraints.toString()});

        CarType type = getType(constraints.getCarType());

        if (!isAvailable(constraints.getCarType(), constraints.getStartDate(), constraints.getEndDate())) {
            throw new ReservationException("<" + name
                    + "> No cars available to satisfy the given constraints.");
        }

        double price = calculateRentalPrice(type.getRentalPricePerDay(), constraints.getStartDate(), constraints.getEndDate());

        return new Quote(guest, constraints.getStartDate(), constraints.getEndDate(), getName(), constraints.getCarType(), price);
    }

    // Implementation can be subject to different pricing strategies
    private double calculateRentalPrice(double rentalPricePerDay, Date start, Date end) {
        return rentalPricePerDay * Math.ceil((end.getTime() - start.getTime())
                / (1000 * 60 * 60 * 24D));
    }

    public Reservation confirmQuote(Quote quote) throws ReservationException {
        logger.log(Level.INFO, "<{0}> Reservation of {1}", new Object[]{name, quote.toString()});
        List<Car> availableCars = getAvailableCars(quote.getCarType(), quote.getStartDate(), quote.getEndDate());
        if (availableCars.isEmpty()) {
            throw new ReservationException("Reservation failed, all cars of type " + quote.getCarType()
                    + " are unavailable from " + quote.getStartDate() + " to " + quote.getEndDate());
        }
        Car car = availableCars.get((int) (Math.random() * availableCars.size()));

        Reservation res = new Reservation(quote, car.getId());
        car.addReservation(res);
        return res;
    }

    public void cancelReservation(Reservation res) {
        logger.log(Level.INFO, "<{0}> Cancelling reservation {1}", new Object[]{name, res.toString()});
        getCar(res.getCarId()).removeReservation(res);
    }

    public Set<Reservation> getReservationsBy(String renter) {
        logger.log(Level.INFO, "<{0}> Retrieving reservations by {1}", new Object[]{name, renter});
        Set<Reservation> out = new HashSet<Reservation>();
        for (Car c : cars) {
            for (Reservation r : c.getReservations()) {
                if (r.getCarRenter().equals(renter)) {
                    out.add(r);
                }
            }
        }
        return out;
    }
}