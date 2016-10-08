package session;

import java.util.List;
import java.util.Set;
import javax.ejb.Remote;
import rental.Car;
import rental.CarType;

@Remote
public interface ManagerSessionRemote {
    
    public Set<CarType> getCarTypes(String company);
    
    public Set<Integer> getCarIds(String company,String type);
    
    public int getNumberOfReservations(String company, String type, int carId);
    
    public int getNumberOfReservations(String company, String type);
      
    public int getNumberOfReservationsBy(String renter);

    public String getMostPopularCarRentalCompany();
    
    public void loadRental(String name, String datafile);
        
    public void addCompany(String name) throws Exception;
    
    public void addCompany(String name, List<Car> cars) throws Exception;
    
    public void addCar(String company, int uid, CarType type) throws Exception;
    
    public void addCar(String company, Car car) throws Exception;
    
    public void addCarType(String company, CarType carType) throws Exception;
    
}