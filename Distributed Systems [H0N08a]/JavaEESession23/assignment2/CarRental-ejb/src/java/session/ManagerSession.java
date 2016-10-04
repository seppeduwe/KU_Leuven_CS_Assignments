package session;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import rental.Car;
import rental.CarRentalCompany;
import rental.CarType;

@Stateless
public class ManagerSession implements ManagerSessionRemote {

    @PersistenceContext
    private EntityManager em;

    @Override
    public Set<CarType> getCarTypes(String company) {
        try { // Edit
            return new HashSet<CarType>(em.createNamedQuery("getCarTypes")
                    .setParameter("company", company)
                    .getResultList());
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(ManagerSession.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
    }

    @Override
    public Set<Integer> getCarIds(String company, String type) {
        Set<Integer> out = new HashSet<Integer>();
        try {
            CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, company);
            for (Car c : carRentalCompany.getCars(type)) {
                out.add(c.getId());
            }
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(ManagerSession.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
        return out;
    }

    @Override
    public int getNumberOfReservations(String company, String type, int id) {
        try {
            /*CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, company);
             return carRentalCompany.getCar(id).getReservations().size();*/
            return em.createNamedQuery("getNumberOfReservationsByCarId", Number.class)
                    //.setParameter("company", company)
                    //.setParameter("type", type)
                    .setParameter("id", id)
                    .getSingleResult()
                    .intValue();

        } catch (IllegalArgumentException ex) {
            Logger.getLogger(ManagerSession.class.getName()).log(Level.SEVERE, null, ex);
            return 0;
        }
    }

    @Override
    public int getNumberOfReservations(String company, String type) {
        return em.createNamedQuery("getNumberOfReservationsByType", Number.class)
                .setParameter("company", company)
                .setParameter("type", type)
                .getSingleResult()
                .intValue();
    }

    @Override
    public int getNumberOfReservationsBy(String renter) {
        return em.createNamedQuery("getNumberOfReservationsByRenter", Number.class)
                .setParameter("renter", "Jos")
                .getSingleResult()
                .intValue();
    }

    @Override
    public String getMostPopularCarRentalCompany() {
        CarRentalCompany crc = em.createNamedQuery("getMostPopularCarRentalCompany", CarRentalCompany.class)
                .setMaxResults(1)
                .getSingleResult();
        return crc.getName();
    }
    
    @Override
    public void addCar(String company, int uid, CarType type) throws Exception {
        addCar(company,new Car(uid, type));
    }

    @Override
    public void addCar(String company, Car car) throws Exception {
        CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, company);
        carRentalCompany.setCar(car);
        em.persist(carRentalCompany);
    }
        
    @Override
    public void addCarType(String company, CarType carType) throws Exception {
        CarRentalCompany carRentalCompany = em.find(CarRentalCompany.class, company);
        carRentalCompany.setCarType(carType);
        em.persist(carRentalCompany);
    }
    
    @Override
    public void addCompany(String name) throws Exception {
         em.persist(new CarRentalCompany(name));
    }
    
    @Override
    public void addCompany(String name, List<Car> cars) throws Exception {
        em.persist(new CarRentalCompany(name,cars));
    }

    @Override
    public void loadRental(String name, String datafile) {
        Logger.getLogger(ManagerSession.class.getName()).log(Level.INFO, "loading {0} from file {1}", new Object[]{name, datafile});
        try {
            List<Car> cars = loadData(datafile);
            CarRentalCompany company = new CarRentalCompany(name, cars);
            em.persist(company);
        } catch (NumberFormatException ex) {
            Logger.getLogger(ManagerSession.class.getName()).log(Level.SEVERE, "bad file", ex);
        } catch (IOException ex) {
            Logger.getLogger(ManagerSession.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private List<Car> loadData(String datafile)
            throws NumberFormatException, IOException {

        List<Car> cars = new LinkedList<Car>();

        int nextuid = 0;

        //open file from jar
        BufferedReader in = new BufferedReader(new InputStreamReader(ManagerSession.class.getClassLoader().getResourceAsStream(datafile)));
        //while next line exists
        while (in.ready()) {
            //read line
            String line = in.readLine();
            //if comment: skip
            if (line.startsWith("#")) {
                continue;
            }
            //tokenize on ,
            StringTokenizer csvReader = new StringTokenizer(line, ",");
            //create new car type from first 5 fields
            CarType type = new CarType(csvReader.nextToken(),
                    Integer.parseInt(csvReader.nextToken()),
                    Float.parseFloat(csvReader.nextToken()),
                    Double.parseDouble(csvReader.nextToken()),
                    Boolean.parseBoolean(csvReader.nextToken()));
            //create N new cars with given type, where N is the 5th field
            for (int i = Integer.parseInt(csvReader.nextToken()); i > 0; i--) {
                cars.add(new Car(nextuid++, type));
            }
        }
        return cars;
    }
}