/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package session;

import java.util.Collection;
import java.util.Map;
import javax.ejb.Stateless;
import rental.CarRentalCompany;
import rental.CarType;
import rental.RentalStore;

/**
 *
 * @author Xavier and Seppe
 */
@Stateless
public class ManagerSession implements ManagerSessionRemote {

    @Override
    public Collection<CarType> getAllTypesOfCompany(String company) {
        return RentalStore.getRentals().get(company).getAllTypes();
    }

    @Override
    public Map<CarType, Integer> getReservationsPerCarType(String company) {
        return RentalStore.getRentals().get(company).getReservationsPerCarType();
    }

    @Override
    public int getNumberOfReservationsBy(String client) {
        int total = 0;
        for (CarRentalCompany company : RentalStore.getRentals().values()) {
            total += company.getNumberOfReservationsBy(client);
        }
        return total;
    }

    @Override
    public int getNumberOfReservationsForCarType(String carRentalName, String carType) {
        return RentalStore.getRentals().get(carRentalName).getNumberOfReservationsForCarType(carType);
    }
}
