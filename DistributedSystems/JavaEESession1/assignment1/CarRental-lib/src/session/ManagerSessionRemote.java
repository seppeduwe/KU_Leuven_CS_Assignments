/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package session;

import java.util.Collection;
import java.util.Map;
import javax.ejb.Remote;
import rental.CarType;

/**
 *
 * @author xavier
 */
@Remote
public interface ManagerSessionRemote {

    Collection<CarType> getAllTypesOfCompany(String company);

    Map<CarType, Integer> getReservationsPerCarType(String company);

    int getNumberOfReservationsForCarType(String carRentalName, String carType);

    int getNumberOfReservationsBy(String client);
}
