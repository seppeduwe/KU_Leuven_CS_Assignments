package session;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

import rental.CarType;
import rental.RemoteCarRentalCompany;

public interface RemoteManagerSession extends Remote {
	void registerCarRentalCompany(RemoteCarRentalCompany company) throws RemoteException;
	
	void unregisterCarRentalCompany(String companyName) throws RemoteException;
	
	Set<String> getRegisteredCompanyNames() throws RemoteException;
		
	Set<String> getCarTypes(String carRentalCompany) throws Exception;
    
    int getNumberOfReservationsByCarType(String carRentalCompany, String carType) throws Exception;
    
    int getNuberOfReservationsByCarRenter (String carRenter) throws RemoteException;

	String getMostPopularCarRentalCompany() throws Exception;
	
	void setManagerName(String name) throws RemoteException;
}
