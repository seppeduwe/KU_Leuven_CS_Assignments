package session;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import rental.Car;
import rental.CarType;
import rental.RemoteCarRentalCompany;
import rental.Reservation;

public class ManagerSession extends Session implements RemoteManagerSession {
		
	public ManagerSession(String name) {
		super(name);
	}

	@Override
	public void registerCarRentalCompany(RemoteCarRentalCompany company)
			throws RemoteException {
		NameServer.registerCarRentalCompany(company);
	}

	@Override
	public void unregisterCarRentalCompany(String companyName)
			throws RemoteException {
		NameServer.unregisterCarRentalCompany(companyName);
	}

	@Override
	public Set<String> getRegisteredCompanyNames() throws RemoteException {
		return NameServer.getCompanies();
	}

	@Override
	public Set<String> getCarTypes(String carRentalCompany) throws Exception {
		RemoteCarRentalCompany carRentalComp = NameServer.getCompany(carRentalCompany);
		if(carRentalComp == null) {
			throw new IllegalArgumentException("No CarRentalCompany found with name " + carRentalCompany + ".");
		}
		Collection<CarType> carTypes = carRentalComp.getCarTypes();

		Set<String> carTypesStr = new HashSet<String>();
		for(CarType carType : carTypes) {
			carTypesStr.add(carType.getName());
		}

		return carTypesStr;
		
	}

	@Override
	public int getNumberOfReservationsByCarType(String carRentalCompany,
			String carType) throws Exception {
		RemoteCarRentalCompany carRentalComp = NameServer.getCompany(carRentalCompany);
		if(carRentalComp == null) {
			throw new IllegalArgumentException("No CarRentalCompany found with name " + carRentalCompany + ".");
		}
		Set<Car> carsByType = carRentalComp.getCarsByType(carType);
		List<Reservation> resByCarType = new ArrayList<Reservation>();
		for (Car car : carsByType) {
			resByCarType.addAll(car.getReservations());
		}
		return resByCarType.size();
	}

	@Override
	public int getNuberOfReservationsByCarRenter(String carRenter)
			throws RemoteException {
		int totalNbOfRes = 0;
		for (RemoteCarRentalCompany carRentalComp : NameServer.getCarRentalCompanies()) {
			totalNbOfRes += carRentalComp.getReservationsByRenter(carRenter).size();
		}
		return totalNbOfRes;  
	}

	@Override
	public String getMostPopularCarRentalCompany() throws Exception {
		ArrayList<RemoteCarRentalCompany> carRentalCompanies = NameServer.getCarRentalCompanies();
		RemoteCarRentalCompany popular = carRentalCompanies.get(0);
		for (RemoteCarRentalCompany carRentalComp : carRentalCompanies) {
			if(popular.getNumberOfReservations() < carRentalComp.getNumberOfReservations())
				popular = carRentalComp;
		}
		return popular.getName();
	}

	@Override
	public void setManagerName(String name) throws RemoteException {
		super.setName(name);
	}
}
