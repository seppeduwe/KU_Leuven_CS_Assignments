package session;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import rental.RemoteCarRentalCompany;

public class NameServer {

	private static ArrayList<RemoteCarRentalCompany> companies = new ArrayList<RemoteCarRentalCompany>();
	
	public static void registerCarRentalCompany(RemoteCarRentalCompany company)
			throws RemoteException {
		System.out.println("[NameServer]: company with name: "
				+ company.getName() + " added!");
		companies.add(company);
	}
	
	public static void unregisterCarRentalCompany(RemoteCarRentalCompany company)
			throws RemoteException {
		System.out.println("[NameServer]: company with name: "
				+ company.getName() + " removed!");
		companies.remove(company);
	}
	
	public static void unregisterCarRentalCompany(String name)	throws RemoteException {
		unregisterCarRentalCompany(getCompany(name));
	}

	public static RemoteCarRentalCompany getCompany(String name) throws RemoteException {
		for (RemoteCarRentalCompany company : companies)
			if (company.getName().equals(name))
				return company;
		throw new RemoteException();
	}
	
	public static ArrayList<RemoteCarRentalCompany> getCarRentalCompanies() {
		return new ArrayList<RemoteCarRentalCompany>(companies);
	}
	
	public static Set<String> getCompanies() throws RemoteException {
		Set<String> companiesNames = new HashSet<String>();

		for (RemoteCarRentalCompany company : companies)
			companiesNames.add(company.getName());
		
		return companiesNames;
	}
}
