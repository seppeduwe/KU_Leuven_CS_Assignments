package rental;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import session.RemoteManagerSession;
import session.RemoteSessionFactory;

import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class RentalServerHertz {

	public static void main(String[] args) throws ReservationException, NumberFormatException, IOException {
		List<Car> cars = loadData("hertz.csv");
		//CarRentalCompany carRentalCompanyHertz = new CarRentalCompany("Hertz", cars);
		RemoteCarRentalCompany carRentalCompanyHertz = (RemoteCarRentalCompany) UnicastRemoteObject.exportObject(new CarRentalCompany("Hertz", cars), 0);
		Registry registry = LocateRegistry.getRegistry(null);
		try {
			RemoteSessionFactory remoteSessionFactory = (RemoteSessionFactory) registry.lookup("carRentalAgency");
			RemoteManagerSession managerSessionHertz = remoteSessionFactory.getManagerSession("managerHertz");
			managerSessionHertz.registerCarRentalCompany(carRentalCompanyHertz);
		} catch (NotBoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static List<Car> loadData(String datafile)
			throws ReservationException, NumberFormatException, IOException {

		List<Car> cars = new LinkedList<Car>();

		int nextuid = 0;

		// open file
		BufferedReader in = new BufferedReader(new FileReader(datafile));

		try {
			// while next line exists
			while (in.ready()) {
				// read line
				String line = in.readLine();
				// if comment: skip
				if (line.startsWith("#"))
					continue;
				// tokenize on ,
				StringTokenizer csvReader = new StringTokenizer(line, ",");
				// create new car type from first 5 fields
				CarType type = new CarType(csvReader.nextToken(),
						Integer.parseInt(csvReader.nextToken()),
						Float.parseFloat(csvReader.nextToken()),
						Double.parseDouble(csvReader.nextToken()),
						Boolean.parseBoolean(csvReader.nextToken()));
				System.out.println(type);
				// create N new cars with given type, where N is the 5th field
				for (int i = Integer.parseInt(csvReader.nextToken()); i > 0; i--) {
					cars.add(new Car(nextuid++, type));
				}
			}
		} finally {
			in.close();
		}

		return cars;
	}
}
