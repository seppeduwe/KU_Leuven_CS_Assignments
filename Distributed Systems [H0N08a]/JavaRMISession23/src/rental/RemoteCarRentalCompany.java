package rental;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

public interface RemoteCarRentalCompany extends Remote{
	Quote createQuote(ReservationConstraints constraints, String client) throws RemoteException, ReservationException;
	Set<CarType> getAvailableCarTypes(Date start, Date end) throws RemoteException;
	Reservation confirmQuote(Quote quote) throws ReservationException,RemoteException;
	List<Reservation> getReservationsByRenter(String clientName) throws RemoteException;
	int getNumberOfReservationsForCarType(String carType) throws RemoteException;
	String getName() throws RemoteException;
	Set<Car> getCarsByType(String carType) throws RemoteException;
	int getNumberOfReservations() throws RemoteException;
	Collection<CarType> getCarTypes() throws RemoteException;
	void cancelReservation(Reservation reservation) throws RemoteException;
	CarType getCheapestType(Date start, Date end) throws RemoteException, Exception;
}