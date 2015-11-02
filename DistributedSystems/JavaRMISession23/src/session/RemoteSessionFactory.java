package session;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteSessionFactory extends Remote {
	 RemoteReservationSession getReservationSession(String name) throws RemoteException;

	 RemoteManagerSession getManagerSession(String name) throws RemoteException;
}
