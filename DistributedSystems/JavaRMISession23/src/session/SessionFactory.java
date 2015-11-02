package session;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;


public class SessionFactory  implements RemoteSessionFactory {
	
	@Override
	public RemoteReservationSession getReservationSession(String name) throws RemoteException {
		ReservationSession session = new ReservationSession(name);
		return (RemoteReservationSession) UnicastRemoteObject.exportObject(session, 0);
	}

	@Override
	public RemoteManagerSession getManagerSession(String name) throws RemoteException {
		ManagerSession session  = new ManagerSession(name);
		return (RemoteManagerSession) UnicastRemoteObject.exportObject(session, 0);
	}

}
