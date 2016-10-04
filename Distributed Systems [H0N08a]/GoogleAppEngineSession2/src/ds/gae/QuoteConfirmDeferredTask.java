package ds.gae;

import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.appengine.api.taskqueue.DeferredTask;

import ds.gae.entities.CarRentalCompany;
import ds.gae.entities.Quote;

public class QuoteConfirmDeferredTask implements DeferredTask {

	private static Logger logger = Logger.getLogger(CarRentalCompany.class.getName());
	
	private ArrayList<Quote> qs;

	public QuoteConfirmDeferredTask(ArrayList<Quote> qs) {
		this.qs = qs;
	}

	@Override
	public void run() {
		try {
			CarRentalModel.get().confirmQuotes(qs);
		} catch (ReservationException e) {
			logger.log(Level.INFO, e.getMessage());
		}
	}
}