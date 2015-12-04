package ds.gae.servlets;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ds.gae.CarRentalModel;
import ds.gae.ReservationException;
import ds.gae.entities.Quote;
import ds.gae.entities.ReservationConstraints;
import ds.gae.view.JSPSite;
import ds.gae.view.ViewTools;

public class PersistTestServlet extends HttpServlet {
	private static final long serialVersionUID = -4694162076388862047L;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {

		String companyName = "Hertz";
		String userName = "Pieter A.";
		
		req.getSession().setAttribute("renter", userName);
		try {
			boolean fullApplicationDeployed = new File(getServletContext().getRealPath(JSPSite.CREATE_QUOTES.url())).exists();
			
			if (CarRentalModel.get().getReservations(userName).size() == 0 && !fullApplicationDeployed) {

				ReservationConstraints c = new ReservationConstraints(
						ViewTools.DATE_FORMAT.parse("01.02.2011"), 
						ViewTools.DATE_FORMAT.parse("01.03.2011"), "Compact");
				
				final Quote q = CarRentalModel.get().createQuote(companyName, userName, c);
				CarRentalModel.get().confirmQuote(q);
			}
			
			resp.sendRedirect(JSPSite.PERSIST_TEST.url());
		} catch (ParseException e) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ViewTools.stacktraceToHTMLString(e));
		} catch (ReservationException e) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ViewTools.stacktraceToHTMLString(e));			
		}
	}
}
