package ds.gae.servlets;

import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

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

@SuppressWarnings("serial")
public class CreateQuoteServlet extends HttpServlet {

	@SuppressWarnings("unchecked")
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		        
        
		try {
			Date startDate = ViewTools.DATE_FORMAT.parse(req.getParameter("startDate"));
			Date endDate = ViewTools.DATE_FORMAT.parse(req.getParameter("endDate"));
			String carType = req.getParameter("carType");
			String crc = req.getParameter("crc");
			String renter = (String) req.getSession().getAttribute("renter"); 
			
			Quote q = CarRentalModel.get().createQuote(crc, renter, new ReservationConstraints(startDate, endDate, carType));
			
			HashMap<String, ArrayList<Quote>> quotes = (HashMap<String, ArrayList<Quote>>) req.getSession().getAttribute("quotes");
			if ( quotes == null ) {
				quotes = new HashMap<String, ArrayList<Quote>>();
				req.setAttribute("quotes", quotes);
			}
						
			ArrayList<Quote> quotesOfCurrentCrc;
			if ( quotes.containsKey(crc) ) {
				quotesOfCurrentCrc = quotes.get(crc);
			} else {
				quotesOfCurrentCrc = new ArrayList<Quote>();
				quotes.put(crc, quotesOfCurrentCrc);
			}
			
			quotesOfCurrentCrc.add(q);
			req.getSession().setAttribute("quotes", quotes);
			
			resp.sendRedirect(JSPSite.CREATE_QUOTES.url());	
		} catch (ParseException pe) {
			resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ViewTools.stacktraceToHTMLString(pe));			
		} catch (ReservationException re) {
			req.getSession().setAttribute("errorMsg", ViewTools.encodeHTML(re.getMessage()));
			resp.sendRedirect(JSPSite.RESERVATION_ERROR.url());
		}
		
		
	}
	
}
