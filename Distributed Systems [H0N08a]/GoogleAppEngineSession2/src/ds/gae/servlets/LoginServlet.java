package ds.gae.servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import ds.gae.view.JSPSite;

@SuppressWarnings("serial")
public class LoginServlet extends HttpServlet {

	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		String username = req.getParameter("username");
		if (username == null || username.length() == 0) {			
			resp.sendRedirect(JSPSite.LOGIN.url());
		} else {
			req.getSession().setAttribute("renter", username);
			JSPSite caller = (JSPSite) req.getSession().getAttribute("lastSiteCall");
			if (caller == null) {
				resp.sendRedirect(JSPSite.CREATE_QUOTES.url());
			} else {
				resp.sendRedirect(caller.url());
			}
		}
		
		
	}
}
