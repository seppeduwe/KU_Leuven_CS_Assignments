package ds.gae;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class Worker extends HttpServlet {
	private static final long serialVersionUID = -7058685883212377590L;
	
	// default handler method is POST
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		super.doPost(req, resp);	
		// Nothing here -> QuoteConfirmDeferredTask
	}
}
