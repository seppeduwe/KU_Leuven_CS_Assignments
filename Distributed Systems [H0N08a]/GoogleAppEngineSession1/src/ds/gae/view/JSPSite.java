package ds.gae.view;

import java.util.Vector;

/**
 * A value of this Enum represents a JSP site and should be used to communicate
 * JSP site's Id between JSP sites and servlets.  
 * 
 * @author fatih
 */
public enum JSPSite {
	CREATE_QUOTES("Book", "/main.jsp", true),
	RESERVATIONS("View Bookings", "/bookings.jsp", true),
	
	RESERVATION_ERROR("Reservation Error", "/reservationError.jsp", false),
	LOGIN("Login", "/login.jsp", false),
	CONFIRM_QUOTES_RESPONSE("Confirm Quote Reply", "/confirmQuotesReply.jsp", false),	
	PERSIST_TEST_SERVLET("Persist Test", "/persTest", true),	
	PERSIST_TEST("Persist Test", "/persTest.jsp", false);	

	/**
	 * This is a human readable string describing the JSP site. It may be used
	 * to link to the JSP site.
	 */
	private String label;
	
	/**
	 * An absolute URL that this JSP can be reached at
	 */
	private String url;
	
	/**
	 * Should this site be directly callable from the client?
	 * (If yes, listing it at the navigation bar may be an option) 
	 */
	private boolean isPubLinked;
	
	JSPSite(String label, String url, boolean isPubLinked) {		
		this.label = label;
		this.url = url;
		this.isPubLinked = isPubLinked;
	}

	public String label() { return this.label; }
	public String url() { return this.url; }
	public String filename() { return new java.io.File(url).getName(); }
		
	public static JSPSite[] publiclyLinkedValues() {
		return publiclyLinkedValues(false);
	}
	
	public static JSPSite[] publiclyLinkedValues(boolean onlyPersTest) {
		if (onlyPersTest) {
			return new JSPSite[] { PERSIST_TEST_SERVLET };
		}
		
		Vector<JSPSite> out = new Vector<JSPSite>();
		for (JSPSite j : JSPSite.values()) {
			if (j.isPubLinked)
				out.add(j);
		}
		
		return out.toArray(new JSPSite[out.size()]);
	}
}
