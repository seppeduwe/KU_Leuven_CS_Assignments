package ds.gae.view;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;

/**
 * This class serves as an unsorted tool collection to work with HTML as an 
 * output channel
 * 
 * @author fatih
 *
 */
public class ViewTools {
	public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy");
	
	/**
	 * Converts plain text to HTML-encoded text
	 * 
	 * @param plainText Input string
	 * @return HTML-encoded text
	 */
	public static String encodeHTML(String plainText) {
	    StringBuffer out = new StringBuffer();
	    for(int i=0; i<plainText.length(); i++) {
	        char c = plainText.charAt(i);
	        if(c > 127 || c=='"' || c=='<' || c=='>') {
	           out.append("&#"+(int)c+";");
	        }
	        else {
	            out.append(c);
	        }
	    }
	    return out.toString().replaceAll("(\r\n|\n)", "<br />");
	}
	
	/**
	 * Extracts and HTML-encodes stack trace output of a given Throwable object
	 * and returns it as a string 
	 * 
	 * @param t Throwable object to extract stack trace from 
	 * @return HTML-encoded string
	 */
	public static String stacktraceToHTMLString(Throwable t) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		t.printStackTrace(pw);
		return ViewTools.encodeHTML(sw.toString());			

	}
}
