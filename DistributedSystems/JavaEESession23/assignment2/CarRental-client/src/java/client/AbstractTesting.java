package client;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Small scriptable testing system. Reads a scenario from file and executes it.
 * 
 * A line in this file is either a comment (starts with #) or has this syntax:
 * <clientName> <command+flags> <p1> <p2> <p3> <p..>
 * 
 * The command+flags parameter contains upper case characters to set a command and lower
 * case parameters to set a flag. 
 * 
 * This file only provides the framework for a scripted testing. Subclasses are expected
 * to implement individual commands in the the processLine method.
 */
public abstract class AbstractTesting {
	protected static final DateFormat DATE_FORMAT = new SimpleDateFormat("d/M/y");
	protected final String scriptFile;		//name of the file containing the test script

	public AbstractTesting(String scriptFile) {		
		this.scriptFile = scriptFile;
	}

	/**
	 * This is the main method that is supposed to be run by the client. For simplicity, many 
	 * exceptions are not handled and thrown to the client.
	 *  
	 * @throws Exception
	 */
	final public void run() throws Exception {
		//read script		
		InputStream is = AbstractTesting.class.getClassLoader().getResourceAsStream(scriptFile);
		BufferedReader in = null;
		if (is != null) {	// scriptFile found inside a jar
			in = new BufferedReader(new InputStreamReader(is));
		} else { 
			in = new BufferedReader(new FileReader(scriptFile));
		}
		
		int currentLine = 0;

		while (in.ready()) {
			currentLine++;
			
			//read line
			String line = in.readLine();
			//tokenize
			StringTokenizer scriptLineTokens = new StringTokenizer(line, " ");
			String name = scriptLineTokens.nextToken();
			String cmdAndFlags = scriptLineTokens.nextToken();
			
			StringBuffer cmdTmp = new StringBuffer();
			List<Character> flags = new ArrayList<Character>();	
			
			for (int i=0; i < cmdAndFlags.length(); i++) {
				char test = cmdAndFlags.charAt(i);
				if (Character.isLowerCase(test)) {
					flags.add(test);
				} else {
					cmdTmp.append(test);
				}
			}			
			
			ApplicationException appException = null;
			try {
				processLine(name, cmdTmp.toString(), flags, scriptLineTokens);
			} catch(ApplicationException ae) {
				appException = ae;
			} catch(IllegalArgumentException iae) {
				System.err.println(String.format("Exception caused by script line %d (%s): %s", currentLine, line, iae.getMessage()));
				throw iae;
			}
			
			boolean shouldfail = flags.contains('c');
			if(appException == null && shouldfail) {
				System.err.println("command should have failed: " + line +" on line " + currentLine );
			} else if(appException != null && !shouldfail) {
				System.err.println("command failed: " + line +" on line " + currentLine );
				appException.caughtException.printStackTrace();
			}

		}
			
	}
	
	/**
	 * This method ought be implemented by a concrete scripting test class. 
	 * 
	 * @param name Name of the client/renter
	 * @param cmd  Command
	 * @param flags List of flags 
	 * @param scriptLineTokens The remaining script line as a StringTokenizer
	 * 
	 * @throws ApplicationException Any exception thrown by the application-under-test is 
	 *  should be wrapped into this exception. These exceptions are accounted for when 
	 *  checking for "expected exception" flag in the script
	 * @throws IllegalArgumentException This exception must be thrown to indicate that 
	 *	the script line is unexpected. It will cancel further processing of the script.  
	 * 
	 */
	protected abstract void processLine(String name, String cmd, List<Character> flags, StringTokenizer scriptLineTokens) throws ApplicationException;
	
	public static class ApplicationException extends Exception {
		private static final long serialVersionUID = -6918435292488413185L;
		public Exception caughtException;
		public ApplicationException(Exception caughtException) {
			this.caughtException = caughtException;
		}
	}

	/**
	 * Utility function to join a list of Strings to a delimiter-separated String
	 * 
	 * @param in	List of strings
	 * @param delim Separator
	 * @return Joined String
	 */
    protected final static String joinToString(Iterable<String> in, char delim) {
        StringBuilder out = new StringBuilder();
        for (String s : in) {
            if (out.length() > 0) {
                out.append(delim);
            }
            out.append(s);
        }
        return out.toString();
    }

	
}
