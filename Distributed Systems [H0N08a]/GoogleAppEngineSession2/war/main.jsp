<%@page import="java.util.HashMap"%>
<%@page import="java.util.List"%>
<%@page import="ds.gae.view.JSPSite"%>
<%@page import="ds.gae.view.ViewTools"%>
<%@page import="ds.gae.CarRentalModel"%>
<%@page import="ds.gae.entities.Quote"%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<% 
	JSPSite currentSite = JSPSite.CREATE_QUOTES;
	String renter = (String)session.getAttribute("renter");
	HashMap<String, List<Quote>> quotes = (HashMap<String, List<Quote>>)session.getAttribute("quotes"); 
	boolean anyQuotes = false;
%>   
 
<%@include file="_header.jsp" %>

<% 
if (currentSite != JSPSite.LOGIN && currentSite != JSPSite.PERSIST_TEST && renter == null) {
 %>
	<meta http-equiv="refresh" content="0;URL='/login.jsp'">
<% 
  request.getSession().setAttribute("lastSiteCall", currentSite);
} 
 %>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
	<link rel="stylesheet" type="text/css" href="style.css" />
	<title>Car Rental Application</title>
</head>
<body>
	<div id="mainWrapper">
		<div id="headerWrapper">
			<h1>Car Rental Application</h1>
		</div>
		<div id="navigationWrapper">
			<ul>
<% 
for (JSPSite site : JSPSite.publiclyLinkedValues()) {
	if (site == currentSite) {
 %> 
				<li><a class="selected" href="<%=site.url()%>"><%=site.label()%></a></li>
<% } else {
 %> 
				<li><a href="<%=site.url()%>"><%=site.label()%></a></li>
<% }}
 %> 

				</ul>
		</div>
		<div id="contentWrapper">
<% if (currentSite != JSPSite.LOGIN) { %>
			<div id="userProfile">
				<span>Logged-in as <%= renter %> (<a href="/login.jsp">change</a>)</span>
			</div>
<%
   }
 %>
		<div class="frameDiv">
<% 
for (String crc : CarRentalModel.get().getAllRentalCompanyNames()) {
 %> <!-- begin of CRC loop -->
			<h2>Car Rental Company: <%= crc %></h2>
			
			<h3>Create a Quote</h3>
			<form method="POST" action="/createQuote">
			<div class="group">
				<div class="form">
					<span>From: <input type="text" name="startDate" value="01.01.2012" size="10"> (dd.mm.yyyy)</span>
					<span>To: <input type="text" name="endDate" value="01.02.2012" size="10"> (dd.mm.yyyy)</span>
				</div>
				<div class="form">
					<span>
					Car Type: 
					<select name="carType"> 
	<% for (String carTypeName : CarRentalModel.get().getCarTypesNames(crc) ) { 
	 %>
						<option><%= carTypeName %></option>
	<% } 
	 %> 
					</select>
					</span>
				</div>
				<div class="formsubmit">
					<input type="hidden" name="crc" value="<%= crc %>"/>  
					<input type="submit" value="Create" />
				</div>
				</form>
			</div>
	<% if ( quotes != null && quotes.containsKey(crc) && quotes.get(crc).size() > 0) {
		List<Quote> quotesForCrc = quotes.get(crc);
		anyQuotes = true;
	 %>
			<h3>Current Quotes</h3>
			<div class="group">
				<table>
					<tr>
						<th>Car Type</th>
						<th>Start Date</th>
						<th>End Date</th>
						<th class="numbers">Rental Price</th>			
					</tr>
						
		<%
			for (Quote q : quotesForCrc) { 
		 %>
					<tr>
						<td><%= q.getCarType()%></td>
						<td><%= ViewTools.DATE_FORMAT.format(q.getStartDate())%></td>
						<td><%= ViewTools.DATE_FORMAT.format(q.getEndDate())%></td>
						<td class="numbers"><%= q.getRentalPrice()%> €</td>
					</tr>
		<%
			} 
		 %>
				</table>
			</div>


	<%} %>

<%} 
 %> <!-- end of CRC loop -->

 <% if ( anyQuotes ) {
 %>
	<h2>Confirmation of Quotes</h2>
			<div class="formsubmit">
				<form method="POST" action="/confirmQuotes">
					<input id="confirmSubmitButton" type="submit" value=" >> Confirm all Quotes<< " />
				</form>
			</div>
 <%} 
 %> 
		</div>

 
<%@include file="_footer.jsp" %>
