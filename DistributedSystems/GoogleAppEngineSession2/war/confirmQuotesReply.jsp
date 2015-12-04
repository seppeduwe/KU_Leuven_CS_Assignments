<%@page import="ds.gae.view.JSPSite"%>
<%@page import="ds.gae.CarRentalModel"%>
<%@page import="ds.gae.entities.FailedQuote"%>
<%@page import="ds.gae.entities.Reservation"%>
<%@page import="java.util.List"%>

<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%
	String renter = (String)session.getAttribute("renter");
	JSPSite currentSite = JSPSite.CONFIRM_QUOTES_RESPONSE;
%>

<%@include file="_header.jsp"%>

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
				<%
					} else {
				%>
				<li><a href="<%=site.url()%>"><%=site.label()%></a></li>
				<%
					}}
				%>

			</ul>
		</div>
		<div id="contentWrapper">
			<%
				if (currentSite != JSPSite.LOGIN) {
			%>
			<div id="userProfile">
				<span>Logged-in as <%=renter%> (<a href="/login.jsp">change</a>)
				</span>
			</div>
			<%
				}
			%>
			<div class="frameDiv" style="margin: 150px 0px;">

			<H2>Reservations</H2>
			<div class="groupLabel">Failed Quotes</div>
			<div class="group">
				<table>
					<tr>
						<th>StartDate</th>
						<th>EndDate</th>
						<th>RentalCompany</th>
						<th>CarType</th>
						<th>Time</th>
					</tr>
					<%	List<FailedQuote> failedQuotes = CarRentalModel.get().getFailedQuoteByRenter(renter);			
									if ( failedQuotes != null && failedQuotes.size() > 0) {			
										for (FailedQuote failedQuote : failedQuotes) {
					%>
					<tr>
						<td><%=failedQuote.getStartDate()%></td>
						<td><%=failedQuote.getEndDate()%></td>
						<td><%=failedQuote.getRentalCompany()%></td>
						<td><%=failedQuote.getCarType()%></td>
						<td><%=failedQuote.getDate()%></td>
					</tr>
					<%
										} 
									} else {
					%>
					<tr>
						<td colspan="6">No failed quotes</td>
					</tr>
					<%
						}
					%>
				</table>
			</div>
			<div class="groupLabel">Reservations</div>
			<div class="group">			
				<table>
					<tr>
						<th>StartDate</th>
						<th>EndDate</th>
						<th>RentalCompany</th>
						<th>CarType</th>
						<th>CarId</th>
						<th>RentalPrice</th>
					</tr>
					<%	List<Reservation> reservations = CarRentalModel.get().getReservations(renter);
							if ( reservations != null && reservations.size() > 0) {
								for (Reservation reservation : reservations) {
					%>	
					<tr>
						<td><%=reservation.getStartDate()%></td>
						<td><%=reservation.getEndDate()%></td>
						<td><%=reservation.getRentalCompany()%></td>
						<td><%=reservation.getCarType()%></td>
						<td><%=reservation.getCarId()%></td>
						<td><%=reservation.getRentalPrice()%></td>
					</tr>
					<%
									} 
								} else {
					%>
					<tr>
						<td colspan="6">No reservations</td>
					</tr>
					<%
						}
					%>
				</table>
			</div>
			</div>
			<%@include file="_footer.jsp"%>