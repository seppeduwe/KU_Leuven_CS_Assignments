package ds.gae.entities;

import java.util.Date;

import javax.persistence.Entity;

@Entity
public class FailedQuote extends Quote {
	private Date date;

	public FailedQuote(Quote quote, Date date){
		super(quote.getCarRenter(), quote.getStartDate(), quote.getEndDate(), 
				quote.getRentalCompany(), quote.getCarType(), quote.getRentalPrice());
		this.date = date;	
	}

	public Date getDate() {
		return date;
	}

	@Override
	public String toString() {
		return String.format("FailedQuote for %s from %s to %s at %s\nCar type: %s\t Time:%s\n", 
				getCarRenter(), getStartDate(), getEndDate(), getRentalCompany(), getCarType(), date);
	}

}
