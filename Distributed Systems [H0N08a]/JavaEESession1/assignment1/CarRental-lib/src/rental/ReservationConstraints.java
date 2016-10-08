package rental;

import java.io.Serializable;
import java.util.Date;

public class ReservationConstraints implements Serializable{
    
    private Date startDate;
    private Date endDate;
    private String carType;
	
    public ReservationConstraints(Date start, Date end, String carType){
    	setStartDate(start);
    	setEndDate(end);
    	setCarType(carType);
    }
    
    public Date getStartDate() {
		return startDate;
	}
    
    private void setStartDate(Date startDate) {
		this.startDate = startDate;
	}
    
    public Date getEndDate() {
		return endDate;
	}
    
	private void setEndDate(Date endDate) {
		this.endDate = endDate;
	}
	
	public String getCarType() {
		return carType;
	}
	
	private void setCarType(String carType) {
		this.carType = carType;
	}

	@Override
	public String toString() {
            return String.format("Reservation constraints [from %s until %s, for car type '%s']", 
                    getStartDate(), getEndDate(), getCarType());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((endDate == null) ? 0 : endDate.hashCode());
		result = prime * result
				+ ((startDate == null) ? 0 : startDate.hashCode());
		result = prime * result + ((carType == null) ? 0 : carType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ReservationConstraints other = (ReservationConstraints) obj;
		if (endDate == null) {
			if (other.endDate != null)
				return false;
		} else if (!endDate.equals(other.endDate))
			return false;
		if (startDate == null) {
			if (other.startDate != null)
				return false;
		} else if (!startDate.equals(other.startDate))
			return false;
		if (carType == null) {
			if (other.carType != null)
				return false;
		} else if (!carType.equals(other.carType))
			return false;
		return true;
	}
    
	
    
}
