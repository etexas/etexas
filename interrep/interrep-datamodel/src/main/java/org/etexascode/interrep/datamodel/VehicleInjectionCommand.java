/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel;

import java.io.Serializable;

import org.etexascode.CoberturaIgnore;

/*
 * Vehicle Injection Command holds abbreviated info from the VehicleInjectionRequest for the UI
 * @author bmauldon
 */
public class VehicleInjectionCommand extends Command implements Serializable {

    /**
     * The simulation time to inject the vehicle, in seconds.
     */
    private double injectionTime;

    /**
     * the vehicle id
     */

    private int vehicleID;

    /**
     * the lane id
     */
    private int laneID;

    /**
     * injection speed
     */
    private double speed;

    /**
     * Constructor
     * 
     * @param vir The VehicleInjectionRequest.
     */
    @CoberturaIgnore
    public VehicleInjectionCommand(VehicleInjectionRequest vir) {
        this.vehicleID = vir.getVehicle().getVehicleID();
        this.laneID = vir.getVehicle().getLaneID();
        this.injectionTime = vir.getInjectionTime();
        this.speed = vir.getVehicle().getSpeed();

    }

    /**
     * Get injection time
     * 
     * @return injectionTime
     */
    public double getInjectionTime() {
        return injectionTime;
    }

    /**
     * Set injection time
     * 
     * @param injectionTime The injection time to set.
     */
    public void setInjectionTime(double injectionTime) {
        this.injectionTime = injectionTime;
    }

    /**
     * Get vehicle id
     * 
     * @return vehicleID The vehicle ID.
     */
    public int getVehicleID() {
        return vehicleID;
    }

    /**
     * Set vehicleID
     * 
     * @param vehicleID The vehicle ID to set.
     */
    public void setVehicleID(int vehicleID) {
        this.vehicleID = vehicleID;
    }

    /**
     * Get lane id
     * 
     * @return laneID The lane ID.
     */
    public int getLaneID() {
        return laneID;
    }

    /**
     * Set lane id
     * 
     * @param laneID The lane ID to set.
     */
    public void setLaneID(int laneID) {
        this.laneID = laneID;
    }

    /**
     * Get injection speed of vehicle
     * 
     * @return speed as double
     */
    public double getSpeed() {
        return speed;
    }

    /**
     * Set injection speed of vehicle
     * 
     * @param speed The speed to set.
     */
    public void setSpeed(double speed) {
        this.speed = speed;
    }

    @CoberturaIgnore
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        long temp;
        temp = Double.doubleToLongBits(injectionTime);
        result = prime * result + (int)(temp ^ (temp >>> 32));
        result = prime * result + laneID;
        temp = Double.doubleToLongBits(speed);
        result = prime * result + (int)(temp ^ (temp >>> 32));
        result = prime * result + vehicleID;
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
        VehicleInjectionCommand other = (VehicleInjectionCommand)obj;
        if (Double.doubleToLongBits(injectionTime) != Double.doubleToLongBits(other.injectionTime))
            return false;
        if (laneID != other.laneID)
            return false;
        if (Double.doubleToLongBits(speed) != Double.doubleToLongBits(other.speed))
            return false;
        if (vehicleID != other.vehicleID)
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("VehicleInjectionCommand [injectionTime=");
        builder.append(injectionTime);
        builder.append(", vehicleID=");
        builder.append(vehicleID);
        builder.append(", laneID=");
        builder.append(laneID);
        builder.append(", speed=");
        builder.append(speed);
        builder.append("]");
        return builder.toString();
    }

}
