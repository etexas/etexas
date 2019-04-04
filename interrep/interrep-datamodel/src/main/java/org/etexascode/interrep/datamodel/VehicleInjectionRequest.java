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

/**
 * Holds information for the simulator to inject a single vehicle.
 * 
 * @author janway
 */
public class VehicleInjectionRequest extends Command implements Serializable {

    /**
     * Serial ID.
     */
    private static final long serialVersionUID = -4208890019169428668L;

    /**
     * The vehicle to inject.
     */
    private Vehicle vehicle;

    /**
     * The simulation time to inject the vehicle, in seconds.
     */
    private double injectionTime;

    /**
     * The intersection to receive the vehicle.
     */
    private int intersectionId;

    /**
     * Constructor.
     */
    public VehicleInjectionRequest() {

    }

    /**
     * Constructor.
     * 
     * @param vehicle The vehicle to inject.
     * @param injectionTime The simulation time to inject the vehicle, in seconds.
     * @param intersectionId The intersection to receive the vehicle.
     */
    public VehicleInjectionRequest(Vehicle vehicle, double injectionTime, int intersectionId) {
        this.vehicle = vehicle;
        this.injectionTime = injectionTime;
        this.intersectionId = intersectionId;
    }

    @Override
    public String getDescription() {

        return String.format("Request that a new vehicle be injected on lane %d travelling at %.2f m/s", vehicle.getLaneID(), vehicle.getSpeed());
    }

    /**
     * Getter.
     * 
     * @return the vehicle
     */
    public Vehicle getVehicle() {
        return vehicle;
    }

    /**
     * Setter.
     * 
     * @param vehicle the vehicle to set
     */
    public void setVehicle(Vehicle vehicle) {
        this.vehicle = vehicle;
    }

    /**
     * Getter.
     * 
     * @return the injectionTime
     */
    public double getInjectionTime() {
        return injectionTime;
    }

    /**
     * Setter.
     * 
     * @param injectionTime the injectionTime to set
     */
    public void setInjectionTime(double injectionTime) {
        this.injectionTime = injectionTime;
    }

    /**
     * Getter.
     * 
     * @return the intersectionId
     */
    public int getIntersectionId() {
        return intersectionId;
    }

    /**
     * Setter.
     * 
     * @param intersectionId the intersectionId to set
     */
    public void setIntersectionId(int intersectionId) {
        this.intersectionId = intersectionId;
    }

    @CoberturaIgnore
    @Override
    public String toString() {
        StringBuilder add = new StringBuilder();
        add.append("VehicleInjectionRequest [vehicle=");
        add.append(vehicle);
        add.append(", injectionTime=");
        add.append(injectionTime);
        add.append(", intersectionId=");
        add.append(intersectionId);
        add.append("]");
        return add.toString();

    }
}
