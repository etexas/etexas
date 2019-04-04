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

package org.etexascode.apps.dcs.model;

/**
 * A data structure used to hold information pertaining to the dilemma zone for a vehicle.
 * 
 * @author jrutherford
 */
public class VehicleDilemmaZoneData {

    /** The duration (in seconds) of the dilemma zone. */
    private static final double DURATION = 3.0;

    /**
     * The unique identifier for the lane in which the vehicle represented by this data resides.
     */
    private int laneId;

    /**
     * The velocity (in feet per second) at which the vehicle represented by this data is
     * travelling.
     */
    private double vehicleSpeed;

    /**
     * The time (in seconds) at which the vehicle represented by this data will enter the dilemma
     * zone.
     */
    private double timeOfArrivalToDilemmaZone;

    /** The length (in feet) of the vehicle represented by this data. */
    private double vehicleLength;

    /**
     * Get the unique identifier for the lane in which the vehicle represented by this data resides.
     * 
     * @return The unique identifier for the lane in which the vehicle represented by this data
     * resides.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Set the unique identifier for the lane in which the vehicle represented by this data resides.
     * 
     * @param laneId The unique identifier for the lane in which the vehicle represented by this
     * data resides.
     */
    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Get the velocity (in feet per second) at which the vehicle represented by this data is
     * travelling.
     * 
     * @return The velocity (in feet per second) at which the vehicle represented by this data is
     * travelling.
     */
    public double getVehicleSpeed() {
        return vehicleSpeed;
    }

    /**
     * Set the velocity (in feet per second) at which the vehicle represented by this data is
     * travelling.
     * 
     * @param speed The velocity (in feet per second) at which the vehicle represented by this data
     * is travelling.
     */
    public void setVehicleSpeed(double speed) {
        this.vehicleSpeed = speed;
    }

    /**
     * Get the time (in seconds) at which the vehicle represented by this data will enter the
     * dilemma zone.
     * 
     * @return The time (in seconds) at which the vehicle represented by this data will enter the
     * dilemma zone.
     */
    public double getTimeOfArrivalToDilemmaZone() {
        return timeOfArrivalToDilemmaZone;
    }

    /**
     * Set the time (in seconds) at which the vehicle represented by this data will enter the
     * dilemma zone.
     * 
     * @param timeOfArrivalToDilemmaZone The time (in seconds) at which the vehicle represented by
     * this data will enter the dilemma zone.
     */
    public void setTimeOfArrivalToDilemmaZone(double timeOfArrivalToDilemmaZone) {
        this.timeOfArrivalToDilemmaZone = timeOfArrivalToDilemmaZone;
    }

    /**
     * Get the length (in feet) of the vehicle represented by this data.
     * 
     * @return The length (in feet) of the vehicle represented by this data.
     */
    public double getVehicleLength() {
        return vehicleLength;
    }

    /**
     * Set the length (in feet) of the vehicle represented by this data.
     * 
     * @param length The length (in feet) of the vehicle represented by this data.
     */
    public void setVehicleLength(double length) {
        this.vehicleLength = length;
    }

    /**
     * Get the time (in seconds) at which the vehicle represented by this data will exit the dilemma
     * zone.
     * 
     * @return The time (in seconds) at which the vehicle represented by this data will exit the
     * dilemma zone.
     */
    public double getTimeOfDepartureFromDilemmaZone() {
        return this.timeOfArrivalToDilemmaZone + DURATION;
    }
}