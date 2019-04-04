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

package org.etexascode.interrep.datamodel.interfaces;

import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;

/**
 * Interface for merging the mutable and immutable models.
 * 
 * @author jrutherford
 * @author ablatt
 */
public interface IVehicle extends IDistanceable, IDable {

    /**
     * Gets the vehicle ID. (simulator assigned ID)
     * 
     * @return The vehicle ID.
     */
    public int getVehicleID();

    /**
     * Gets the global vehicle ID.
     * 
     * @return The global vehicle ID.
     */
    public long getGlobalId();

    /**
     * Gets the simulation ID.
     * 
     * @return The simulation ID.
     */
    public Long getSimulationId();

    /**
     * Gets the speed.
     * 
     * @return The speed.
     */
    public double getSpeed();

    /**
     * Gets the acceleration.
     * 
     * @return The acceleration.
     */
    public double getAcceleration();

    /**
     * Gets the length.
     * 
     * @return The length.
     */
    public double getLength();

    /**
     * Gets the width.
     * 
     * @return The width.
     */
    public double getWidth();

    /**
     * Gets the lane ID.
     * 
     * @return The lane ID.
     */
    public int getLaneID();

    /**
     * Gets the J2735 spec heading, 0d north, positive degrees clockwise.
     * 
     * @return The heading.
     */
    public double getHeading();

    /**
     * Gets the height of the vehicle.
     * 
     * @return The height.
     */
    public double getHeight();

    /**
     * Gets the type of vehicle.
     * 
     * @return The type of vehicle.
     */
    public VEHICLE_TYPE getType();

    /**
     * Gets if the brake is pressed.
     * 
     * @return True/False.
     */
    public boolean isBrakePressed();

    /**
     * Gets the latitude of the Vehicle.
     * 
     * @return The latitude of the vehicle.
     */
    public double getLatitude();

    /**
     * Gets the longitude of the Vehicle.
     * 
     * @return The longitude of the vehicle.
     */
    public double getLongitude();

    /**
     * Gets the elevation of the Vehicle.
     * 
     * @return The elevation of the vehicle.
     */
    public double getElev();

    /**
     * Returns the steering wheel angle (in degrees).
     * 
     * @return the steering wheel angle (in degrees)
     */
    public double getSteeringAngle();
}