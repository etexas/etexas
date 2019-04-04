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
package com.harmonia.etexas;

import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * Basic implementation of a vehicle(currently for use in testing native BSM).
 *
 * @author ttevendale
 */
public class SimpleVehicleImpl implements IVehicle {

    /**
     * The vehicle id.
     */
    int vehicleId;

    /**
     * The latitude.
     */
    double latitude;

    /**
     * The longitude.
     */
    double longitude;

    /**
     * The speed.
     */
    double speed;

    /**
     * The heading.
     */
    double heading;

    /**
     * The acceleration.
     */
    double acceleration;

    /**
     * The width.
     */
    double width;

    /**
     * The length.
     */
    double length;

    /**
     * The steering wheel angle (in degrees).
     */
    double steeringAngle;

    /**
     * Instantiates a new simple vehicle implementation.
     *
     * @param vehicleId the vehicle id
     * @param latitude the latitude
     * @param longitude the longitude
     * @param speed the speed
     * @param heading the heading
     * @param acceleration the acceleration
     * @param width the width
     * @param length the length
     */
    public SimpleVehicleImpl(int vehicleId, double latitude, double longitude, double speed, double heading, double acceleration, double width, double length) {
        this.vehicleId = vehicleId;
        this.latitude = latitude;
        this.longitude = longitude;
        this.speed = speed;
        this.heading = heading;
        this.acceleration = acceleration;
        this.width = width;
        this.length = length;
    }

    /**
     * Gets the x axis of the vehicle
     * 
     * @return x the x axis of the vehicle
     */
    @Override
    public double getX() {
        return 0;
    }

    /**
     * Gets the y axis of the vehicle
     * 
     * @return y the y axis of the vehicle
     */
    @Override
    public double getY() {
        return 0;
    }

    /**
     * Gets the z axis of the vehicle
     * 
     * @return z the z axis of the vehicle
     */
    @Override
    public double getZ() {
        return 0;
    }

    /**
     * Checks if the IDs equal
     */
    @Override
    public boolean equalsId(IDable entity) {
        return false;
    }

    /**
     * Gets the proper ID of the vehicle
     */
    @Override
    public String getProperId() {
        return null;
    }

    /**
     * Gets the simulation ID of the vehicle
     */
    @Override
    public Long getSimulationId() {
        return null;
    }

    /**
     * Gets the ID of the vehicle
     * 
     * @return vehicleId the ID of the vehicle
     */
    @Override
    public int getVehicleID() {
        return vehicleId;
    }

    /**
     * Gets the speed of the vehicle
     * 
     * @return speed the speed of the vehicle
     */
    @Override
    public double getSpeed() {
        return speed;
    }

    /**
     * Gets the acceleration of the vehicle
     * 
     * @return acceleration the acceleration of the vehicle
     */
    @Override
    public double getAcceleration() {
        return acceleration;
    }

    /**
     * Gets the length of the vehicle
     * 
     * @return length the longitude of the vehicle
     */
    @Override
    public double getLength() {
        return length;
    }

    /**
     * Gets the width of the vehicle
     * 
     * @return width the widht of the vehicle
     */
    @Override
    public double getWidth() {
        return width;
    }

    /**
     * Gets the lane ID where the vehicle is located
     */
    @Override
    public int getLaneID() {
        return 0;
    }

    /**
     * Gets the heading of the vehicle
     * 
     * @return heading the heading of the vehicle
     */
    @Override
    public double getHeading() {
        return heading;
    }

    /**
     * Gets the height of the vehicle
     */
    @Override
    public double getHeight() {
        return 0;
    }

    /**
     * Gets the vehicle_type of the vehicle
     */
    @Override
    public VEHICLE_TYPE getType() {
        return null;
    }

    /**
     * Gets the status of the brake being pressed in the vehicle
     */
    @Override
    public boolean isBrakePressed() {
        return false;
    }

    /**
     * Gets the Latitude of the vehicle
     * 
     * @return latitude the latitude of the vehicle
     */
    @Override
    public double getLatitude() {
        return latitude;
    }

    /**
     * Gets the Longitude of the vehicle
     * 
     * @return longitude the longitude of the vehicle
     */
    @Override
    public double getLongitude() {
        return longitude;
    }

    /**
     * Gets the elevation of the vehicle
     */
    @Override
    public double getElev() {
        return 0;
    }

    /**
     * Returns the steering wheel angle (in degrees).
     * 
     * @return the steering wheel angle (in degrees)
     */
    @Override
    public double getSteeringAngle() {

        return steeringAngle;
    }

    @Override
    public long getGlobalId() {

        return 0;
    }
}