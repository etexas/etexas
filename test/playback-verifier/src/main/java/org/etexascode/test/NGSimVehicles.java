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
package org.etexascode.test;

import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * Represents a vehicle from the NGSIM data.
 * 
 * @author janway
 */
public class NGSimVehicles {

    /**
     * The ID of the vehicle.
     */
    private int id;

    /**
     * The x position as an offset.
     */
    private double x;

    /**
     * The y position as an offset.
     */
    private double y;

    /**
     * The speed.
     */
    private double speed;

    /**
     * The length.
     */
    private double length;

    /**
     * The width.
     */
    private double width;

    /**
     * The laneId, already converted to match our map data.
     */
    private int laneId;

    /**
     * Constructor.
     * 
     * @param id Vehicle ID.
     * @param x Vehicle x position.
     * @param y Vehicle y position.
     * @param speed Vehicle speed.
     * @param length Vehicle length.
     * @param width Vehicle width.
     * @param laneId Vehicle lane.
     */
    public NGSimVehicles(int id, double x, double y, double speed, double length, double width, int laneId) {
        this.id = id;
        this.x = x;
        this.y = y;
        this.speed = speed;
        this.length = length;
        this.width = width;
        this.laneId = laneId;
    }

    /**
     * Getter
     * 
     * @return The ID.
     */
    public int getId() {
        return id;
    }

    /**
     * Setter
     * 
     * @param id The ID to set.
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Getter
     * 
     * @return The X coordinate.
     */
    public double getX() {
        return x;
    }

    /**
     * Setter
     * 
     * @param x The x coordinate to set.
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Getter
     * 
     * @return The Y coordinate.
     */
    public double getY() {
        return y;
    }

    /**
     * Setter
     * 
     * @param y The y coordinate to set.
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Getter
     * 
     * @return The speed.
     */
    public double getSpeed() {
        return speed;
    }

    /**
     * Setter
     * 
     * @param speed The speed to set.
     */
    public void setSpeed(double speed) {
        this.speed = speed;
    }

    /**
     * Getter
     * 
     * @return The length.
     */
    public double getLength() {
        return length;
    }

    /**
     * Setter
     * 
     * @param length The length to set.
     */
    public void setLength(double length) {
        this.length = length;
    }

    /**
     * Getter
     * 
     * @return The width.
     */
    public double getWidth() {
        return width;
    }

    /**
     * Setter
     * 
     * @param width The width to set.
     */
    public void setWidth(double width) {
        this.width = width;
    }

    /**
     * Getter
     * 
     * @return The lane ID.
     */
    public int getLaneId() {
        return laneId;
    }

    /**
     * Setter
     * 
     * @param laneId The lane ID to set.
     */
    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Equals method to compare an NGSIM vehicle entry to an InterRep vehicle.
     * 
     * @param v The InterRep vehicle to compare against.
     * @return True if the vehicles match, false otherwise.
     */
    public boolean matches(Vehicle v) {
        boolean ret = true;

        final double TOLERANCE = 0.0005;

        if (this.id != v.getVehicleID())
            ret = false;
        if (this.laneId != v.getLaneID() && this.laneId != -1)
            ret = false;
        if (Math.abs(UtilsUnitConversion.convertFeetToCentimeters(this.x) - v.getX()) > TOLERANCE)
            ret = false;
        if (Math.abs(UtilsUnitConversion.convertFeetToCentimeters(this.y) - v.getY()) > TOLERANCE)
            ret = false;
        if (Math.abs(UtilsUnitConversion.convertMilesPerHourToMetersPerSecond(this.speed) - v.getSpeed()) > TOLERANCE)
            ret = false;
        if (Math.abs(UtilsUnitConversion.convertFeetToCentimeters(this.length) - v.getLength()) > TOLERANCE)
            ret = false;
        if (Math.abs(UtilsUnitConversion.convertFeetToCentimeters(this.width) - v.getWidth()) > TOLERANCE)
            ret = false;

        return ret;
    }

    /**
     * Prints the fields for debugging.
     * 
     * @return The string version of the object.
     */
    public String toString() {
        String ret = String.format("id: %d, laneId: %d, x: %f, y: %f, length: %f, width: %f, speed: %f", id, laneId, x, y, length, width, speed);

        return ret;
    }

}
