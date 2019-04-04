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
package org.etexascode.webapp.rest.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * The model for a vehicle.
 * 
 * @author ttevendale
 */
public class Vehicle implements Serializable {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = -4198737767895828552L;

    /** The vehicle ID. */
    @XmlElement
    private final long vehicleID;

    /** The X coordinate. */
    @XmlElement
    protected final double x;

    /** The Y coordinate. */
    @XmlElement
    protected final double y;

    /** The Z coordinate. */
    @XmlElement
    protected final double z;

    /** The speed. */
    @XmlElement
    private final double speed;

    /** The length. */
    @XmlElement
    private final double length;

    /** The acceleration. */
    @XmlElement
    private final double acceleration;

    /** The width. */
    @XmlElement
    private final double width;

    /** The lane ID. */
    @XmlElement
    private final int laneID;

    /**
     * The heading which is 0 less than or equal to heading less than 359.. Heading is measured
     * clockwise from north.
     */
    @XmlElement
    private final double heading;

    /**
     * TODO - ttevendale - Find out what the height of a vehicle is. I assumed that it was the
     * vehicle's highest point aka roof, but the following comment from old code makes me uncertain:
     * (Bumper height of the vehicle. I believe the units are inches.)
     */
    @XmlElement
    private final double height;

    /** The type of vehicle. */
    @XmlElement
    private final VEHICLE_TYPE type;

    /** If the brake is pressed. */
    @XmlElement
    private final boolean brakePressed;

    /** The latitude of the vehicle. */
    @XmlElement
    private final double latitude;

    /** The longitude of the vehicle. */
    @XmlElement
    private final double longitude;

    /** The elevation of the vehicle */
    @XmlElement
    private final double elev;

    /** The steering wheel angle (degrees). */
    @XmlElement
    private final double steeringAngle;

    @XmlElement
    private final List<OnBoardDevice> devices;

    /**
     * Constructor for a vehicle.
     * 
     * @param vehicleID The vehicle ID associated with this vehicle.
     * @param x The X coordinate of this vehicle.
     * @param y The Y coordinate of this vehicle.
     * @param z The Z coordinate of this vehicle.
     * @param speed The speed that this vehicle is going.
     * @param length The length of this vehicle.
     * @param acceleration The acceleration of this vehicle.
     * @param width The width of this vehicle.
     * @param laneID The lane ID that this vehicle is attached to.
     * @param heading The heading of this vehicle.
     * @param height The height of this vehicle.
     * @param type The type of vehicle (Car, Bus, etc...).
     * @param brakePressed The boolean for if the brake is pressed in this vehicle.
     * @param latitude The latitude of this vehicle.
     * @param longitude The longitude of this vehicle.
     * @param elev The elevation of this vehicle.
     * @param steeringAngle The angle of this vehicle's steering wheel.
     * @param devices The devices that are on this vehicle.
     */
    public Vehicle(long vehicleID, double x, double y, double z, double speed, double length, double acceleration, double width, int laneID, double heading, double height, VEHICLE_TYPE type,
            boolean brakePressed, double latitude, double longitude, double elev, double steeringAngle, List<OnBoardDevice> devices) {
        this.vehicleID = vehicleID;
        this.x = x;
        this.y = y;
        this.z = z;
        this.speed = speed;
        this.length = length;
        this.acceleration = acceleration;
        this.width = width;
        this.laneID = laneID;
        this.heading = heading;
        this.height = height;
        this.type = type;
        this.brakePressed = brakePressed;
        this.latitude = latitude;
        this.longitude = longitude;
        this.elev = elev;
        this.steeringAngle = steeringAngle;
        this.devices = devices;
    }

    /**
     * Constructor for a vehicle from a IVehicle.
     * 
     * @param iVehicle The interrep vehicle to create this vehicle from.
     * @param devices The devices that are on this vehicle.
     */
    public Vehicle(IVehicle iVehicle, List<OnBoardDevice> devices) {
        this(iVehicle.getGlobalId(), iVehicle.getX(), iVehicle.getY(), iVehicle.getZ(), iVehicle.getSpeed(), iVehicle.getLength(),
                iVehicle.getAcceleration(), iVehicle.getWidth(), iVehicle.getLaneID(), iVehicle.getHeading(), iVehicle.getHeight(), iVehicle.getType(), iVehicle.isBrakePressed(),
                iVehicle.getLatitude(), iVehicle.getLongitude(), iVehicle.getElev(), iVehicle.getSteeringAngle(), devices);
    }

    /**
     * Gets the vehicle ID.
     * 
     * @return The vehicle ID.
     */
    public long getVehicleID() {

        return vehicleID;
    }

    /**
     * Gets the X coordinate of the vehicle.
     * 
     * @return The X coordinate of the vehicle.
     */
    public double getX() {

        return x;
    }

    /**
     * Gets the Y coordinate of the vehicle.
     * 
     * @return The Y coordinate of the vehicle.
     */
    public double getY() {

        return y;
    }

    /**
     * Gets the Z coordinate of the vehicle.
     * 
     * @return The Z coordinate of the vehicle.
     */
    public double getZ() {

        return z;
    }

    /**
     * Gets the speed of the vehicle.
     * 
     * @return The speed of the vehicle.
     */
    public double getSpeed() {

        return speed;
    }

    /**
     * Gets the length of the vehicle.
     * 
     * @return The length of the vehicle.
     */
    public double getLength() {

        return length;
    }

    /**
     * Gets the acceleration of the vehicle.
     * 
     * @return The acceleration of the vehicle.
     */
    public double getAcceleration() {

        return acceleration;
    }

    /**
     * Gets the width of the vehicle.
     * 
     * @return The width of the vehicle.
     */
    public double getWidth() {

        return width;
    }

    /**
     * Gets the lane ID that the vehicle is attached to.
     * 
     * @return The lane ID that the vehicle is attached to.
     */
    public int getLaneID() {

        return laneID;
    }

    /**
     * Gets the heading of the vehicle.
     * 
     * @return The heading of the vehicle.
     */
    public double getHeading() {

        return heading;
    }

    /**
     * Gets the height of the vehicle.
     * 
     * @return The height of the vehicle.
     */
    public double getHeight() {

        return height;
    }

    /**
     * Gets the vehicle type.
     * 
     * @return The vehicle type.
     */
    public VEHICLE_TYPE getType() {

        return type;
    }

    /**
     * Checks if the brake is pressed.
     * 
     * @return True if the brake is pressed, false otherwise.
     */
    public boolean isBrakePressed() {

        return brakePressed;
    }

    /**
     * Gets the latitude of the vehicle.
     * 
     * @return The latitude of the vehicle.
     */
    public double getLatitude() {

        return latitude;
    }

    /**
     * Gets the longitude of the vehicle.
     * 
     * @return The longitude of the vehicle.
     */
    public double getLongitude() {

        return longitude;
    }

    /**
     * Gets the elevation of the vehicle.
     * 
     * @return The elevation of the vehicle.
     */
    public double getElev() {

        return elev;
    }

    /**
     * Gets the steering angle of the vehicle.
     * 
     * @return The steering angle of the vehicle.
     */
    public double getSteeringAngle() {

        return steeringAngle;
    }

    /**
     * Gets the devices on the vehicle.
     * 
     * @return The devices on the vehicle.
     */
    public List<OnBoardDevice> getDevices() {

        return devices;
    }
}