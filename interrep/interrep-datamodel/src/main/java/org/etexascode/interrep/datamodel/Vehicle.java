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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;

/**
 * The vehicle model.
 * 
 * @author ablatt
 * @author jconnelly
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class Vehicle implements Cloneable, Serializable, IVehicle {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 4963642334339705429L;

    /**
     * TODO: bbadillo - Which of these are require or optional?
     */
    public enum VEHICLE_TYPE {

        CAR(4),
        BUS(6),
        TRACTOR_TRAILER(11);

        /** The integer value of the vehicle type. */
        private int value = 0;

        /**
         * Constructor.
         * 
         * @param value The integer value of the vehicle type.
         */
        private VEHICLE_TYPE(int value) {
            this.value = value;
        }

        /**
         * Gets the value of the vehicle type.
         * 
         * @return The integer value.
         */
        public int getValue() {
            return value;
        }
    }

    /** The vehicle ID. (simulator assigned ID) */
    @XmlElement
    private int vehicleID = 0;

    /** The vehicle's global ID. */
    @XmlElement
    private long globalId;

    /** The simulation ID this vehicle is attached to. */
    @XmlElement
    private Long simulationId = null;

    /** The X location. */
    @XmlElement
    protected double x = 0.0;

    /** The Y location. */
    @XmlElement
    protected double y = 0.0;

    /** The Z location. */
    @XmlElement
    protected double z = 0.0;

    /** The speed. */
    @XmlElement
    private Double speed;

    /** The length. */
    @XmlElement
    private double length = 0.0;

    /** The acceleration. */
    @XmlElement
    private Double acceleration;

    /** The width. */
    @XmlElement
    private double width = 0.0;

    /** The lane ID. */
    @XmlElement
    private Integer laneID;

    /**
     * The heading which is 0 less than or equal to heading less than 359.. Heading is measured
     * clockwise from north.
     */
    @XmlElement
    private Double heading = null;

    /** Bumper height of the vehicle. I believe the units are inches. */
    @XmlElement
    private double height = 0;

    /** The type of vehicle. */
    @XmlElement
    private VEHICLE_TYPE type = VEHICLE_TYPE.CAR;

    /** If the brake is pressed. */
    @XmlElement
    private boolean brakePressed;

    /** The latitude of the vehicle. */
    @XmlElement
    private double latitude;

    /** The longitude of the vehicle. */
    @XmlElement
    private double longitude;

    /** The elevation of the vehicle */
    @XmlElement
    private double elev;

    /** The steering wheel angle (degrees). */
    @XmlElement
    private double steeringAngle;

    /**
     * DO NOT DELETE Empty default constructor for serialization only. For creation of vehicles use
     * constructor with required fields.
     */
    Vehicle() {}

    /**
     * Constructor for Vehicle to have required fields.
     * 
     * @param id The vehicle id for this vehicle.
     * @param l The length of this vehicle.
     * @param w The width of this vehicle.
     * @param dx The x coordinate of this vehicle.
     * @param dy The y coordinate of this vehicle.
     * @param dz The z coordinate of this vehicle.
     */
    public Vehicle(int id, double l, double w, double dx, double dy, double dz) {

        vehicleID = id;
        length = l;
        width = w;
        x = dx;
        y = dy;
        z = dz;

        // flag that the steering angle is not available
        steeringAngle = Byte.MAX_VALUE;
    }

    /**
     * Copy constructor.
     * 
     * @param veh The vehicle to copy.
     */
    @CoberturaIgnore
    public Vehicle(IVehicle veh) {
        this.vehicleID = veh.getVehicleID();
        this.globalId = veh.getGlobalId();
        this.simulationId = veh.getSimulationId();
        this.x = veh.getX();
        this.y = veh.getY();
        this.z = veh.getZ();
        this.speed = veh.getSpeed();
        this.length = veh.getLength();
        this.width = veh.getWidth();
        this.acceleration = veh.getAcceleration();
        this.laneID = veh.getLaneID();
        this.heading = veh.getHeading();
        this.height = veh.getHeight();
        this.type = veh.getType();
        this.brakePressed = veh.isBrakePressed();
        this.latitude = veh.getLatitude();
        this.longitude = veh.getLongitude();
        this.elev = veh.getElev();
        this.steeringAngle = veh.getSteeringAngle();
    }

    /**
     * The string representation of a vehicle.
     * 
     * @return The string representation of the vehicle.
     */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("vehicleID = ");
        ret.append(vehicleID);
        ret.append("\n");

        ret.append("x = ");
        ret.append(x);
        ret.append("\n");

        ret.append("y = ");
        ret.append(y);
        ret.append("\n");

        ret.append("z = ");
        ret.append(z);
        ret.append("\n");
        if (speed != null) {
            ret.append("speed = ");
            ret.append(speed);
            ret.append("\n");
        }
        if (acceleration != null) {
            ret.append("accel = ");
            ret.append(acceleration);
            ret.append("\n");
        }

        ret.append("length = ");
        ret.append(length);
        ret.append("\n");

        ret.append("width = ");
        ret.append(width);
        ret.append("\n");

        ret.append("laneID = ");
        ret.append(laneID);
        ret.append("\n");

        ret.append("heading = ");
        ret.append(heading);
        ret.append("\n");

        ret.append("steeringAngle = ");
        ret.append(steeringAngle);
        ret.append("\n");

        UtilsStringOnModel.addDouble(ret, latitude, "latitude");
        UtilsStringOnModel.addDouble(ret, longitude, "longitude");
        UtilsStringOnModel.addDouble(ret, elev, "elev");

        return ret.toString();
    }

    /**
     * Check if two vehicles are equal.
     * 
     * @param obj The object with which to compare the vehicle.
     * @return True/False the object and vehicle are equal.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Vehicle) {
            Vehicle rhs = (Vehicle)obj;
            // TODO: ablatt - equalsbuilder appears to fail on extremely close x
            // y values... this is relevant for lat,lons
            if (Math.abs(latitude - rhs.latitude) > 0.0001) {
                return false;
            }
            if (Math.abs(longitude - rhs.longitude) > 0.0001) {
                return false;
            }
            return new EqualsBuilder().append(vehicleID, rhs.vehicleID).append(x, rhs.x).append(y, rhs.y).append(z, rhs.z).append(speed, rhs.speed).append(length, rhs.length)
                    .append(acceleration, rhs.acceleration).append(width, rhs.width).append(laneID, rhs.laneID).append(heading, rhs.heading).append(height, rhs.height).append(type, rhs.type)
                    .append(brakePressed, rhs.brakePressed).append(elev, rhs.elev).append(steeringAngle, rhs.steeringAngle).isEquals();
        }

        return false;
    }

    /**
     * HashCode operation. TODO: bbadillo - need to reconcile equals and hashcode
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(41, 43).append(vehicleID).append(x).append(y).append(z).append(speed).append(length).append(acceleration)
                .append(width).append(laneID).append(heading).append(height).append(type).append(brakePressed).append(latitude)
                .append(longitude).append(elev).append(steeringAngle).toHashCode();
    }

    /**
     * Gets the vehicle ID.
     * 
     * @return The vehicle ID.
     */
    @Override
    public int getVehicleID() {

        return vehicleID;
    }

    /**
     * Sets the vehicle ID.
     * 
     * @param vehicleID The new vehicle ID.
     */
    public void setVehicleID(int vehicleID) {

        this.vehicleID = vehicleID;
    }

    /**
     * Sets the global ID for the vehicle.
     * 
     * @param globalId The global ID to set.
     */
    public void setGlobalId(long globalId) {

        this.globalId = globalId;
    }

    @Override
    public long getGlobalId() {

        return globalId;
    }

    /**
     * Gets the ID of the simulation that this vehicle is attached to.
     * 
     * @return The simulation ID.
     */
    @Override
    public Long getSimulationId() {

        return simulationId;
    }

    /**
     * Sets the ID of the simulation that this vehicle is attached to.
     * 
     * @param simulationId The simulation ID.
     */
    public void setSimulationId(long simulationId) {

        this.simulationId = simulationId;
    }

    /**
     * Gets the X coordinate.
     * 
     * @return The X coordinate.
     */
    @Override
    public double getX() {
        return x;
    }

    /**
     * Sets the X coordinate.
     * 
     * @param x The new X coordinate.
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Gets the Y coordinate.
     * 
     * @return The Y coordinate.
     */
    @Override
    public double getY() {
        return y;
    }

    /**
     * Sets the Y coordinate.
     * 
     * @param y The new Y coordinate.
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Gets the Z coordinate.
     * 
     * @return The Z coordinate.
     */
    @Override
    public double getZ() {
        return z;
    }

    /**
     * Sets the Z coordinate.
     * 
     * @param z The new Z coordinate.
     */
    public void setZ(double z) {
        this.z = z;
    }

    /**
     * Checks to see if the Speed is null.
     * 
     * @return True/False the speed is null.
     */
    public boolean isSpeedSet() {
        return (speed == null) ? false : true;
    }

    /**
     * Gets the speed.
     * 
     * @return The speed.
     */
    @Override
    public double getSpeed() {
        return speed;
    }

    /**
     * Sets the speed.
     * 
     * @param speed The new speed.
     */
    public void setSpeed(double speed) {
        this.speed = speed;
    }

    /**
     * Checks to see if the acceleration is null.
     * 
     * @return True/False the acceleration is null.
     */
    public boolean isAccelerationSet() {
        return (acceleration == null) ? false : true;

    }

    /**
     * Gets the acceleration.
     * 
     * @return The acceleration.
     */
    @Override
    public double getAcceleration() {
        // TODO:jconnelly Should this actually be nullable, if so what usage would the null
        // acceleration generate?
        return acceleration;
    }

    /**
     * Sets the acceleration.
     * 
     * @param acceleration The new acceleration.
     */
    public void setAcceleration(double acceleration) {
        this.acceleration = acceleration;
    }

    /**
     * Gets the length.
     * 
     * @return The length.
     */
    @Override
    public double getLength() {
        return length;
    }

    /**
     * Sets the length.
     * 
     * @param length The new length.
     */
    public void setLength(double length) {
        this.length = length;
    }

    /**
     * Gets the width.
     * 
     * @return The width.
     */
    @Override
    public double getWidth() {
        return width;
    }

    /**
     * Sets the width.
     * 
     * @param width The new width.
     */
    public void setWidth(double width) {
        this.width = width;
    }

    /**
     * Checks to see if the lane ID has been set.
     * 
     * @return True/False the lane ID has been set.
     */
    public boolean isLaneIDSet() {
        return (laneID == null) ? false : true;
    }

    /**
     * Gets the lane ID this call ensures that the id cannot be null.
     * 
     * @return The lane ID.
     */
    @Override
    public int getLaneID() {
        return (laneID == null) ? 0 : laneID;
    }

    /**
     * Sets the lane ID.
     * 
     * @param laneID The new lane ID.
     */
    public void setLaneID(Integer laneID) {
        this.laneID = laneID;
    }

    /**
     * Checks to see if the heading has been set.
     * 
     * @return True/False the heading has been set.
     */
    public boolean isHeadingSet() {
        return (heading == null) ? false : true;

    }

    /**
     * Gets the heading and returns as the java primitive double. This ensures that the heading is
     * not able to be null within the vehicle by setting any null value to a default of due north
     * (0.0).
     * 
     * @return The heading which is 0 less than or equal to heading less than 359.9875 Heading is
     *         measured clockwise from north.
     */
    @Override
    public double getHeading() {
        return (heading == null) ? 0.0 : heading;
    }

    /**
     * Sets the heading to J2735 spec heading 0d north, positive clockwise.
     * 
     * @param heading The new heading.
     */
    public void setHeading(double heading) {
        this.heading = heading;
    }

    /**
     * Gets the height of the vehicle.
     * 
     * @return The height.
     */
    @Override
    public double getHeight() {
        return height;
    }

    /**
     * Sets the height of the vehicle.
     * 
     * @param height The new vehicle height.
     */
    public void setHeight(double height) {
        this.height = height;
    }

    /**
     * Gets the type of vehicle.
     * 
     * @return The type of vehicle.
     */
    @Override
    public VEHICLE_TYPE getType() {
        return type;
    }

    /**
     * Sets the type of vehicle.
     * 
     * @param type The new type of vehicle.
     */
    public void setType(VEHICLE_TYPE type) {
        this.type = type;
    }

    /**
     * Gets if the brake is pressed.
     * 
     * @return True/False.
     */
    @Override
    public boolean isBrakePressed() {
        return brakePressed;
    }

    /**
     * Sets if the brake is pressed.
     * 
     * @param brakePressed True/False.
     */
    public void setBrakePressed(boolean brakePressed) {
        this.brakePressed = brakePressed;
    }

    /**
     * Get the heading in the form of {delta x, delta y}
     * 
     * @return {run, rise}
     */
    public double[] genRunRiseHeading() {
        double angle = Math.toRadians(getHeading());

        return new double[] { Math.cos(angle), Math.sin(angle) };
    }

    /**
     * Gets the latitude of the vehicle.
     * 
     * @return latitude The latitude.
     */
    @Override
    public double getLatitude() {
        return latitude;
    }

    /**
     * Set the latitude of the vehicle.
     * 
     * @param latitude The new latitude of the vehicle.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Gets the longitude of the vehicle.
     * 
     * @return longitude The longitude of the vehicle.
     */
    @Override
    public double getLongitude() {
        return longitude;
    }

    /**
     * Set the longitude of the vehicle.
     * 
     * @param longitude The new longitude of the vehicle.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Gets the elevation of the vehicle.
     * 
     * @return elev The elevation of the vehicle.
     */
    @Override
    public double getElev() {
        return elev;
    }

    /**
     * Set the elevation of the vehicle.
     * 
     * @param elev The new elevation of the vehicle.
     */
    public void setElev(double elev) {
        this.elev = elev;
    }

    /**
     * Returns whether the steering wheel angle is available.
     * 
     * @return the availability of the steering wheel angle
     */
    public boolean isSteeringAngleAvailable() {

        return steeringAngle != Byte.MAX_VALUE;
    }

    /**
     * Returns the steering wheel angle (in degrees).
     * 
     * @return the steering wheel angle (in degrees)
     */
    public double getSteeringAngle() {

        return steeringAngle;
    }

    /**
     * Sets the steering wheel angle (in degrees).
     * 
     * @param steeringAngle the new steering wheel angle (in degrees)
     */
    public void setSteeringAngle(double steeringAngle) {

        this.steeringAngle = steeringAngle;
    }

    /**
     * A deep clone of this instance.
     */
    @Override
    public Vehicle clone() {
        try {
            return (Vehicle)super.clone();
        }
        catch (CloneNotSupportedException ex) {
            throw new AssertionError();
        }
    }

    /**
     * Checks if the vehicle id is the same as an object.
     * 
     * @param entity The object to compare to.
     * @return True/False
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof IVehicle) {
            IVehicle d = (IVehicle)entity;
            return vehicleID == d.getVehicleID();
        }
        else {
            return false;
        }
    }

    /**
     * Gets the proper id for the vehicle.
     * 
     * @return The proper id.
     */
    @Override
    public String getProperId() {

        StringBuilder sb = new StringBuilder();
        if (simulationId != null) {

            sb.append("Simulation:");
            sb.append(simulationId);
            sb.append("-");
        }
        sb.append("Vehicle:");
        sb.append(vehicleID);

        return sb.toString();
    }
}