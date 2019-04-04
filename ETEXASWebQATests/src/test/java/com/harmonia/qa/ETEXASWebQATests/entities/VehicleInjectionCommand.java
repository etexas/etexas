package com.harmonia.qa.ETEXASWebQATests.entities;

import com.harmonia.qa.ETEXASWebQATests.enums.CommandType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;

/**
 * Entity class representing an eTexas vehicle injection command
 *
 * @author llaroussini
 */
public class VehicleInjectionCommand extends Command {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -3306140099704517548L;

    /**
     * Default Constructor.
     */
    public VehicleInjectionCommand() {
        super();
        this.entityType = ETexasEntityType.VEHICLE_INJECTION_COMMAND;
        this.setCommandType(CommandType.INJECT_VEHICLE);
    }

    /**
     * The lane associated with the command
     */
    private String lane;

    /**
     * The speed associated with the command
     */
    private String speed;

    /**
     * The vehicle ID associated with the command
     */
    private String vehicleID;

    /**
     * The vehicle type associated with the command
     */
    private String vehicleType;

    /**
     * Gets the lane
     *
     * @return the lane
     */
    public String getLane() {
        return this.lane;
    }

    /**
     * Sets the lane
     *
     * @param lane -the lane to set
     */
    public void setLane(String lane) {
        this.lane = lane;
    }

    /**
     * Gets the speed
     *
     * @return the speed
     */
    public String getSpeed() {
        return this.speed;
    }

    /**
     * Sets the speed
     *
     * @param speed -the speed to set
     */
    public void setSpeed(String speed) {
        this.speed = speed;
    }

    /**
     * Gets the vehicle ID
     *
     * @return the vehicle ID
     */
    public String getVehicleID() {
        return this.vehicleID;
    }

    /**
     * Sets the vehicle ID
     *
     * @param vehicleID -the vehicle ID to set
     */
    public void setVehicleID(String vehicleID) {
        this.vehicleID = vehicleID;
    }

    /**
     * Gets the vehicle type
     *
     * @return the vehicle type
     */
    public String getVehicleType() {
        return this.vehicleType;
    }

    /**
     * Sets the vehicle type
     *
     * @param vehicleType -the vehicle type to set
     */
    public void setVehicleType(String vehicleType) {
        this.vehicleType = vehicleType;
    }

}
