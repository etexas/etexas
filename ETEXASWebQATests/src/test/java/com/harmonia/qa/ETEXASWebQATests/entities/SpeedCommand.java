package com.harmonia.qa.ETEXASWebQATests.entities;

import com.harmonia.qa.ETEXASWebQATests.enums.CommandType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.SpeedChangeCommand;

/**
 * Entity class representing an eTexas speed command
 *
 * @author llaroussini
 */
public class SpeedCommand extends Command {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = -631333494237197371L;

    /**
     * Default Constructor.
     */
    public SpeedCommand() {
        super();
        this.entityType = ETexasEntityType.SPEED_COMMAND;
        this.setCommandType(CommandType.SPEED_CHANGE);
    }

    /**
     * The id of the vehicle associated with the command
     */
    private String vehicleID;

    /**
     * The speed associated with the command
     */
    private String speed;

    /**
     * The speed command
     */
    private SpeedChangeCommand speedCommand;

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
     * @param vehicleID -the vehicleID to set
     */
    public void setVehicleID(String vehicleID) {
        this.vehicleID = vehicleID;
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
     * Gets the speed change command
     *
     * @return the speed change command
     */
    public SpeedChangeCommand getSpeedChangeCommand() {
        return this.speedCommand;
    }

    /**
     * Sets the speed change command
     *
     * @param speedCommand -the speed change command to set
     */
    public void setSpeedChangeCommand(SpeedChangeCommand speedCommand) {
        this.speedCommand = speedCommand;
    }

}
