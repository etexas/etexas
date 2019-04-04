package com.harmonia.qa.ETEXASWebQATests.entities;

import com.harmonia.qa.ETEXASWebQATests.enums.CommandType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneChangeCommandType;

/**
 * Entity class representing an eTexas lane change command
 *
 * @author llaroussini
 */
public class LaneChangeCommand extends Command {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = 5241216605608310123L;

    /**
     * Default Constructor.
     */
    public LaneChangeCommand() {
        super();
        this.entityType = ETexasEntityType.LANE_CHANGE_COMMAND;
        this.setCommandType(CommandType.LANE_CHANGE);
    }

    /**
     * The id of the vehicle associated with the command
     */
    private String vehicleID;

    /**
     * The lane change command
     */
    private LaneChangeCommandType laneCommandType;

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
     * Gets the lane change command type
     *
     * @return the lane change command type
     */
    public LaneChangeCommandType getLaneChangeCommandType() {
        return this.laneCommandType;
    }

    /**
     * Sets the lane change command type
     *
     * @param laneCommandType -the lane change command type to set
     */
    public void setLaneChangeCommandType(LaneChangeCommandType laneCommandType) {
        this.laneCommandType = laneCommandType;
    }

}
