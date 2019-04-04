package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enum representing lane change command types in application
 *
 * @author llaroussini
 */
public enum LaneChangeCommandType {
    /**
     * Change to left lane
     */
    CHANGE_LANE_LEFT("Change Lane Left"),
    /**
     * Change to right lane
     */
    CHANGE_LANE_RIGHT("Change Lane Right");

    /**
     * The label of the lane change command type as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param fullCommandlabel The string to set as the label
     */
    LaneChangeCommandType(String label) {
        this.label = label;

    }

    /**
     * Gets the label associated with the lane change command type as it is
     * displayed in the Web UI
     *
     * @return The label of the lane movement
     */
    public String getLabel() {
        return this.label;
    }

}