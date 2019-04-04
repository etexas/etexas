package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enum representing lane movements in application
 *
 * @author llaroussini
 */
public enum LaneMovement {
    /**
     * Straight lane movement
     */
    STRAIGHT("STRAIGHT"),
    /**
     * Right turn lane movement
     */
    RIGHT_TURN("RIGHT_TURN"),
    /**
     * Left turn lane movement
     */
    LEFT_TURN("LEFT_TURN");

    /**
     * The label of the lane movement as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    LaneMovement(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the lane movement as it is displayed in
     * the Web UI
     *
     * @return The label of the lane movement
     */
    public String getLabel() {
        return this.label;
    }
}
