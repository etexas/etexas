package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enumeration of command types
 *
 * @author llaroussini
 */
public enum CommandType {
    /**
     * Speed change command type
     */
    SPEED_CHANGE("Speed Change"),
    /**
     * Lane change command type
     */
    LANE_CHANGE("Lane Change"),
    /**
     * Signal change command type
     */
    SIGNAL_CHANGE("Signal Change"),
    /**
     * Inject vehicle command type
     */
    INJECT_VEHICLE("Vehicle Injection");

    /**
     * The label of the command type as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    CommandType(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the command type as it is displayed in the
     * Web UI
     *
     * @return The label of the command type
     */
    public String getLabel() {
        return this.label;
    }
}
