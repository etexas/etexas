package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enum representing lane types in application
 *
 * @author llaroussini
 */
public enum LaneType {
    /**
     * Inbound lane type
     */
    INBOUND("INBOUND"),
    /**
     * Outbound lane type
     */
    OUTBOUND("OUTBOUND");

    /**
     * The label of the lane type as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    LaneType(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the lane type as it is displayed in the
     * Web UI
     *
     * @return The label of the lane type
     */
    public String getLabel() {
        return this.label;
    }
}
