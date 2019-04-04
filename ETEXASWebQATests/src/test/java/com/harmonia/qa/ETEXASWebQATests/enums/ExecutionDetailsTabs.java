package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Tabs associated with details of an execution
 *
 * @author llaroussini
 */
public enum ExecutionDetailsTabs {
    /**
     * The Vehicles detail tab
     */
    VEHICLES_TAB("Vehicles"),
    /**
     * The Signals detail tab
     */
    SIGNALS_TAB("Signals"),
    /**
     * The Detectors detail tab
     */
    DETECTORS_TAB("Detectors"),
    /**
     * The Lane Geometry detail tab
     */
    LANE_GEOMETRY_TAB("Lane Geometry"),
    /**
     * The Logs detail tab
     */
    LOGS_TAB("Logs"),
    /**
     * The Command History detail tab
     */
    COMMAND_HISTORY_TAB("Command History");

    /**
     * The label of the buttons as they appear in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    ExecutionDetailsTabs(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the button as it is displayed in the Web
     * UI
     *
     * @return The label of the button
     */
    public String getLabel() {
        return this.label;
    }
}
