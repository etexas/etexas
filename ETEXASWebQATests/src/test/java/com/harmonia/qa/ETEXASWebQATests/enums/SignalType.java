package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enum representing signal types in application
 *
 * @author llaroussini
 */
public enum SignalType {
    /**
     * Ball signal type
     */
    BALL("BALL");

    /**
     * The label of the signal type as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    SignalType(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the signal type as it is displayed in the
     * Web UI
     *
     * @return The label of the signal type
     */
    public String getLabel() {
        return this.label;
    }
}
