package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enum representing signal change command types in application
 *
 * @author llaroussini
 */
public enum SignalChangeCommandType {
    /**
     * Change Signal
     */
    CHANGE_SIGNAL("Change Signal"),
    /**
     * Hold Signal
     */
    HOLD_SIGNAL("Hold Signal");

    /**
     * The label of the signal change command type as appears in the application
     * when creating the command or viewing in command history
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    SignalChangeCommandType(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the signal change command type as it is
     * displayed in the Web UI when creating the command and when displayed in
     * command history
     *
     * @return The label of the signal change command
     */
    public String getLabel() {
        return this.label;
    }

}
