package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Speed Change command options
 *
 * @author llaroussini
 */
public enum SpeedChangeCommand {
    /**
     * The Normal Accelerate command
     */
    NORMAL_ACCELERATE("Normal Accelerate"),
    /**
     * The Max Accelerate command
     */
    MAX_ACCELERATE("Max Accelerate"),
    /**
     * The Normal Decelerate command
     */
    NORMAL_DECELERATE("Normal Decelerate"),
    /**
     * The Max Decelerate command
     */
    MAX_DECELERATE("Max Decelerate");

    /**
     * The label of the options as they appear in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    SpeedChangeCommand(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the option as it is displayed in the Web
     * UI
     *
     * @return The label of the option
     */
    public String getLabel() {
        return this.label;
    }
}