package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enumeration of lane IDs
 *
 * @author llaroussini
 */
public enum LaneID {
    /**
     * Lane 1
     */
    ONE("1"),
    /**
     * Lane 2
     */
    TWO("2"),
    /**
     * Lane 3
     */
    THREE("3"),
    /**
     * Lane 4
     */
    FOUR("4"),
    /**
     * Lane 5
     */
    FIVE("5"),
    /**
     * Lane 6
     */
    SIX("6"),
    /**
     * Lane 7
     */
    SEVEN("7"),
    /**
     * Lane 8
     */
    EIGHT("8"),
    /**
     * Lane 9
     */
    NINE("9"),
    /**
     * Lane 10
     */
    TEN("10"),
    /**
     * Lane 11
     */
    ELEVEN("11"),
    /**
     * Lane 12
     */
    TWELVE("12"),
    /**
     * Lane 13
     */
    THIRTEEN("13"),
    /**
     * Lane 14
     */
    FOURTEEN("14"),
    /**
     * Lane 15
     */
    FIFTEEN("15"),
    /**
     * Lane 16
     */
    SIXTEEN("16");

    /**
     * The label of the lane ID as appears in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    LaneID(String label) {
        this.label = label;
    }

    /**
     * Gets the label associated with the lane ID as it is displayed in the Web
     * UI
     *
     * @return The label of the lane movement
     */
    public String getLabel() {
        return this.label;
    }

}
