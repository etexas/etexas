package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enumeration of buttons common to various windows and forms in the application
 *
 * @author llaroussini
 */
public enum BtnNames {
    /**
     * Add button
     */
    ADD("Add"),
    /**
     * Create button
     */
    CREATE("Create"),
    /**
     * Update button
     */
    UPDATE("Update"),
    /**
     * Register button
     */
    REGISTER("Register"),
    /**
     * Reset button
     */
    RESET("Reset"),
    /**
     * Cancel button
     */
    CANCEL("Cancel"),
    /**
     * Edit button
     */
    EDIT("Edit"),
    /**
     * Save button
     */
    SAVE("Save"),
    /**
     * Close button
     */
    CLOSE("Close"),
    /**
     * Delete button
     */
    DELETE("Delete"),

    /**
     * Show Lanes Button
     */
    SHOWLANES("Show Lanes");

    /**
     * The label of button as they appear in the application
     */
    private String label;

    /**
     * Default constructor; sets the label
     *
     * @param label The string to set as the label
     */
    BtnNames(String label) {
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
