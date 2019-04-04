package com.harmonia.qa.ETEXASWebQATests.enums;

/**
 * Enumeration of app types
 *
 * @author llaroussini
 */
public enum DeviceType {
    /**
     * OBU device type
     */
    OBU("OBU", "OBU"),
    /**
     * RSE device type
     */
    RSE("RSE", "RSE"),
    /**
     * Report device type
     */
    REPORT("Report", "Report"),
    /**
     * Cellular device type
     */
    CELLULAR("Cellular", "Cellular"),
    /**
     * Fixed cellular device type
     */
    FIXED_CELLULAR("Fixed Cellular", "Cellular");

    /**
     * The label of the device type as appears in the REST API
     */
    private String restLabel;

    /**
     * The label of the device type as appears in the UI
     */
    private String uiLabel;

    /**
     * Default constructor; sets the label
     *
     * @param restLabel -the label of the device type as appears in the REST API
     * @param uiLabel -the label of the device type as appears in the UI
     */
    DeviceType(String restLabel, String uiLabel) {
        this.restLabel = restLabel;
        this.uiLabel = uiLabel;
    }

    /**
     * Gets the label of the device type as appears in the REST API
     *
     * @return The REST label of the device type
     */
    public String getRESTLabel() {
        return this.restLabel;
    }

    /**
     * Gets the label of the device type as appears in the UI
     *
     * @return The UI label of the device type
     */
    public String getUILabel() {
        return this.uiLabel;
    }
}