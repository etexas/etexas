package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas app parameter
 *
 * @author llaroussini
 */
public class AppParameter extends ETexasBaseEntity {

    /**
     * Auto generated serial ID
     */
    private static final long serialVersionUID = -5651137801276784639L;

    /**
     * Default Constructor.
     */
    public AppParameter() {
        super();
        this.entityType = ETexasEntityType.APP_PARAMETER;
    }

    /**
     * App parameter name
     */
    private String name;

    /**
     * App parameter value
     */
    private String value;

    /**
     * The app to which the parameter is assigned
     */
    private UUID app;

    /**
     * Gets the app parameter name
     *
     * @return the app parameter name
     */
    public String getParameterName() {
        return this.name;
    }

    /**
     * Sets the app parameter name
     *
     * @param name -the app parameter name to set
     */
    public void setParameterName(String name) {
        this.name = name;
    }

    /**
     * Gets the app parameter value
     *
     * @return the app parameter value
     */
    public String getParameterValue() {
        return this.value;
    }

    /**
     * Sets the app parameter value
     *
     * @param value -the app parameter value to set
     */
    public void setParameterValue(String value) {
        this.value = value;
    }

    /**
     * Gets the UUID for the app with which this parameter is associated
     *
     * @return the UUID for the app with which this parameter is associated
     */
    public UUID getAppID() {
        return this.app;
    }

    /**
     * Gets the app with which this parameter is associated
     *
     * @return the app with which this parameter is associated
     */
    public EmbeddedApp getApp() {
        return ETexasEntityManager.getEntity(getAppID(), EmbeddedApp.class);
    }

    /**
     * Sets the app for this parameter
     *
     * @param app the app to associate with the parameter
     */
    public void setApp(EmbeddedApp app) {
        UUID appUUID = app.getUuid();
        this.app = appUUID;
    }

}
