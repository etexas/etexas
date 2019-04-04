package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas app
 *
 * @author llaroussini
 */
public class EmbeddedApp extends ETexasBaseEntity {

    /**
     * Auto generated serial ID
     */
    private static final long serialVersionUID = 1076577598841660023L;

    /**
     * Default Constructor.
     */
    public EmbeddedApp() {
        super();
        this.entityType = ETexasEntityType.BUILT_IN_APP;
    }

    /**
     * The app name
     */
    private String name;

    /**
     * The app type
     */
    private DeviceType appType;

    /**
     * The app parameters
     */
    private List<UUID> parameters = new ArrayList<UUID>();

    /**
     * The device on which the apps are assigned
     */
    private UUID device;

    /**
     * Gets the app's name
     *
     * @return the apps' name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Sets the app's name
     *
     * @param name -the app name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Gets the app's type
     *
     * @return the app's type
     */
    public DeviceType getDeviceType() {
        return this.appType;
    }

    /**
     * Sets the app's type
     *
     * @param type -the app type to set
     */
    public void setDeviceType(DeviceType type) {
        this.appType = type;
    }

    /**
     * Gets the UUID list of parameters for this app
     *
     * @return the UUID parameters list
     */
    public List<UUID> getParameterIds() {
        return this.parameters;
    }

    /**
     * Gets the app's associated parameters
     *
     * @return app's associated parameters
     */
    public List<AppParameter> getParameters() {
        List<AppParameter> parameterList = new ArrayList<AppParameter>(0);
        if (this.parameters == null) {
            return null;
        }
        else {
            for (UUID id : this.parameters) {
                parameterList.add(ETexasEntityManager.getEntity(id, AppParameter.class));
            }
            return parameterList;
        }
    }

    /**
     * Sets the parameter list for this app
     *
     * @param parameters -the list of parameters to set
     */
    public void setParameters(List<AppParameter> parameters) {
        List<UUID> paramList = new ArrayList<UUID>(parameters.size());
        for (AppParameter parameter : parameters) {
            UUID paramUUID = parameter.getUuid();
            paramList.add(paramUUID);
        }
        this.parameters = paramList;
    }

    /**
     * Adds a parameter to this app's list of parameters
     *
     * @param parameter the parameter to add
     */
    public void addParameter(AppParameter parameter) {
        if (this.getParameters() == null) {
            this.setParameters(new ArrayList<AppParameter>(1));
        }
        this.getParameters().add(parameter);
        if (parameter.getApp() == null || !parameter.getApp().equals(this)) {
            parameter.setApp(this);
        }
    }

    /**
     * Gets the UUID for the obu device with which this app is associated
     *
     * @return the UUID for the obu device with which this app is associated
     */
    public UUID getOBUDeviceID() {
        return this.device;
    }

    /**
     * Gets the obu device with which this app is associated
     *
     * @return the obu device with which this app is associated
     */
    public OBUDevice getOBUDevice() {
        return ETexasEntityManager.getEntity(getOBUDeviceID(), OBUDevice.class);
    }

    /**
     * Sets the obu device for this app
     *
     * @param obu device the obu device to associate with the app
     */
    public void setOBUDevice(OBUDevice obu) {
        UUID obuUUID = obu.getUuid();
        this.device = obuUUID;
    }

}
