package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas OBU Device
 *
 * @author llaroussini
 */
public class OBUDevice extends ETexasBaseEntity {

    /**
     * Auto generated serial UUID
     */
    private static final long serialVersionUID = -2368611467341627952L;

    /**
     * Default Constructor
     */
    public OBUDevice() {
        super();
        this.entityType = ETexasEntityType.OBU_DEVICE;
    }

    /**
     * The OBU device name
     */
    private String name;

    /**
     * The OBU rule percentage
     */
    private String percent;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * The OBU ID
     */
    private String id;

    /**
     * The apps associated with the device
     */
    private List<UUID> apps;

    /**
     * Gets the OBU device name
     *
     * @return the OBU device name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Sets the OBU device name
     *
     * @param name the name of OBU device to set
     */
    public void setOBUName(String name) {
        this.name = name;
    }

    /**
     * Gets the OBU rule percentage value
     *
     * @return the OBU rule percentage value
     */
    public String getOBUPercent() {
        return this.percent;
    }

    /**
     * Sets the OBU rule percentage value
     *
     * @param percent the percent value (as string) to set
     */
    public void setOBUPercent(String percent) {
        this.percent = percent;
    }

    /**
     * Gets the simulation UUID with which this device is associated
     *
     * @return the UUID of the simulation with which this device is associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this device is associated
     *
     * @return the simulation with which this device is associated
     */
    public Simulation getSimulation() {
        return ETexasEntityManager.getEntity(getSimulationID(), Simulation.class);

    }

    /**
     * Sets the simulation for this device
     *
     * @param sim the simulation to set
     */
    public void setSimulation(Simulation sim) {
        UUID simUUID = sim.getUuid();
        this.simulation = simUUID;
    }

    /**
     * Gets the OBU ID (auto-created upon creation of OBU in UI)
     *
     * @return the OBU ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the OBU ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
    }

    /**
     * Gets the UUID list of apps for this device
     *
     * @return the UUID apps list
     */
    public List<UUID> getAppIds() {
        return this.apps;
    }

    /**
     * Gets the device's associated apps
     *
     * @return device's associated apps
     */
    public List<EmbeddedApp> getApps() {
        List<EmbeddedApp> appList = new ArrayList<EmbeddedApp>();
        for (UUID id : this.apps) {
            appList.add(ETexasEntityManager.getEntity(id, EmbeddedApp.class));
        }
        return appList;
    }

    /**
     * Sets the app list for this device
     *
     * @param apps -the list of apps to set
     */
    public void setApps(List<EmbeddedApp> apps) {
        List<UUID> appList = new ArrayList<UUID>(apps.size());
        for (EmbeddedApp app : apps) {
            UUID appUUID = app.getUuid();
            appList.add(appUUID);
        }
        this.apps = appList;
    }

    /**
     * Adds an app to this device's list of apps
     *
     * @param app the app to add
     */
    public void addApp(EmbeddedApp app) {
        if (this.getApps() == null) {
            this.setApps(new ArrayList<EmbeddedApp>(1));
        }
        this.getApps().add(app);
        if (app.getOBUDevice() == null || !app.getOBUDevice().equals(this)) {
            app.setOBUDevice(this);
        }
    }

}
