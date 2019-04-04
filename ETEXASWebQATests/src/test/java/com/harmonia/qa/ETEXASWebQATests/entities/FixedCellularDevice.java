package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas Fixed Cellular Device
 *
 * @author llaroussini
 */
public class FixedCellularDevice extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -4345014092665320020L;

    /**
     * Default Constructor.
     */
    public FixedCellularDevice() {
        super();
        this.entityType = ETexasEntityType.FIXED_CELLULAR_DEVICE;
    }

    /**
     * The fixed cellular device name
     */
    private String name;

    /**
     * The fixed cell device mac address
     */
    private String macAddress;

    /**
     * The fixed cell device x coordinate
     */
    private String x;

    /**
     * The fixed cell device y coordinate
     */
    private String y;

    /**
     * The fixed cell device z coordinate
     */
    private String z;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * The Fixed Cellular ID
     */
    private String id;

    /**
     * Gets the Fixed Cellular device name
     *
     * @return the Fixed Cellular device name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the Fixed Cellular device name
     *
     * @param name the Fixed Cellular device name to set
     */
    public void setName(String name) {
        if (name == null) {
            this.name = "";
        }
        else {
            this.name = name;
        }
    }

    /**
     * Sets the ID for the Fixed Cellular Device
     *
     * @param id the fixed cellular device id to set
     */
    public void setID(String id) {
        if (id == null) {
            this.id = "";
        }
        else {
            this.id = id;
        }
    }

    /**
     * Gets the Fixed Cellular device mac address
     *
     * @return the Fixed Cellular device mac address
     */
    public String getMacAddress() {
        if (this.macAddress == null) {
            this.setMacAddress("");
        }
        return this.macAddress;
    }

    /**
     * Sets the Fixed Cellular device mac address
     *
     * @param macAddress the Fixed Cellular device mac address to set
     */
    public void setMacAddress(String macAddress) {
        if (macAddress == null) {
            this.macAddress = "";
        }
        else {
            this.macAddress = macAddress;
        }
    }

    /**
     * Gets the x coordinate
     *
     * @return the x coordinate type
     */
    public String getXCoordinate() {
        return this.x;
    }

    /**
     * Sets the x coordinate
     *
     * @param newX the x coordinate to set
     */
    public void setXCoordinate(String x) {
        this.x = x;
    }

    /**
     * Gets the y coordinate
     *
     * @return the y coordinate type
     */
    public String getYCoordinate() {
        return this.y;
    }

    /**
     * Sets the y coordinate
     *
     * @param y the y coordinate to set
     */
    public void setYCoordinate(String y) {
        this.y = y;
    }

    /**
     * Gets the z coordinate
     *
     * @return the z coordinate type
     */
    public String getZCoordinate() {
        return this.z;
    }

    /**
     * Sets the z coordinate
     *
     * @param z the z coordinate to set
     */
    public void setZCoordinate(String z) {
        this.z = z;
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
     * Gets the Fixed Cellular ID (auto-created upon creation of Fixed Cellular
     * in UI)
     *
     * @return the fixed cell ID
     */
    public String getID() {
        return this.id;
    }
}
