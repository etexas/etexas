package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas RSE Device
 *
 * @author llaroussini
 */
public class RSEDevice extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -3541050930713226926L;

    /**
     * Default Constructor.
     */
    public RSEDevice() {
        super();
        this.entityType = ETexasEntityType.RSE_DEVICE;
    }

    /**
     * The RSE device name
     */
    private String name;

    /**
     * The X coordinate of the RSE Device
     */
    private String xCoordinate;

    /**
     * The Y coordinate of the RSE Device
     */
    private String yCoordinate;

    /**
     * The Z coordinate of the RSE Device
     */
    private String zCoordinate;

    /**
     * The RSE ID
     */
    private String id;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * Gets the RSE device name
     *
     * @return the RSE device name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the RSE device name
     *
     * @param name the RSE device name to set
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
     * Gets the x coordinate
     *
     * @return the x coordinate type
     */
    public String getXCoordinate() {
        return this.xCoordinate;
    }

    /**
     * Sets the x coordinate
     *
     * @param xCoord the x coordinate to set
     */
    public void setXCoordinate(String xCoord) {
        this.xCoordinate = xCoord;
    }

    /**
     * Gets the y coordinate
     *
     * @return the y coordinate type
     */
    public String getYCoordinate() {
        return this.yCoordinate;
    }

    /**
     * Sets the y coordinate
     *
     * @param yCoord the y coordinate to set
     */
    public void setYCoordinate(String yCoord) {
        this.yCoordinate = yCoord;
    }

    /**
     * Gets the z coordinate
     *
     * @return the z coordinate type
     */
    public String getZCoordinate() {
        return this.zCoordinate;
    }

    /**
     * Sets the z coordinate
     *
     * @param zCoord the z coordinate to set
     */
    public void setZCoordinate(String zCoord) {
        this.zCoordinate = zCoord;
    }

    /**
     * Gets the RSE ID (auto-created upon creation of RSE in UI)
     *
     * @return the RSE ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the RSE ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
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

}
