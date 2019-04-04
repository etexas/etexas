package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas Cell Tower
 *
 * @author llaroussini
 */
public class CellTower extends ETexasBaseEntity {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = -2866107321610236564L;

    /**
     * Default Constructor.
     */
    public CellTower() {
        super();
        this.entityType = ETexasEntityType.CELL_TOWER;
    }

    /**
     * The cell tower provider
     */
    private String provider;

    /**
     * The cell tower x coordinate
     */
    private String x;

    /**
     * The cell tower y coordinate
     */
    private String y;

    /**
     * The cell tower z coordinate
     */
    private String z;

    /**
     * The cell tower ID
     */
    private String id;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * Gets the cell tower provider
     *
     * @return the cell tower provider
     */
    public String getProvider() {
        if (this.provider == null) {
            this.setProvider("");
        }
        return this.provider;
    }

    /**
     * Sets the cell tower provider
     *
     * @param provider the cell tower provider to set
     */
    public void setProvider(String provider) {
        if (provider == null) {
            this.provider = "";
        }
        else {
            this.provider = provider;
        }
    }

    /**
     * Gets the cell tower x coordinate
     *
     * @return the cell tower x coordinate
     */
    public String getXCoordinate() {
        return this.x;
    }

    /**
     * Sets the cell tower x coordinate
     *
     * @param x the x coordinate (as string) to set
     */
    public void setXCoordinate(String x) {
        this.x = x;
    }

    /**
     * Gets the cell tower y coordinate
     *
     * @return the cell tower y coordinate
     */
    public String getYCoordinate() {
        return this.y;
    }

    /**
     * Sets the cell tower y coordinate
     *
     * @param y the y coordinate (as string) to set
     */
    public void setYCoordinate(String y) {
        this.y = y;
    }

    /**
     * Gets the cell tower z coordinate
     *
     * @return the cell tower z coordinate
     */
    public String getZCoordinate() {
        return this.z;
    }

    /**
     * Sets the cell tower z coordinate
     *
     * @param z the z coordinate (as string) to set
     */
    public void setZCoordinate(String z) {
        this.z = z;
    }

    /**
     * Gets the cell tower ID (auto-created upon creation of cell tower in UI)
     *
     * @return the cell tower ID
     */
    public String getID() {
        return this.id;
    }

    /**
     * Sets the cell tower ID
     *
     * @param id the id (as string) to set
     */
    public void setID(String id) {
        this.id = id;
    }

    /**
     * Gets the simulation UUID with which this tower is associated
     *
     * @return the UUID of the simulation with which this tower is associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this tower is associated
     *
     * @return the simulation with which this tower is associated
     */
    public Simulation getSimulation() {
        return ETexasEntityManager.getEntity(getSimulationID(), Simulation.class);

    }

    /**
     * Sets the simulation for this tower
     *
     * @param sim the simulation to set
     */
    public void setSimulation(Simulation sim) {
        UUID simUUID = sim.getUuid();
        this.simulation = simUUID;
    }
}
