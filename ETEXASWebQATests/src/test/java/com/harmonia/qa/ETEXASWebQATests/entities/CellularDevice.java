package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas Cellular Device
 *
 * @author llaroussini
 */
public class CellularDevice extends ETexasBaseEntity {

    /**
     * Auto-generate serial UID
     */
    private static final long serialVersionUID = -604450280798377686L;

    /**
     * Default Constructor.
     */
    public CellularDevice() {
        super();
        this.entityType = ETexasEntityType.CELLULAR_DEVICE;
    }

    /**
     * The cellular device name
     */
    private String name;

    /**
     * The cellular rule percentage
     */
    private String percent;

    /**
     * The min number of cells per vehicles for the cellular device rule
     */
    private String minCellsPerVehicle;

    /**
     * The max number of cells per vehicle for the cellular device rule
     */
    private String maxCellsPerVehicle;

    /**
     * The ID of the Cellular Device as it appears in the GUI
     */
    private String deviceID;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * Gets the Cellular device name
     *
     * @return the Cellular device name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the Cellular device name
     *
     * @param name the Cellular device name to set
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
     * Gets the cellular rule percentage value
     *
     * @return the cellular rule percentage value
     */
    public String getCellularPercent() {
        return this.percent;
    }

    /**
     * Sets the cellular rule percentage value
     *
     * @param percent the percent value (as int) to set
     */
    public void setCellularPercent(String percent) {
        this.percent = percent;
    }

    /**
     * Gets the min number of cells per vehicles for the cellular device/rule
     *
     * @return the min number of cells per vehicles as int
     */
    public String getMinCellsPerVehicle() {
        return this.minCellsPerVehicle;
    }

    /**
     * Sets the min number of cells per vehicles for the cellular device/rule
     *
     * @param minCellsPerVehicle the min number of cells per vehicles to set
     */
    public void setMinCellsPerVehicle(String minCellsPerVehicle) {
        this.minCellsPerVehicle = minCellsPerVehicle;
    }

    /**
     * Gets the max number of cells per vehicles for the cellular device/rule
     *
     * @return the max number of cells per vehicles as string
     */
    public String getMaxCellsPerVehicle() {
        return this.maxCellsPerVehicle;
    }

    /**
     * Sets the max number of cells per vehicles for the cellular device/rule
     *
     * @param maxCellsPerVehicle the max number of cells per vehicles to set
     */
    public void setMaxCellsPerVehicle(String maxCellsPerVehicle) {
        this.maxCellsPerVehicle = maxCellsPerVehicle;
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
     * Gets the simulation's ID in the GUI (auto-generated upon device creation)
     *
     * @return the String ID of the cellular device
     */
    public String getDeviceId() {
        return this.deviceID;
    }

    /**
     * Sets the ID of the device profile based on the provided input
     *
     * @param id - the String ID to bet set for the device
     */
    public void setDeviceId(String id) {
        this.deviceID = id;
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
