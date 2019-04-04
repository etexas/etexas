package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an ETexas RSE Device
 *
 * @author llaroussini
 */
public class ReportDevice extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Default Constructor.
     */
    public ReportDevice() {
        super();
        this.entityType = ETexasEntityType.REPORT_DEVICE;
    }

    /**
     * The Report device name
     */
    private String name;

    /**
     * The simulation with which this device is associated/assigned
     */
    private UUID simulation;

    /**
     * Gets the Report device name
     *
     * @return the Report device name
     */
    public String getName() {
        if (this.name == null) {
            this.setName("");
        }
        return this.name;
    }

    /**
     * Sets the Report device name
     *
     * @param name the Report device name to set
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
