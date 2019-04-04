package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas uploaded simulation
 *
 * @author llaroussini
 */
public class UploadedSimulation extends Simulation {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = 5003458150380483375L;

    /**
     * Default constructor. Sets Simulation Type to Template
     */
    public UploadedSimulation() {
        super();
        this.setSimFoundation(SimulationFoundation.UPLOAD);
    }

    /**
     * The composite associated with the simulation
     */
    private UUID composite;

    /**
     * Gets the composite UUID with which this simulation is associated
     *
     * @return the UUID of the composite with which this simulation is
     *         associated
     */
    public UUID getCompositeID() {
        return this.composite;
    }

    /**
     * Gets the composite with which this simulation is associated
     *
     * @return the composite with which this simulation is associated
     */
    public CompositeSimulation getComposite() {
        return ETexasEntityManager.getEntity(getCompositeID(), CompositeSimulation.class);

    }

    /**
     * Sets the composite for this simulation
     *
     * @param composite the composite to set
     */
    public void setComposite(CompositeSimulation composite) {
        UUID compositeUUID = composite.getUuid();
        ETexasEntityManager.addEntity(composite);
        this.composite = compositeUUID;
    }

    //TODO support files
}
