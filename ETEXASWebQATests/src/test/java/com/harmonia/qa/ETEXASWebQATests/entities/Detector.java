package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas detector
 *
 * @author llaroussini
 * @author rsmith
 */
public class Detector extends ETexasBaseEntity {

    /**
     * Auto-generated serial UID
     */
    private static final long serialVersionUID = -7769167375672686339L;

    /**
     * Width value of detector
     */
    private String width;

    /**
     * Length value of detector
     */
    private String height;

    /**
     * Distance from stop line value of detector
     */
    private String distance;

    /**
     * lane associated with the detector
     */
    private UUID lane;

    /**
     * The simulation in which the detector is included
     */
    private UUID simulation;

    /**
     * the ID associated with the detector
     */
    private String ID;

    /**
     * Default Constructor.
     */
    public Detector() {
        super();
        this.entityType = ETexasEntityType.DETECTOR;
    }

    /**
     * Gets the detector's width
     *
     * @return the detector's width
     */
    public String getWidth() {
        return this.width;
    }

    /**
     * Sets the detector's width
     *
     * @param width -the width to set
     */
    public void setWidth(String width) {
        this.width = width;
    }

    /**
     * Get's the Detector's ID
     *
     * @return the Detector's ID
     */
    public String getID() {
        return this.ID;
    }

    /**
     * Sets the detector's ID
     *
     * @param ID -the ID to set
     */
    public void setID(String ID) {
        this.ID = ID;
    }

    /**
     * Gets the detector's height
     *
     * @return the detector's height
     */
    public String getHeight() {
        return this.height;
    }

    /**
     * Sets the detector's height
     *
     * @param length -the height to set
     */
    public void setHeight(String height) {
        this.height = height;
    }

    /**
     * Gets the detector's distance from stop line
     *
     * @return the detector's distance from stop line
     */
    public String getDistance() {
        return this.distance;
    }

    /**
     * Sets the detector's distance from stop line
     *
     * @param distance -the distance to set
     */
    public void setDistance(String distance) {
        this.distance = distance;
    }

    /**
     * Gets the UUID for the lane associated with the signal
     *
     * @return the UUID for the lane associated with the signal
     */
    public UUID getLaneUUID() {
        return this.lane;
    }

    /**
     * Gets the lane associated with the detector
     *
     * @return the lane associated with the detector
     */
    public Lane getLane() {
        return ETexasEntityManager.getEntity(getLaneUUID(), Lane.class);
    }

    /**
     * Sets the lane for this detector
     *
     * @param lane the lane to associate with the detector
     */
    public void setLane(Lane lane) {
        UUID laneUUID = lane.getUuid();
        this.lane = laneUUID;
    }

    /**
     * Gets the UUID for the simulation with which this detector is associated
     *
     * @return the UUID for the simulation with which this detector is
     *         associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this detector is associated
     *
     * @return the simulation with which this detector is associated
     */
    public Simulation getSimulation() {
        return ETexasEntityManager.getEntity(getSimulationID(), Simulation.class);
    }

    /**
     * Sets the simulation for this lane
     *
     * @param simualtion the simulation to associate with the lane
     */
    public void setSimulation(Simulation sim) {
        UUID simUUID = sim.getUuid();
        this.simulation = simUUID;
    }

}
