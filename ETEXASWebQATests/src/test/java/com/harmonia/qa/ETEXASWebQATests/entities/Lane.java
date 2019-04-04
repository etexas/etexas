package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneID;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneMovement;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas lane
 *
 * @author llaroussini
 */
public class Lane extends ETexasBaseEntity {

    /**
     * /** Auto-generated serial UID
     */
    private static final long serialVersionUID = -5910075161341133744L;

    /**
     * The lane ID used in application
     */
    private LaneID laneID;

    /**
     * The lane's approach value used in application to determine approach to
     * intersection
     */
    private String approach;

    /**
     * The lane type (Inbound/Outbound)
     */
    private LaneType type;

    /**
     * List of movements associated with the lane (left turn, right turn,
     * straight)
     */
    private List<LaneMovement> movements;

    /**
     * Speed limit in the lane
     */
    private String speedLimit;

    /**
     * Node defining the starting location of the lane
     */
    private String startNode;

    /**
     * Node defining the ending location of the lane
     */
    private String endNode;

    /**
     * Signal associated with the lane
     */
    private UUID signal;

    /**
     * Detector associated with the lane
     */
    private UUID detector;

    /**
     * The simulation in which the lane is included
     */
    private UUID simulation;

    /**
     * Default Constructor.
     */
    public Lane() {
        super();
        this.entityType = ETexasEntityType.LANE;
    }

    /**
     * Gets the lane's ID
     *
     * @return the lane's ID
     */
    public LaneID getLaneID() {
        return this.laneID;
    }

    /**
     * Sets the lane's ID
     *
     * @param id -the lane ID to set
     */
    public void setLaneID(LaneID id) {
        this.laneID = id;
    }

    /**
     * Gets the lane's approach value
     *
     * @return the lane's approach value
     */
    public String getLaneApproach() {
        return this.approach;
    }

    /**
     * Sets the lane's approach value
     *
     * @param approach -the lane approach to set
     */
    public void setLaneApproach(String approach) {
        this.approach = approach;
    }

    /**
     * Gets the lane type
     *
     * @return the lane type
     */
    public LaneType getLaneType() {
        return this.type;
    }

    /**
     * Sets the lane type
     *
     * @param laneType -the lane type to set
     */
    public void setLaneType(LaneType laneType) {
        this.type = laneType;
    }

    /**
     * Gets the lane's movements
     *
     * @return the lane's movements
     */
    public List<LaneMovement> getLaneMovements() {
        return this.movements;
    }

    /**
     * Sets the lane's movements
     *
     * @param movements -the lane movements to set
     */
    public void setLaneMovements(List<LaneMovement> movements) {
        this.movements = movements;
    }

    /**
     * Gets the lane's speed limit
     *
     * @return the lane's speed limit
     */
    public String getSpeedLimit() {
        return this.speedLimit;
    }

    /**
     * Sets the lane's speed limit
     *
     * @param speed -the lane speed limit to set
     */
    public void setSpeedLimit(String speed) {
        this.speedLimit = speed;
    }

    /**
     * Gets the lane's starting node (coordinates)
     *
     * @return the lane's starting node (coordinates)
     */
    public String getLaneStartNode() {
        return this.startNode;
    }

    /**
     * Sets the lane's starting node (coordinates)
     *
     * @param node -the lane start nodes to set (in format "(x, y, z, w)")
     */
    public void setLaneStartNode(String node) {
        this.startNode = "1: " + node;
    }

    /**
     * Gets the lane's ending node (coordinates)
     *
     * @return the lane's ending node (coordinates)
     */
    public String getLaneEndNode() {
        return this.endNode;
    }

    /**
     * Sets the lane's ending node (coordinates)
     *
     * @param node -the lane end nodes to set (in format "(x, y, z, w)")
     */
    public void setLaneEndNode(String node) {
        this.endNode = "2: " + node;
    }

    /**
     * Gets the UUID for the signal associated with the lane
     *
     * @return the UUID for the signal associated with the lane
     */
    public UUID getSignalID() {
        return this.signal;
    }

    /**
     * Gets the signal associated with the lane
     *
     * @return the signal associated with the lane
     */
    public Signal getSignal() {
        return ETexasEntityManager.getEntity(getSignalID(), Signal.class);
    }

    /**
     * Sets the signal for this lane
     *
     * @param signal the signal to associate with the lane
     */
    public void setSignal(Signal signal) {
        UUID signalUUID = signal.getUuid();
        this.signal = signalUUID;
    }

    /**
     * Gets the UUID for the detector associated with the lane
     *
     * @return the UUID for the detector associated with the lane
     */
    public UUID getDetectorID() {
        return this.detector;
    }

    /**
     * Gets the detector associated with the lane
     *
     * @return the detector associated with the lane
     */
    public Detector getDetector() {
        return ETexasEntityManager.getEntity(getDetectorID(), Detector.class);
    }

    /**
     * Sets the detector for this lane
     *
     * @param detector the detector to associate with the lane
     */
    public void setDetector(Detector detector) {
        UUID detectorUUID = detector.getUuid();
        this.detector = detectorUUID;
    }

    /**
     * Gets the UUID for the simulation with which this lane is associated
     *
     * @return the UUID for the simulation with which this lane is associated
     */
    public UUID getSimulationID() {
        return this.simulation;
    }

    /**
     * Gets the simulation with which this lane is associated
     *
     * @return the simulation with which this lane is associated
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
