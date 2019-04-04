package com.harmonia.qa.ETEXASWebQATests.entities;

import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.SignalType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;

/**
 * Entity class representing an eTexas signal
 *
 * @author llaroussini
 */
public class Signal extends ETexasBaseEntity {

    /**
     * Auto-generated serial ID
     */
    private static final long serialVersionUID = 6864508846802135896L;

    /**
     * The type of signal
     */
    private SignalType type;

    /**
     * The lane in which the signal is located
     */
    private UUID lane;

    /**
     * Default Constructor.
     */
    public Signal() {
        super();
        this.entityType = ETexasEntityType.SIGNAL;
    }

    /**
     * Gets the signal type
     *
     * @return the signal type
     */
    public SignalType getSignalType() {
        return this.type;
    }

    /**
     * Sets the signal type
     *
     * @param type -the signal type to set
     */
    public void setSignalType(SignalType type) {
        this.type = type;
    }

    /**
     * Gets the UUID for the lane associated with the signal
     *
     * @return the UUID for the lane associated with the signal
     */
    public UUID getLaneID() {
        return this.lane;
    }

    /**
     * Gets the lane associated with the signal
     *
     * @return the lane associated with the signal
     */
    public Lane getLane() {
        return ETexasEntityManager.getEntity(getLaneID(), Lane.class);
    }

    /**
     * Sets the lane for this signal
     *
     * @param lane the lane to associate with the signal
     */
    public void setLane(Lane lane) {
        UUID laneUUID = lane.getUuid();
        this.lane = laneUUID;
    }

}
