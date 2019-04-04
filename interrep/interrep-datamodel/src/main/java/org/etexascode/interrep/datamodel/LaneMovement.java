/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;

/**
 * The model for a lane movement.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class LaneMovement implements Serializable, ILaneMovement {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -6306445106705081573L;

    /** Enumeration of movements. */
    @XmlType
    @XmlEnum
    public enum Movement {
        LEFT_TURN,
        RIGHT_TURN,
        STRAIGHT,
        RIGHT_TURN_ON_RED,
        LEFT_TURN_ON_RED,
        U_TURN
    };

    /** Movement ID. */
    @XmlElement
    private int movementId;

    /** Movement. */
    @XmlElement
    private Movement movement;

    public LaneMovement() {}

    /**
     * A constructor used to duplicate a lane movement.
     * 
     * @param laneMovement The lane movement to duplicate.
     */
    public LaneMovement(LaneMovement laneMovement) {

        this.movement = laneMovement.getMovement();
        this.movementId = laneMovement.getMovementId();
    }

    /** String representation of movement. */
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("movement id = ");
        ret.append(movementId);
        ret.append("\n");

        ret.append("movement = ");
        ret.append(movement);
        ret.append("\n");

        return ret.toString();
    }

    /** Checks if two lane movements are equal. */
    @Override
    public boolean equals(Object o) {
        if (o instanceof LaneMovement) {
            LaneMovement lm = (LaneMovement)o;
            return (movementId == lm.movementId) && (movement == lm.movement);
        }
        else {
            return false;
        }
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder(33, 29).append(movementId).append(movement).toHashCode();
    }

    /**
     * Gets the movement ID.
     * 
     * @return The movement ID.
     */
    @Override
    public int getMovementId() {
        return movementId;
    }

    /**
     * Gets the movement.
     * 
     * @return The movement enumeration value.
     */
    @Override
    public Movement getMovement() {
        return movement;
    }

    /**
     * Sets the movement ID.
     * 
     * @param id The new ID.
     */
    public void setMovementId(int id) {
        movementId = id;
    }

    /**
     * Sets the movement.
     * 
     * @param m The new movement.
     */
    public void setMovement(Movement m) {
        movement = m;
    }

    /**
     * Checks if 2 lane movement ids are the same
     * 
     * @param entity The object to check against
     * @return True/False
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof ILaneMovement) {
            ILaneMovement d = (ILaneMovement)entity;
            return movementId == d.getMovementId();
        }
        else {
            return false;
        }
    }

    /**
     * Gets the proper if for the lane movement
     * 
     * @return The proper id
     */
    @Override
    public String getProperId() {
        StringBuilder sb = new StringBuilder("LaneMovement:");
        sb.append(movementId);
        return sb.toString();
    }
}