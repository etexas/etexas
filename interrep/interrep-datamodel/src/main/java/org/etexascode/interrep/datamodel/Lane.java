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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter;

/**
 * The model for a lane.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlSeeAlso({ LaneMovement.class, LaneNode.class })
public class Lane implements ILane {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -6255765316875701222L;

    /**
     * One of two identifiers for the lane type. Designates the lane as being an inbound lane.
     */
    @XmlTransient
    public static final String INBOUND = "INBOUND";

    /**
     * One of two identifiers for the lane type. Designates the lane as being an outbound lane.
     */
    @XmlTransient
    public static final String OUTBOUND = "OUTBOUND";

    /** The ID. */
    @XmlElement
    private int laneId;

    /** The approach ID. */
    @XmlElement
    private int approachId;

    /** The intersection ID. */
    @XmlElement
    private int intersectionId;

    /** The list of lane nodes. */
    @XmlElement
    protected List<LaneNode> laneGeomList;

    /** The map of lane movements. */
    @XmlElement
    @XmlJavaTypeAdapter(ManagerAdapter.class)
    private Map<Integer, LaneMovement> laneMovements;

    @XmlElement
    private double speedLimitInMetersPerSecond = 0.0;

    /** The type of lane. */
    @XmlElement
    protected String type; // TODO: ablatt - consider making this an enum type

    /** Empty Constructor. */
    public Lane() {
        laneId = 0;
        approachId = 0;
        laneGeomList = new ArrayList<LaneNode>();
        laneMovements = new HashMap<Integer, LaneMovement>();
        type = "Unset";
        intersectionId = 0;
    }

    /**
     * A constructor used to duplicate a lane.
     * 
     * @param lane The lane to duplicate.
     */
    public Lane(Lane lane) {

        this.laneId = lane.getLaneId();
        this.approachId = lane.getApproachId();
        this.laneGeomList = new ArrayList<LaneNode>();
        for (LaneNode node : lane.getLaneGeomList()) {

            this.laneGeomList.add(new LaneNode(node));
        }
        this.laneMovements = new HashMap<Integer, LaneMovement>();
        for (Entry<Integer, LaneMovement> entry : lane.getLaneMovements().entrySet()) {

            this.laneMovements.put(entry.getKey(), new LaneMovement(entry.getValue()));
        }
        this.type = lane.getType();
        this.intersectionId = lane.getIntersectionId();
    }

    /**
     * Add a lane node to the list of nodes.
     * 
     * @param toAdd The node to add.
     */
    public void addLaneNode(LaneNode toAdd) {
        laneGeomList.add(toAdd);
    }

    /** Checks if two lanes are equal. */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Lane) {
            Lane l = (Lane)obj;
            if (laneId != l.laneId || approachId != l.approachId || !type.equals(l.type) || intersectionId != l.intersectionId) {
                return false;
            }

            if (laneGeomList.size() != l.laneGeomList.size()) {
                return false;
            }

            Set<Integer> thisKeys = laneMovements.keySet();
            Set<Integer> lKeys = l.laneMovements.keySet();

            if (!(thisKeys.containsAll(lKeys) && lKeys.containsAll(thisKeys))) {
                return false;
            }

            for (int i = 0; i < laneGeomList.size(); i++) {

                if (!laneGeomList.contains(l.laneGeomList.get(i))) {
                    return false;
                }
            }

            for (Integer i : lKeys) {
                if (!laneMovements.get(i).equals(l.laneMovements.get(i))) {
                    return false;
                }
            }

            return true;
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
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(27, 5).append(laneId).append(approachId).append(intersectionId).append(type).toHashCode();
    }

    /**
     * Gets the approach ID.
     * 
     * @return The approach ID.
     */
    @Override
    public int getApproachId() {
        return approachId;
    }

    /**
     * Gets the intersection ID.
     * 
     * @return The intersection ID.
     */
    @Override
    public int getIntersectionId() {
        return intersectionId;
    }

    /**
     * Gets the lane geometry list.
     * 
     * @return The lane geometry list.
     */
    @Override
    public List<LaneNode> getLaneGeomList() {
        return laneGeomList;
    }

    /**
     * Gets the lane ID.
     * 
     * @return The lane ID.
     */
    @Override
    public int getLaneId() {
        return laneId;
    }

    /**
     * Gets the lane movements.
     * 
     * @return The map of lane movements.
     */
    @Override
    public Map<Integer, LaneMovement> getLaneMovements() {
        return laneMovements;
    }

    /**
     * Gets the speed limit of the lane (m/s).
     * 
     * @return The speed limit in m/s.
     */
    @Override
    public double getSpeedLimitInMetersPerSecond() {
        return speedLimitInMetersPerSecond;
    }

    /**
     * Gets the type of lane.
     * 
     * @return The type of lane.
     */
    @Override
    public String getType() {
        return type;
    }

    /**
     * Sets the approach ID.
     * 
     * @param approachId The new approach ID.
     */
    public void setApproachId(int approachId) {
        this.approachId = approachId;
    }

    /**
     * Sets the intersection ID.
     * 
     * @param intersectionId The new approach ID.
     */
    public void setIntersectionId(int intersectionId) {
        this.intersectionId = intersectionId;
    }

    /**
     * Sets the lane geometry list.
     * 
     * @param laneGeomList The new lane geometry list.
     */
    public void setLaneGeomList(List<LaneNode> laneGeomList) {
        this.laneGeomList = laneGeomList;
    }

    /**
     * Sets the lane ID.
     * 
     * @param laneId The new lane ID.
     */
    public void setLaneId(int laneId) {
        this.laneId = laneId;
    }

    /**
     * Sets the lane movements.
     * 
     * @param laneMovements The new lane movement map.
     */
    public void setLaneMovements(Map<Integer, LaneMovement> laneMovements) {
        this.laneMovements = laneMovements;
    }

    /**
     * Sets the speed limit of the lane (m/s).
     * 
     * @param speedLimitInMetersPerSecond The speed limit of the lane to set.
     */
    public void setSpeedLimitInMetersPerSecond(double speedLimitInMetersPerSecond) {
        this.speedLimitInMetersPerSecond = speedLimitInMetersPerSecond;
    }

    /**
     * Sets the type of lane.
     * 
     * @param type The new type of lane.
     */
    public void setType(String type) {
        this.type = type;
    }

    /** String representation of a lane. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        UtilsStringOnModel.addInt(ret, laneId, "lane id");
        UtilsStringOnModel.addInt(ret, approachId, "approach id");
        UtilsStringOnModel.addInt(ret, intersectionId, "intersection id");
        UtilsStringOnModel.addList(ret, laneGeomList, "lane geom list");
        UtilsStringOnModel.addMap(ret, laneMovements, "lane movements");
        UtilsStringOnModel.addString(ret, type, "type");
        return ret.toString();
    }

    /**
     * Gets the x coordinate for the lane from the geometry list
     * 
     * @return The x coordinate
     */
    @Override
    public double getX() {
        return laneGeomList.get(0).getX();
    }

    /**
     * Gets the y coordinate for the lane from the geometry list
     * 
     * @return The y coordinate
     */
    @Override
    public double getY() {
        return laneGeomList.get(0).getY();
    }

    /**
     * Gets the z coordinate for the lane from the geometry list
     * 
     * @return The z coordinate
     */
    @Override
    public double getZ() {
        return laneGeomList.get(0).getZ();
    }

    /**
     * Special iterator for lane nodes
     * 
     * @return An iterator across lane nodes
     */
    public Iterable<LaneNode> getIterable() {
        return laneGeomList;
    }

    /**
     * Special iterator for lane movements. The default for this object is an iterator across lane
     * nodes.
     * 
     * @return An iterator across this object's lane movements
     */
    @Override
    public Iterable<LaneMovement> lanMovIterator() {
        return laneMovements.values();
    }

    /**
     * Verifies that the lane id, approach id, and intersection id are the same
     * 
     * @return True/False they are the same
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof ILane) {
            ILane l = (ILane)entity;
            return (laneId == l.getLaneId()) && (approachId == l.getApproachId()) && (intersectionId == l.getIntersectionId());
        }
        else {
            return false;
        }
    }

    /**
     * Gets the proper id for the lane (LaneID:approachID:intersectionID)
     * 
     * @return The proper id for the lane
     */
    @Override
    public String getProperId() {
        StringBuilder sb = new StringBuilder("Lane:");
        sb.append(laneId);
        sb.append(":");
        sb.append(approachId);
        sb.append(":");
        sb.append(intersectionId);

        return sb.toString();
    }

    /**
     * Special iterator helper for lane nodes
     * 
     * @return The iterator helper
     */
    @CoberturaIgnore
    @Override
    public Iterator<ILaneNode> iterator() {
        return new IteratorHelper<ILaneNode, LaneNode>(laneGeomList.iterator());
    }
}