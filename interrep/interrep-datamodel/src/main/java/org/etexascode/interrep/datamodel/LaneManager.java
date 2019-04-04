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
import java.util.HashMap;
import java.util.Iterator;
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
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.Shiftable;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter;

/**
 * The lane manager model.
 * 
 * @author jrutherford
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlSeeAlso(value = Lane.class)
@XmlRootElement(name = "LaneManager")
public class LaneManager implements Serializable, ILaneManager, Shiftable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 7163537396158539634L;

    /** The latitude. */
    @XmlElement
    private double latitude;

    /** The longitude. */
    @XmlElement
    private double longitude;

    /** The elevation. */
    @XmlElement
    private double elevation;

    /** The map of lanes. */
    @XmlElement
    @XmlJavaTypeAdapter(ManagerAdapter.class)
    private Map<Integer, Lane> lanes;

    /** The id of the this intersections */
    @XmlElement
    private int intersectionId;

    /**
     * The calculator type to be used in latitude and longitude conversions. (Calculator type is
     * used by UtilsLatLongConversions for calculations)
     */
    @XmlElement
    private int geoCalculatorType = UtilsLatLongConversion.GEODETIC2D;

    /** Constructor. */
    public LaneManager() {
        latitude = 0.0;
        longitude = 0.0;
        elevation = 0.0;
        lanes = new HashMap<Integer, Lane>();
        intersectionId = 0;
    }

    /**
     * A constructor used to duplicate a lane manager.
     * 
     * @param laneManager The lane manager to duplicate.
     */
    public LaneManager(LaneManager laneManager) {

        this.elevation = laneManager.getElevation();
        this.geoCalculatorType = laneManager.getGeoCalculatorType();
        this.intersectionId = laneManager.getIntersectionId();
        this.lanes = new HashMap<Integer, Lane>();
        for (Entry<Integer, Lane> entry : laneManager.getLanes().entrySet()) {

            this.lanes.put(entry.getKey(), new Lane(entry.getValue()));
        }
        this.latitude = laneManager.getLatitude();
        this.longitude = laneManager.getLongitude();
    }

    /**
     * Check if 2 lane managers are equal.
     * 
     * @param obj The object to compare to.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LaneManager) {
            LaneManager lm = (LaneManager)obj;

            if (Double.compare(latitude, lm.latitude) != 0) {
                return false;
            }

            if (Double.compare(longitude, lm.longitude) != 0) {
                return false;
            }

            if (Double.compare(elevation, lm.elevation) != 0) {
                return false;
            }
            Set<Integer> thisKeys = lanes.keySet();
            Set<Integer> lmKeys = lm.lanes.keySet();

            if (!(thisKeys.containsAll(lmKeys) && lmKeys.containsAll(thisKeys))) {
                return false;
            }

            for (Integer i : lmKeys) {

                if (!lm.lanes.get(i).equals(lanes.get(i))) {
                    return false;
                }
            }

            return true;
        }
        return false;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @CoberturaIgnore
    @Override
    public int hashCode() {
        return new HashCodeBuilder(45, 23).append(latitude).append(longitude).append(elevation).toHashCode();
    }

    /**
     * Gets the elevation.
     * 
     * @return The elevation.
     */
    @Override
    public double getElevation() {
        return elevation;
    }

    /**
     * Gets the lane by ID.
     * 
     * @param id The ID.
     * @return The lane.
     */
    @Override
    public Lane getLaneById(int id) {
        return lanes.get(id);
    }

    /**
     * Gets the map of lanes.
     * 
     * @return The map of lanes.
     */
    @Override
    public Map<Integer, Lane> getLanes() {
        return lanes;
    }

    /**
     * Gets the latitude.
     * 
     * @return The latitude.
     */
    @Override
    public double getLatitude() {
        return latitude;
    }

    /**
     * Gets the longitude.
     * 
     * @return The longitude.
     */
    @Override
    public double getLongitude() {
        return longitude;
    }

    /**
     * Sets the elevation.
     * 
     * @param elevation The new elevation.
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    /**
     * Sets the map of lanes.
     * 
     * @param lanes The new lanes.
     */
    public void setLanes(Map<Integer, Lane> lanes) {
        this.lanes = lanes;
    }

    /**
     * Sets the latitude.
     * 
     * @param latitude The latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Sets the longitude.
     * 
     * @param longitude The longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Get the type of Calculator being used by the simulation.
     * 
     * @return the type of Calculator
     */
    @Override
    public int getGeoCalculatorType() {
        return geoCalculatorType;
    }

    /**
     * Set the type of Calculator being used by the simulation.
     * 
     * @param geoCalculatorType The geographic calculator type to set.
     */
    public void setGeoCalculatorType(int geoCalculatorType) {
        this.geoCalculatorType = geoCalculatorType;
    }

    /** String representation of the lane manager. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        UtilsStringOnModel.addDouble(ret, latitude, "latitude");
        UtilsStringOnModel.addDouble(ret, longitude, "longitude");
        UtilsStringOnModel.addDouble(ret, elevation, "elevation");
        UtilsStringOnModel.addMap(ret, lanes, "lanes");

        return ret.toString();
    }

    /**
     * Getter for the intersection id.
     * 
     * @return Get the id of the intersection.
     */
    @Override
    public int getIntersectionId() {
        return intersectionId;
    }

    /**
     * Setter for the intersection id.
     * 
     * @param intersectionId The new intersection id.
     */
    public void setIntersectionId(int intersectionId) {
        this.intersectionId = intersectionId;
    }

    /**
     * Special iterator for going through lanes in the manager.
     * 
     * @return The lanes in the manager.
     */
    public Iterable<Lane> getIterable() {
        return lanes.values();
    }

    /**
     * Checks if the lane manager id is unique.
     * 
     * @param entity Object to check against.
     * @return True/False the lane manager is unique.
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof ILaneManager) {
            ILaneManager d = (ILaneManager)entity;
            return intersectionId == d.getIntersectionId();
        }
        else {
            return false;
        }
    }

    /**
     * Get proper id for the lane manager.
     * 
     * @return The proper id for the lane manager.
     */
    @Override
    public String getProperId() {
        StringBuilder sb = new StringBuilder("LaneManager:");
        sb.append(intersectionId);
        return sb.toString();
    }

    /**
     * Gets the set of lane IDs.
     * 
     * @return The set of lane IDs
     */
    @CoberturaIgnore
    @Override
    public Set<Integer> getLaneIds() {
        return lanes.keySet();
    }

    /**
     * Special iterator helper parsing lanes.
     * 
     * @return The iterator helper.
     */
    @CoberturaIgnore
    @Override
    public Iterator<ILane> iterator() {
        return new IteratorHelper<ILane, Lane>(lanes.values().iterator());
    }

    @Override
    public void shift(double deltaX, double deltaY) {

        for (Lane lane : this.getIterable()) {
            for (LaneNode node : lane.getIterable()) {

                node.setX(node.getX() + deltaX);
                node.setY(node.getY() + deltaY);
            }
        }
    }
}