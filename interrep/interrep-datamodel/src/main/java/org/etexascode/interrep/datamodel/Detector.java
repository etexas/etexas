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

import java.awt.Polygon;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.interrep.datamodel.xmladapters.PolygonAdapter;

/**
 * The detector model.
 * 
 * @author unknown
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlSeeAlso(DetectorEvent.class)
public class Detector implements Serializable, IDetector {

    /** Serial ID */
    @XmlTransient
    private static final long serialVersionUID = 2999196670024338084L;

    /** Detector ID. */
    @XmlElement
    private int detectorID;

    /** List of lane IDs. */
    @XmlElement
    private List<Integer> laneIDs;

    /** Is presence detector. */
    @XmlElement
    private boolean presenceDetectCap;

    /** Is pulse detector. */
    @XmlElement
    private boolean pulseDetectCap;

    /** Is speed detector. */
    @XmlElement
    private boolean speedDetectCap;

    /** Is length detector. */
    @XmlElement
    private boolean lengthDetectCap;

    /** The detector event. */
    @XmlElement
    private DetectorEvent detEvent;

    /** The area of the detector. */
    @XmlJavaTypeAdapter(PolygonAdapter.class)
    @XmlElement
    private Polygon area;

    /** Constructor. */
    public Detector() {
        detectorID = 0;
        laneIDs = new ArrayList<Integer>();
        presenceDetectCap = false;
        pulseDetectCap = false;
        speedDetectCap = false;
        lengthDetectCap = false;
        detEvent = null;
        area = new Polygon();
    }

    /**
     * Clone the area field of this class.
     * 
     * @return The clone of the area.
     */
    public Polygon cloneArea() {
        return SerializationUtils.clone(area);
    }

    /**
     * Standard equality operator for the detector.
     * 
     * @param obj The detector to check this detector against.
     * @return Is this detector the same as d?
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Detector) {
            Detector d = (Detector)obj;
            if (!(detectorID == d.detectorID)) {
                return false;
            }
            else if (!(presenceDetectCap == d.presenceDetectCap)) {
                return false;
            }
            else if (!(pulseDetectCap == d.pulseDetectCap)) {
                return false;
            }
            else if (!(speedDetectCap == d.speedDetectCap)) {
                return false;
            }
            else if (!(lengthDetectCap == d.lengthDetectCap)) {
                return false;
            }
            else if (!(laneIDs.size() == d.laneIDs.size())) {
                return false;
            }
            else if (!laneIDs.containsAll(d.laneIDs)) {
                return false;
            }
            else if (!d.laneIDs.containsAll(laneIDs)) {
                return false;
            }
            else if (!UtilsSpecialEquals.equals(area, d.getArea())) {
                return false;
            }
            // if the detEvent is null or empty set equal. JaxB will write an empty event if it's
            // null.
            if (detEvent == null || detEvent.detectorId == 0) {
                if (d.detEvent == null || d.detEvent.detectorId == 0) {
                    return true;
                }
                else {
                    return false;
                }
            }
            else if (!detEvent.equals(d.detEvent)) {
                return false;
            }

            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Clone the detector of this class.
     * 
     * @param d The detector to clone.
     * @return ret The clone of the detector.
     */
    public static Detector cloneDetector(Detector d) {
        if (d == null) {
            return null;
        }

        Detector ret = new Detector();

        ret.detectorID = d.detectorID;
        ret.laneIDs = d.laneIDs;
        ret.presenceDetectCap = d.presenceDetectCap;
        ret.pulseDetectCap = d.pulseDetectCap;
        ret.speedDetectCap = d.speedDetectCap;
        ret.lengthDetectCap = d.lengthDetectCap;
        ret.detEvent = SerializationUtils.clone(d.detEvent);
        ret.area = d.cloneArea();

        return ret;
    }

    /**
     * HashCode operation.
     * 
     * @return A hashcode of the data.
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder(63, 17).append(detectorID).append(detEvent).toHashCode();
    }

    /**
     * Gets the detector ID.
     * 
     * @return Detector ID.
     */
    @Override
    public int getDetectorID() {
        return detectorID;
    }

    /**
     * Sets the detector ID.
     * 
     * @param detectorID The new detector ID.
     */
    public void setDetectorID(int detectorID) {
        this.detectorID = detectorID;
    }

    /**
     * Gets the lane IDs.
     * 
     * @return List of lane ID's.
     */
    @Override
    public List<Integer> getLaneIDs() {
        return laneIDs;
    }

    /**
     * Sets the lane IDs.
     * 
     * @param laneIDs The new list of lane IDs.
     */
    public void setLaneIDs(List<Integer> laneIDs) {
        this.laneIDs = laneIDs;
    }

    /**
     * Is this a presence detector.
     * 
     * @return True/False
     */
    @Override
    public boolean isPresenceDetectCap() {
        return presenceDetectCap;
    }

    /**
     * Set if this is a presence detector.
     * 
     * @param presenceDetectCap True/False
     */
    public void setPresenceDetectCap(boolean presenceDetectCap) {
        this.presenceDetectCap = presenceDetectCap;
    }

    /**
     * Is this a pulse detector.
     * 
     * @return True/False
     */
    @Override
    public boolean isPulseDetectCap() {
        return pulseDetectCap;
    }

    /**
     * Set if this is a pulse detector.
     * 
     * @param pulseDetectCap True/False
     */
    public void setPulseDetectCap(boolean pulseDetectCap) {
        this.pulseDetectCap = pulseDetectCap;
    }

    /**
     * Is this a speed detector.
     * 
     * @return True/False
     */
    @Override
    public boolean isSpeedDetectCap() {
        return speedDetectCap;
    }

    /**
     * Sets if this is a speed detector.
     * 
     * @param speedDetectCap True/False
     */
    public void setSpeedDetectCap(boolean speedDetectCap) {
        this.speedDetectCap = speedDetectCap;
    }

    /**
     * Is this a length detector.
     * 
     * @return True/False
     */
    @Override
    public boolean isLengthDetectCap() {
        return lengthDetectCap;
    }

    /**
     * Sets if this is a length detector.
     * 
     * @param lengthDetectCap True/False
     */
    public void setLengthDetectCap(boolean lengthDetectCap) {
        this.lengthDetectCap = lengthDetectCap;
    }

    /**
     * Gets the detector event.
     * 
     * @return The detector event.
     */
    @Override
    public DetectorEvent getDetEvent() {
        return detEvent;
    }

    /**
     * Sets the detector event.
     * 
     * @param detEvent The new detector event.
     */
    public void setDetEvent(DetectorEvent detEvent) {
        this.detEvent = detEvent;
    }

    /**
     * Gets the area of the detector.
     * 
     * @return The area.
     */
    @Override
    public Polygon getArea() {
        return area;
    }

    /**
     * Sets the area of the detector.
     * 
     * @param area The new area.
     */
    public void setArea(Polygon area) {
        this.area = area;
    }

    /** The string representation of the detector. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("Detector Id = ");
        ret.append(detectorID);
        ret.append("\n");

        ret.append("Presence Detection Cap = ");
        ret.append(presenceDetectCap);
        ret.append("\n");

        ret.append("Pulse Detect Cap = ");
        ret.append(pulseDetectCap);
        ret.append("\n");

        ret.append("Speed Detect Cap = ");
        ret.append(speedDetectCap);
        ret.append("\n");

        ret.append("Length Detect Cap = ");
        ret.append(lengthDetectCap);
        ret.append("\n");

        ret.append(laneIDsString());
        ret.append(UtilsStringOnModel.buildPolygonString(area));

        if (detEvent != null) {
            ret.append("Current Event:\n");
            ret.append(detEvent.toString());
            ret.append("\n");
        }
        else {
            ret.append("Current Event:\nnull\n");
        }

        return ret.toString();
    }

    /**
     * Gets the lane IDs string.
     * 
     * @return The lane IDs to string.
     */
    private String laneIDsString() {
        StringBuilder ret = new StringBuilder("Lane IDs = {");

        for (int i = 0; i < laneIDs.size(); i++) {
            ret.append(laneIDs.get(i).toString());
            if (i != laneIDs.size() - 1) {
                ret.append(", ");
            }
        }
        ret.append("}\n");

        return ret.toString();
    }

    /**
     * Gets the x coordinates for the detector area
     * 
     * @return The x points
     */
    @Override
    public double getX() {
        double ret = 0.0;
        for (int i = 0; i < area.npoints; i++) {
            ret += area.xpoints[i];
        }

        return ret / area.npoints;
    }

    /**
     * Gets the y coordinates for the detector area
     * 
     * @return The y points
     */
    @Override
    public double getY() {
        double ret = 0.0;
        for (int i = 0; i < area.npoints; i++) {
            ret += area.ypoints[i];
        }

        return ret / area.npoints;
    }

    /**
     * Gets the z coordinates for the detector area
     * 
     * @return The z points
     */
    @Override
    public double getZ() {
        return 0; // TODO: ablatt - what should this be?
    }

    /**
     * Standard equality operator for the detector id.
     * 
     * @param entity The entity to check against this detector
     * @return True/False the ids are the same
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof IDetector) {
            IDetector d = (IDetector)entity;
            return detectorID == d.getDetectorID();
        }
        else {
            return false;
        }
    }

    /**
     * Gets the proper id of the detector
     * 
     * @return The proper id for the detector
     */
    @Override
    public String getProperId() {
        StringBuilder sb = new StringBuilder("Detector:");
        sb.append(detectorID);
        return sb.toString();
    }
}