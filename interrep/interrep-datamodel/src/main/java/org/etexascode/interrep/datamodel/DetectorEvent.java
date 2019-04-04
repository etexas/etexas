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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorEvent;

/**
 * Detector event model.
 * 
 * @author unknown
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class DetectorEvent implements Serializable, IDetectorEvent {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 3045420162460601805L;

    /** The detector ID. */
    @XmlElement
    int detectorId = 0;

    /** The pulse. */
    @XmlElement
    int pulse = 0;

    /** The presence. */
    @XmlElement
    boolean presence = false;

    /** The speed. */
    @XmlElement
    double speed = 0.0;

    /** The length. */
    @XmlElement
    double length = 0.0;

    /** Gets the string representation. */
    @Override
    public String toString() {
        StringBuilder ret = new StringBuilder();

        ret.append("detectorId = ");
        ret.append(detectorId);
        ret.append("\n");
        ret.append("pulse = ");
        ret.append(pulse);
        ret.append("\n");
        ret.append("presence = ");
        ret.append(presence);
        ret.append("\n");
        ret.append("speed = ");
        ret.append(speed);
        ret.append("\n");
        ret.append("length = ");
        ret.append(length);

        return ret.toString();
    }

    /**
     * Check if de is the same as this.
     * 
     * @param object The DetectorEvent to test against.
     * @return If the object are equal.
     */
    @Override
    public boolean equals(Object object) {
        if (object instanceof DetectorEvent) {
            DetectorEvent de = (DetectorEvent)object;

            return detectorId == de.detectorId &&
                    pulse == de.pulse &&
                    presence == de.presence &&
                    closeEnough(speed, de.speed) &&
                    closeEnough(length, de.length);
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
        return new HashCodeBuilder(55, 7).append(detectorId).append(pulse).append(presence).append(speed).append(length).toHashCode();
    }

    /**
     * Simple check to see if d1 is close enough to d2 given floating point arithmetic.
     * 
     * @param d1
     * @param d2
     * @return See if the 2 values are approximate.
     */
    private boolean closeEnough(double d1, double d2) {
        return ((d1 - d2) <= 0.05) && ((d1 - d2) >= -0.05);
    }

    /**
     * Gets the detector ID.
     * 
     * @return The detector ID.
     */
    @Override
    public int getDetectorId() {
        return detectorId;
    }

    /**
     * Sets the detector ID.
     * 
     * @param detectorId The detector ID.
     */
    public void setDetectorId(int detectorId) {
        this.detectorId = detectorId;
    }

    /**
     * Gets the pulse.
     * 
     * @return The pulse.
     */
    @Override
    public int getPulse() {
        return pulse;
    }

    /**
     * Sets the pulse.
     * 
     * @param pulse The new pulse.
     */
    public void setPulse(int pulse) {
        this.pulse = pulse;
    }

    /**
     * Is presence?
     * 
     * @return True/False
     */
    @Override
    public boolean isPresence() {
        return presence;
    }

    /**
     * Sets the presence.
     * 
     * @param presence True/False
     */
    public void setPresence(boolean presence) {
        this.presence = presence;
    }

    /**
     * Gets the speed.
     * 
     * @return The speed.
     */
    @Override
    public double getSpeed() {
        return speed;
    }

    /**
     * Sets the speed.
     * 
     * @param speed The new speed.
     */
    public void setSpeed(double speed) {
        this.speed = speed;
    }

    /**
     * Gets the length.
     * 
     * @return The length.
     */
    @Override
    public double getLength() {
        return length;
    }

    /**
     * Sets the length.
     * 
     * @param length The new length.
     */
    public void setLength(double length) {
        this.length = length;
    }

    /**
     * Equality check of detector event id.
     * 
     * @return True/False
     */
    @Override
    public boolean equalsId(IDable entity) {
        if (entity instanceof IDetectorEvent) {
            IDetectorEvent d = (IDetectorEvent)entity;
            return detectorId == d.getDetectorId();
        }
        else {
            return false;
        }
    }

    /**
     * Gets the proper id of the detector event.
     * 
     * @return The proper id
     */
    @Override
    public String getProperId() {
        StringBuilder sb = new StringBuilder("DetectorEvent:");
        sb.append(detectorId);
        return sb.toString();
    }
}