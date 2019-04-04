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

package org.etexascode.nonstd;

import java.awt.Polygon;
import java.io.Serializable;

/**
 * Detector class to hold the information in each detector (this information is non-standard because
 * we do not have the specifications for the actually used detectors.
 * 
 * @author dranker
 * @author bbadillo
 */

public class SingleDetectorData extends Message implements Serializable {

    /** Detector id. */
    private int id;

    /** Whether or not this detector detects presence. */
    private boolean presence;

    /** The number of pulses this detector has detected. */
    // TODO: should this be boolean or integer - jconnelly
    private boolean pulse;

    /** Whether or not this detector was tripped. */
    private boolean tripped;

    /** Area of the detector. */
    private Polygon area = new Polygon();

    /**
     * Default constructor.
     */
    public SingleDetectorData() {}

    /**
     * Set the detector id.
     * 
     * @param id to set the detector to
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Get the detector id.
     * 
     * @return id The detector id.
     */
    public int getId() {
        return id;
    }

    /**
     * Get the pulse information from the detector.
     * 
     * @return pulse The detector pulse.
     */
    public boolean getPulse() {
        return pulse;
    }

    /**
     * Sets the pulse information for the detector.
     * 
     * @param pulse The pulse information to be added to detector.
     */
    public void setPulse(boolean pulse) {
        this.pulse = pulse;
    }

    /**
     * Gets whether or not there is a presence detected.
     * 
     * @return presence True/False a presence is detected.
     */
    public boolean isPresence() {
        return presence;
    }

    /**
     * Gets whether or not there is a presence detected.
     * 
     * @return presence True/False a presence is detected.
     */
    public boolean getPresence() {
        return presence;
    }

    /**
     * Sets whether or not a presence is detected.
     * 
     * @param presence True/False a presence is detected.
     */
    public void setPresence(boolean presence) {
        this.presence = presence;
    }

    /**
     * Sets the area of the detector.
     * 
     * @param area The area of the detector as a Polygon.
     */
    public void setArea(Polygon area) {
        this.area = area;
    }

    /**
     * Gets the area of the detector.
     * 
     * @return area The area of the detector as a Polygon.
     */
    public Polygon getArea() {
        return area;
    }

    /**
     * Determines if the detector was tripped.
     * 
     * @return tripped True/False the detector was tripped.
     */
    public boolean isTripped() {
        return tripped;
    }

    /**
     * Set whether the detector is tripped.
     * 
     * @param tripped True/False the detector was tripped.
     */
    public void setTripped(boolean tripped) {
        this.tripped = tripped;
    }

}
