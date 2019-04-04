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
package org.etexascode.test.restharness.templateclasses;

/**
 * Templating class for detectors.
 * 
 * @author ablatt
 */
public class DetectorInfo {

    /**
     * The distance from the stop line for this detector.
     */
    String dist;

    /**
     * The length dimension (in cm) of the detector.
     */
    String length;

    /**
     * The width dimension (in cm) of the detector.
     */
    String width;

    /**
     * The id of the lane this detector is in.
     */
    String laneId;

    /**
     * Generic Constructor.
     */
    public DetectorInfo() {
        dist = null;
        length = null;
        width = null;
        laneId = null;
    }

    /**
     * Constructor with all relevant information.
     * 
     * @param laneId The id of the lane this detector is in.
     * @param width The width dimension (in cm) of the detector.
     * @param length The length dimension (in cm) of the detector.
     * @param dist The distance from the stop line for this detector.
     */
    public DetectorInfo(int laneId, int width, int length, int dist) {
        this.dist = "" + dist;
        this.length = "" + length;
        this.width = "" + width;
        this.laneId = "" + laneId;
    }

    /**
     * Getter.
     * 
     * @return The distance from the stop line for this detector.
     */
    public String getDist() {
        return dist;
    }

    /**
     * Setter.
     * 
     * @param dist The distance from the stop line for this detector.
     */
    public void setDist(int dist) {
        this.dist = "" + dist;
    }

    /**
     * Getter.
     * 
     * @return The length dimension (in cm) of the detector.
     */
    public String getLength() {
        return length;
    }

    /**
     * Setter.
     * 
     * @param length The length dimension (in cm) of the detector.
     */
    public void setLength(int length) {
        this.length = "" + length;
    }

    /**
     * Getter.
     * 
     * @return The width dimension (in cm) of the detector.
     */
    public String getWidth() {
        return width;
    }

    /**
     * Setter.
     * 
     * @param width The width dimension (in cm) of the detector.
     */
    public void setWidth(int width) {
        this.width = "" + width;
    }

    /**
     * Getter.
     * 
     * @return The id of the lane this detector is in.
     */
    public String getLaneId() {
        return laneId;
    }

    /**
     * Setter.
     * 
     * @param laneId The id of the lane this detector is in.
     */
    public void setLaneId(int laneId) {
        this.laneId = "" + laneId;
    }
}
