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
package org.etexascode.webapp.rest.model;

import java.io.Serializable;

/**
 * THe simulation configuration options data.
 * 
 * @author jrutherford
 */
public class SimConfigData implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 2445388879879283570L;

    /** Communication Model. */
    String commModel;

    /** Latitude Coordinate. */
    Double latitude;

    /** Longitude Coordinate. */
    Double longitude;

    /** Coordinate System. */
    String coorSys;

    /** Coverage Percentage. */
    String coverage;

    /**
     * Gets the communication model.
     * 
     * @return The communication model.
     */
    public String getCommModel() {
        return commModel;
    }

    /**
     * Sets the communication model.
     * 
     * @param commModel The new communication model.
     */
    public void setCommModel(String commModel) {
        this.commModel = commModel;
    }

    /**
     * Gets the latitude coordinate.
     * 
     * @return The latitude.
     */
    public Double getLatitude() {
        return latitude;
    }

    /**
     * Sets the latitude coordinate.
     * 
     * @param latitude The new latitude.
     */
    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    /**
     * Gets the longitude coordinate.
     * 
     * @return The longitude coordinate.
     */
    public Double getLongitude() {
        return longitude;
    }

    /**
     * Sets the longitude coordinate.
     * 
     * @param longitude The new longitude.
     */
    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    /**
     * Gets the coordinate system.
     * 
     * @return The coordinate system.
     */
    public String getCoorSys() {
        return coorSys;
    }

    /**
     * Sets the coordinate system.
     * 
     * @param coorSys The new coordinate system.
     */
    public void setCoorSys(String coorSys) {
        this.coorSys = coorSys;
    }

    /**
     * Gets the coverage percentage.
     * 
     * @return The coverage amount.
     */
    public String getCoverage() {
        return coverage;
    }

    /**
     * Sets the coverage percent.
     * 
     * @param coverage New coverage amount.
     */
    public void setCoverage(String coverage) {
        this.coverage = coverage;
    }
}