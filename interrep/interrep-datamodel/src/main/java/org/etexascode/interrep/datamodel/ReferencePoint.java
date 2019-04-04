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

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;

/**
 * ReferencePoint is a class which wraps all the data inherent in a reference point.
 * 
 * @author ablatt
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class ReferencePoint implements Serializable {

    /** Serial ID. */
    private static final long serialVersionUID = 6393676216728938069L;

    /**
     * The latitude of the reference point.
     */
    @XmlElement
    private double latitude;

    /**
     * The longitude of the reference point.
     */
    @XmlElement
    private double longitude;

    /**
     * Simple constructor which sets the reference point to (0, 0). (Not recommended but available)
     */
    public ReferencePoint() {
        latitude = 0.0;
        longitude = 0.0;
    }

    /**
     * Constructor which creates the reference point using the given parameters.
     * 
     * @param latitude The latitude of the new reference point
     * @param longitude The longitude of the new reference point
     */
    public ReferencePoint(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Get the latitude of the reference point
     * 
     * @return latitude The latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Set the latitude of the reference point.
     * 
     * @param latitude The latitude to set.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Get the longitude of the reference point
     * 
     * @return longitude The longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Set the longitude of the reference point.
     * 
     * @param longitude The longitude to set.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Check if object is the same as this.
     * 
     * @param object The object to test against.
     * @return If the values are equal.
     */
    @Override
    public boolean equals(Object object) {
        if (object instanceof ReferencePoint) {
            ReferencePoint rp = (ReferencePoint)object;

            if (this.latitude != rp.getLatitude()) {
                return false;
            }
            if (this.longitude != rp.getLongitude()) {
                return false;
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
        return new HashCodeBuilder(7, 71).append(latitude).append(longitude).toHashCode();
    }
}
