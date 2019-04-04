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
package org.etexascode.webapp.datamodel.topography;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A building topography feature.
 * 
 * @author ttevendale
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@DiscriminatorValue(TopographyType.Discriminator.BUILDING)
public class Building extends TopographyFeature {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The width of this building */
    private double width;

    /** The length of this building */
    private double length;

    /** The height of this building */
    private double height;

    /**
     * Returns the width (cm) of this building.
     * 
     * @return The width (cm) of this building.
     */
    public double getWidth() {
        return width;
    }

    /**
     * Sets the width (cm) of this building.
     * 
     * @param width The width (cm) to set.
     */
    public void setWidth(double width) {
        this.width = width;
    }

    /**
     * Returns the length (cm) of this building.
     * 
     * @return The length (cm) of this building.
     */
    public double getLength() {
        return length;
    }

    /**
     * Sets the length (cm) of this building.
     * 
     * @param length The length (cm) to set.
     */
    public void setLength(double length) {
        this.length = length;
    }

    /**
     * Returns the height (cm) of this building.
     * 
     * @return The height (cm) of this building.
     */
    public double getHeight() {
        return height;
    }

    /**
     * Sets the height (cm) of this building.
     * 
     * @param height The height (cm) to set.
     */
    public void setHeight(double height) {
        this.height = height;
    }

    @Override
    protected TopographyFeature specificCopy() {
        Building feature = new Building();
        feature.setWidth(this.getWidth());
        feature.setLength(this.getLength());
        feature.setHeight(this.getHeight());

        return feature;
    }

}