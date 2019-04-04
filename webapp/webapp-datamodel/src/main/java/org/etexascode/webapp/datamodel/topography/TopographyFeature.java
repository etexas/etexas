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

import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.DiscriminatorType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.etexascode.webapp.datamodel.AbstractEntity;
import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * A topography feature.
 * 
 * @author ttevendale
 */
@Entity
@Table(name = "topography_features")
@XmlAccessorType(XmlAccessType.FIELD)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "type", discriminatorType = DiscriminatorType.STRING)
@XmlSeeAlso({ Building.class })
public abstract class TopographyFeature extends AbstractEntity implements ICopyable<TopographyFeature> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The string name of this feature. */
    protected String name;

    /** The topography type for this feature. */
    @Enumerated(EnumType.STRING)
    @Column(name = "type", insertable = false, updatable = false)
    private TopographyType type;

    /** The composite that owns this feature. */
    @Column(name = "composite", insertable = false, updatable = false)
    private Long composite;

    /** The x coordinate of this feature. */
    private double x;

    /** The y coordinate of this feature. */
    private double y;

    /**
     * Returns the name of this feature.
     * 
     * @return The string name of this feature.
     */
    public String getName() {

        return name;
    }

    /**
     * Sets the name of this feature.
     * 
     * @param name The string name to set.
     */
    public void setName(String name) {

        this.name = name;
    }

    /**
     * Returns the topography type for this feature.
     * 
     * @return The topography type for this feature.
     */
    public TopographyType getType() {

        return type;
    }

    /**
     * Returns the x coordinate (cm) of this building.
     * 
     * @return The double x coordinate (cm) of this building.
     */
    public double getX() {

        return x;
    }

    /**
     * Sets the x coordinate (cm) of this building.
     * 
     * @param x The double x coordinate (cm) to set.
     */
    public void setX(double x) {

        this.x = x;
    }

    /**
     * Returns the y coordinate (cm) of this building.
     * 
     * @return The double y coordinate (cm) of this building.
     */
    public double getY() {

        return y;
    }

    /**
     * Sets the y coordinate (cm) of this building.
     * 
     * @param y The double y coordinate (cm) to set.
     */
    public void setY(double y) {

        this.y = y;
    }

    @Override
    public final TopographyFeature copy() {
        TopographyFeature feature = this.specificCopy();

        if (feature == null) {

            throw new IllegalStateException("The specificCopy method cannot return a null value");
        }

        feature.setName(this.getName());
        feature.setX(this.getX());
        feature.setY(this.getY());

        return feature;
    }

    /**
     * Creates a copy of this specific topography type.
     * 
     * @return The new topography feature.
     */
    protected abstract TopographyFeature specificCopy();
}