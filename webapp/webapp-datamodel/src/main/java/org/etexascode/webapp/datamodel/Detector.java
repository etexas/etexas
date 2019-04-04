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
package org.etexascode.webapp.datamodel;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.webapp.datamodel.util.ICopyable;

/**
 * An inductive-loop detector.
 * 
 * @author ablatt
 * @author emyers
 */
@Entity
@Table(name = "detectors")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Detector extends AbstractEntity implements ICopyable<Detector> {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The width (cm) of this detector. */
    private double width;

    /** The height (cm) of this detector. */
    private double height;

    /** The distance (cm) of this detector from the stop line. */
    private double distance;

    /** The ID of the lane where this detector is located. */
    private int lane;

    /**
     * Returns the width (cm) of this detector.
     * 
     * @return The double width (cm) of this detector.
     */
    public double getWidth() {

        return width;
    }

    /**
     * Sets the width (cm) of this detector.
     * 
     * @param width The double width (cm) to set.
     */
    public void setWidth(double width) {

        this.width = width;
    }

    /**
     * Returns the height (cm) of this detector.
     * 
     * @return The double height (cm) of this detector.
     */
    public double getHeight() {

        return height;
    }

    /**
     * Sets the height (cm) of this detector.
     * 
     * @param height The double height (cm) to set.
     */
    public void setHeight(double height) {

        this.height = height;
    }

    /**
     * Returns the distance (cm) of this detector from the stop line.
     * 
     * @return The double distance (cm) of this detector from the stop line.
     */
    public double getDistance() {

        return distance;
    }

    /**
     * Sets the distance (cm) of this detector from the stop line.
     * 
     * @param distance The double distance (cm) from the stop line to set.
     */
    public void setDistance(double distance) {

        this.distance = distance;
    }

    /**
     * Returns the ID of the lane where this detector is located.
     * 
     * @return The integer ID of the lane where this detector is located.
     */
    public int getLane() {

        return lane;
    }

    /**
     * Sets the ID of the lane where this detector is located.
     * 
     * @param lane The integer ID of the lane to set.
     */
    public void setLane(int lane) {

        this.lane = lane;
    }

    /**
     * The method to copy this Detector
     * 
     * @return The copied Detector
     */
    @Override
    public Detector copy() {
        Detector detector = new Detector();
        detector.setDistance(this.getDistance());
        detector.setHeight(this.getHeight());
        detector.setLane(this.getLane());
        detector.setWidth(this.getWidth());

        return detector;
    }
}