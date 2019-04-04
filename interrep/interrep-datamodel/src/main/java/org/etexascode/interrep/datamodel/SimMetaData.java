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

/**
 * Messaging class for passing the project's metadata from the etexas simulation up to the webapp.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
public class SimMetaData implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = 8702631056101998190L;

    /** The width of the simulation in centimeters. */
    @XmlElement
    double simWidth;

    /** The height of the simulation in centimeters. */
    @XmlElement
    double simHeight;

    /** The total amount of time steps this simulation will take. */
    @XmlElement
    long maxSteps;

    /** The first step in the simulation. */
    @XmlElement
    long firstStep = 0;

    /** The number of seconds per simulated step. */
    @XmlElement
    double stepSize;

    /**
     * Get the simulation's width.
     * 
     * @return The simulation's width
     */
    public double getSimWidth() {
        return simWidth;
    }

    /**
     * Set the simulation's width.
     * 
     * @param simWidth The simulation width to set.
     */
    public void setSimWidth(double simWidth) {
        this.simWidth = simWidth;
    }

    /**
     * Get the simulation's height.
     * 
     * @return The simulation's height
     */
    public double getSimHeight() {
        return simHeight;
    }

    /**
     * Set the simulation's height.
     * 
     * @param simHeight The simulation height to set.
     */
    public void setSimHeight(double simHeight) {
        this.simHeight = simHeight;
    }

    /**
     * Get the total number of steps in the simulation.
     * 
     * @return The total number of steps in the simulation.
     */
    public long getMaxSteps() {
        return maxSteps;
    }

    /**
     * Set the total number of steps in the simulation.
     * 
     * @param maxSteps The total number of steps in the simulation.
     */
    public void setMaxSteps(long maxSteps) {
        this.maxSteps = maxSteps;
    }

    /**
     * Get the step size of the simulation in seconds.
     * 
     * @return The step size of the simulation in seconds.
     */
    public double getStepSize() {
        return stepSize;
    }

    /**
     * Set the step size of the simulation in seconds.
     * 
     * @param stepSize The step size to set.
     */
    public void setStepSize(double stepSize) {
        this.stepSize = stepSize;
    }

    /**
     * Gets the first step of the simulation.
     * 
     * @return The first step.
     */
    public long getFirstStep() {
        return firstStep;
    }

    /**
     * Sets the first step of the simulation.
     * 
     * @param firstStep The first step to set.
     */
    public void setFirstStep(long firstStep) {
        this.firstStep = firstStep;
    }
}