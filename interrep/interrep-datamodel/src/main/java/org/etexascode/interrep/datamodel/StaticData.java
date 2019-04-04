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
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

/**
 * The static simulation data model.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlSeeAlso({ DetectorManager.class, SignalManager.class, LaneManager.class, SimMetaData.class })
public class StaticData implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -2714571386151794764L;

    /** The detector manager. */
    @XmlElement
    DetectorManager detectorManager;

    /** The signal manager. */
    @XmlElement
    SignalManager signalManager;

    /** The lane manager. */
    @XmlElement
    LaneManager laneManager;

    /** The metadata. */
    @XmlElement
    SimMetaData metaData;

    /**
     * Gets the detector manager.
     * 
     * @return The detector manager.
     */
    public DetectorManager getDetectorManager() {
        return detectorManager;
    }

    /**
     * Gets the lane manager.
     * 
     * @return The lane manager.
     */
    public LaneManager getLaneManager() {
        return laneManager;
    }

    /**
     * Gets the metadata.
     * 
     * @return The metadata.
     */
    public SimMetaData getMetaData() {
        return metaData;
    }

    /**
     * Gets the signal manager.
     * 
     * @return The new signal manager.
     */
    public SignalManager getSignalManager() {
        return signalManager;
    }

    /**
     * Sets the detector manager.
     * 
     * @param detectorManager The new detector manager.
     */
    public void setDetectorManager(DetectorManager detectorManager) {
        this.detectorManager = detectorManager;
    }

    /**
     * Sets the lane manager.
     * 
     * @param laneManager The new lane manager.
     */
    public void setLaneManager(LaneManager laneManager) {
        this.laneManager = laneManager;
    }

    /**
     * Sets the metadata.
     * 
     * @param metaData The new metadata.
     */
    public void setMetaData(SimMetaData metaData) {
        this.metaData = metaData;
    }

    /**
     * Sets the signal manager.
     * 
     * @param signalManager The new signal manager.
     */
    public void setSignalManager(SignalManager signalManager) {
        this.signalManager = signalManager;
    }
}