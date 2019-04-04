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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import org.etexascode.CoberturaIgnore;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * A container class for info classes to be used by devices.
 * 
 * @author ablatt
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class InterRepInfoModel {

    /**
     * LaneManager
     */
    @XmlElement(type = LaneManager.class)
    public final ILaneManager lmi;

    /**
     * VehicleManager
     */
    @XmlElement(type = VehicleManager.class)
    public final IVehicleManager vmi;

    /**
     * SignalManager
     */
    @XmlElement(type = SignalManager.class)
    public final ISignalManager smi;

    /**
     * DetectorManager
     */
    @XmlElement(type = DetectorManager.class)
    public final IDetectorManager dmi;

    /**
     * ReferencePoints
     */
    @XmlElement
    @XmlElementWrapper(name = "rpa")
    public final ReferencePoint[] rpa;

    /**
     * Sim Time
     */
    @XmlElement
    public final Double simTime;

    /**
     * Time Step Interval
     */
    @XmlElement
    public final Double timeStepInterval;

    /**
     * Constructor
     * 
     * @param lanMan The current lane geometry.
     * @param vehMan The current vehicle locations.
     * @param sigMan The current signal states.
     * @param detMan The current detector manager.
     * @param points The reference points.
     * @param time The current time.
     * @param timeStep The time step.
     */
    @CoberturaIgnore
    public InterRepInfoModel(ILaneManager lanMan, IVehicleManager vehMan, ISignalManager sigMan, IDetectorManager detMan, ReferencePoint[] points, Double time, Double timeStep) {
        lmi = lanMan;
        vmi = vehMan;
        smi = sigMan;
        dmi = detMan;
        rpa = points.clone();
        simTime = time;
        timeStepInterval = timeStep;
    }

    /**
     * Default constructor for jaxb.
     */
    InterRepInfoModel() {
        lmi = null;
        vmi = null;
        smi = null;
        dmi = null;
        rpa = null;
        simTime = null;
        timeStepInterval = null;
    }

    /**
     * Gets the lane manager for the intersection
     * 
     * @return lmi The lane manager
     */
    public ILaneManager getLmi() {
        return this.lmi;
    }

    /**
     * Gets the signal manager for the intersection
     * 
     * @return smi The signal manager
     */
    public ISignalManager getSmi() {
        return this.smi;
    }

    /**
     * Gets the detector manager for the intersection
     * 
     * @return dmi The detector manager
     */

    public IDetectorManager getDmi() {
        return this.dmi;
    }

    /**
     * Gets the vehicle manager for the intersection
     * 
     * @return vmi The vehicle manager
     */

    public IVehicleManager getVmi() {
        return this.vmi;
    }

    /**
     * Gets the time step interval for the simulation
     * 
     * @return timeStepInterval time step interval
     */
    public Double getTimeStepInterval() {
        return timeStepInterval;
    }
}
