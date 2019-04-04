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
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.CoberturaIgnore;

/**
 * The step data model.
 * 
 * @author ablatt
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement
@XmlSeeAlso({ DetectorEvent.class, SignalIndication.class, Vehicle.class, String.class })
public class StepData implements Serializable {

    /** Serial ID. */
    @XmlTransient
    private static final long serialVersionUID = -1285759446663941886L;

    /** List of detector events. */
    @XmlElement
    List<DetectorEvent> detectorEvents;

    /** List of signal indications. */
    @XmlElement
    List<SignalIndication> signalIndication;

    /** List of vehicles. */
    @XmlElement
    List<Vehicle> vehicles;

    /** List of simulator error messages. */
    @XmlElement
    List<String> errorMessages;

    /** Constructor. */
    public StepData() {
        detectorEvents = new ArrayList<DetectorEvent>();
        signalIndication = new ArrayList<SignalIndication>();
        vehicles = new ArrayList<Vehicle>();
        errorMessages = new ArrayList<String>();
    }

    /**
     * Adds a detector event.
     * 
     * @param de The detector event.
     */
    public void addDetectorEvent(DetectorEvent de) {
        detectorEvents.add(de);
    }

    /**
     * Adds a signal indication.
     * 
     * @param si The signal indication.
     */
    public void addSignalIndication(SignalIndication si) {
        signalIndication.add(si);
    }

    /**
     * Adds a vehicle to the list.
     * 
     * @param v The vehicle.
     */
    public void addVehicle(Vehicle v) {
        vehicles.add(v);
    }

    /**
     * Gets the list of detectors.
     * 
     * @return The list of detectors.
     */
    public List<DetectorEvent> getDetectorEvents() {
        return detectorEvents;
    }

    /**
     * Gets the list of signal indications.
     * 
     * @return The list of signal indications.
     */
    public List<SignalIndication> getSignalIndication() {
        return signalIndication;
    }

    /**
     * Gets the list of vehicles.
     * 
     * @return The list of vehicles.
     */
    public List<Vehicle> getVehicles() {
        return vehicles;
    }

    /**
     * Sets the list of vehicles.
     * 
     * @param vehicles The list of vehicles to set.
     */
    public void setVehicleList(List<Vehicle> vehicles) {
        this.vehicles = vehicles;
    }

    /**
     * Sets the list of signals.
     * 
     * @param signals The list of signals to set.
     */
    public void setSignalList(List<SignalIndication> signals) {
        this.signalIndication = signals;
    }

    /**
     * Adds error messages to the step data.
     * 
     * @param msg The error messages to add.
     */
    @CoberturaIgnore
    public void addErrorMessage(String msg) {
        this.errorMessages.add(msg);
    }

    /**
     * Gets a list of error messages from the step.
     * 
     * @return errorMessages The list of error messages.
     */
    public List<String> getErrorMessages() {
        return errorMessages;
    }
}