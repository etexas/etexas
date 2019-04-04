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
package org.etexascode.apps;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * A wrapper class for report devices.
 * 
 * @author ablatt
 * @author emyers
 */
public class ReportDevice {

    /** The relevant report device information from the data layer. */
    private final ReportDeviceInfo device;

    /** The application messages to be sent from the device. */
    List<BasicMessage> appMessages = new ArrayList<BasicMessage>();

    /**
     * Constructs a new <code>ReportDevice</code> for the specified report device information.
     * 
     * @param device The report device information.
     */
    public ReportDevice(ReportDeviceInfo device) {

        this.device = device;
    }

    /**
     * Returns the signal manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The signal manager for the specified intersection.
     */
    public ISignalManager getSignalManager(int intersection) {

        return device.getSignalManager(intersection);
    }

    /**
     * Returns the signal managers for the supported intersections.
     * 
     * @return A map of signal managers for the supported intersections.
     */
    public Map<Integer, ISignalManager> getSignalManagers() {

        return device.getSignalManagers();
    }

    /**
     * Returns the lane manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The lane manager for the specified intersection.
     */
    public ILaneManager getLaneManager(int intersection) {

        return device.getLaneManager(intersection);
    }

    /**
     * Returns the lane managers for the supported intersections.
     * 
     * @return A map of lane managers for the supported intersections.
     */
    public Map<Integer, ILaneManager> getLaneManagers() {

        return device.getLaneManagers();
    }

    /**
     * Returns the detector manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The detector manager for the specified intersection.
     */
    public IDetectorManager getDetectorManager(int intersection) {

        return device.getDetectorManager(intersection);
    }

    /**
     * Returns the detector managers for the supported intersections.
     * 
     * @return A map of detector managers for the supported intersections.
     */
    public Map<Integer, IDetectorManager> getDetectorManagers() {

        return device.getDetectorManagers();
    }

    /**
     * Returns the vehicle manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The vehicle manager for the specified intersection.
     */
    public IVehicleManager getVehicleManager(int intersection) {

        return device.getVehicleManager(intersection);
    }

    /**
     * Returns the vehicles managers for the supported intersections.
     * 
     * @return A map of vehicle managers for the supported intersections.
     */
    public Map<Integer, IVehicleManager> getVehicleManagers() {

        return device.getVehicleManagers();
    }

    /**
     * Returns the messages for the device.
     * 
     * @return The list of messages for the device.
     */
    public List<BasicMessage> getAppMessages() {
        return appMessages;
    }

    /**
     * Returns the transmitted messages for the device.
     * 
     * @return The list of transmitted messages for the device.
     */
    public Collection<BasicMessage> getTxMessages() {

        return device.getTxMessages();
    }

    /**
     * Adds the specified messages to the device.
     * 
     * @param newMessages The list of messages to add.
     */
    public void addAppMessages(List<BasicMessage> newMessages) {

        appMessages.addAll(newMessages);
    }

    /**
     * Resets the messages for the device.
     */
    public void resetAppMessages() {

        appMessages = new ArrayList<BasicMessage>(0);
    }
}
