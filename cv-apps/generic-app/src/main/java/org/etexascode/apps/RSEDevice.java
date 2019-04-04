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
import java.util.List;
import java.util.Map;

import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;

/**
 * A wrapper class for RSE devices.
 * 
 * @author ablatt
 * @author emyers
 */
public class RSEDevice {

    /** The relevant RSE information from the data layer. */
    private final RSEDeviceInfo info;

    /** The signal commands sent to the device. */
    final List<SignalCommand> commands = new ArrayList<SignalCommand>();

    /** The application messages to be sent from the device. */
    List<BasicMessage> appMessages = new ArrayList<BasicMessage>();

    /**
     * Constructs a new <code>RSEDevice</code> for the specified RSE device information.
     * 
     * @param device The RSE device information.
     */
    public RSEDevice(RSEDeviceInfo device) {

        info = device;
    }

    /**
     * Returns the signal manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The signal manager for the specified intersection.
     */
    public ISignalManager getSignalManager(Integer intersection) {

        return info.getSignalManager(intersection);
    }

    /**
     * Returns the signal managers for the supported intersections.
     * 
     * @return A map of signal managers for the supported intersections.
     */
    public Map<Integer, ISignalManager> getSignalManagers() {

        return info.getSignalManagers();
    }

    /**
     * Returns the lane manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The lane manager for the specified intersection.
     */
    public ILaneManager getLaneManager(Integer intersection) {

        return info.getLaneManager(intersection);
    }

    /**
     * Returns the lane managers for the supported intersections.
     * 
     * @return A map of lane managers for the supported intersections.
     */
    public Map<Integer, ILaneManager> getLaneManagers() {

        return info.getLaneManagers();
    }

    /**
     * Returns the detector manager for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @return The detector manager for the specified intersection.
     */
    public IDetectorManager getDetectorManager(Integer intersection) {

        return info.getDetectorManager(intersection);
    }

    /**
     * Returns the detector managers for the supported intersections.
     * 
     * @return A map of detector managers for the supported intersections.
     */
    public Map<Integer, IDetectorManager> getDetectorManagers() {

        return info.getDetectorManagers();
    }

    /**
     * Returns the reference points for the device.
     * 
     * @return The reference points for the device.
     */
    public ReferencePoint[] getReferencePoints() {

        return info.getReferencePoints();
    }

    /**
     * Adds a new signal command for the device.
     * 
     * @param command The signal command to add.
     */
    public void addSignalCommand(SignalCommand command) {

        commands.add(command);
    }

    /**
     * Returns the signal commands for the device. The method is for internal use and is not
     * intended for normal users.
     * 
     * @return The signal commands for the device.
     */
    public List<SignalCommand> getSignalCommands() {

        return commands;
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
     * Resets the messages for the device.
     */
    public void resetAppMessages() {

        appMessages = new ArrayList<BasicMessage>(0);
    }

    /**
     * Adds the specified messages to the device.
     * 
     * @param newMessages The list of messages to add.
     */
    public void addAppMessages(List<BasicMessage> newMessages) {

        appMessages.addAll(newMessages);
    }
}