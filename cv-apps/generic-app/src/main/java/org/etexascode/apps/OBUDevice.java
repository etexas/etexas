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

import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * A wrapper class for obu devices
 * 
 * @author ablatt
 */
public class OBUDevice {

    /**
     * The information of the device we are wrapping
     */
    private final OBUDeviceInfo devInf;

    /**
     * The commands the app is sending to the vehicle
     */
    private final List<VehicleCommand> outputCommands = new ArrayList<VehicleCommand>();

    /** Messages from apps on this device */
    List<BasicMessage> appMessages = new ArrayList<BasicMessage>();

    /**
     * Constructor
     * 
     * @param device The device to wrap
     */
    public OBUDevice(OBUDeviceInfo device) {
        devInf = device;
    }

    /**
     * Getter
     * 
     * @return The vehicle according to the device.
     */
    public IVehicle getVehicleInfo() {
        return devInf.getVehicle();
    }

    /**
     * Place a command to be given to the vehicle during the next time step
     * 
     * @param command The command to send to the vehicle
     */
    public void addVehicleCommand(VehicleCommand command) {
        outputCommands.add(command);
    }

    /**
     * Get the mac for this device
     * 
     * @return The mac
     */
    public long getMac() {
        return devInf.getDeviceId();
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The commands being issued to the device this time step.
     */
    public List<VehicleCommand> getOutputCommands() {
        return outputCommands;
    }

    /**
     * Getter for messages from this device
     * 
     * @return app messages
     */
    public List<BasicMessage> getAppMessages() {
        return appMessages;
    }

    /**
     * Deletes all old app messages
     */
    public void resetAppMessages() {
        appMessages = new ArrayList<BasicMessage>(0);
    }

    /**
     * Adds messages from an app to the Message output for this device
     * 
     * @param newMessages The new messages.
     */
    public void addAppMessages(List<BasicMessage> newMessages) {
        appMessages.addAll(newMessages);
    }
}
