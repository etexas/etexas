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
package org.etexascode.appslayerdata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;

/**
 * Remote proxy application that contain common data.
 * 
 * @author jrutherford
 * @author bbadillo
 */
public class RemoteProxyApp<T> implements IConnectedVehicleApp<T>, Serializable, IAppLifecycle, IAppName {

    /** Serial ID. */
    private static final long serialVersionUID = 4971313277390128641L;

    /** The vehicle commands the app is sending. */
    private final List<VehicleCommand> vehCommands = new ArrayList<VehicleCommand>();

    /** The signal commands the app is sending. */
    private final List<SignalCommand> sigCommands = new ArrayList<SignalCommand>();

    /** List of log data to send on update. */
    private final Map<String, String> logData = new HashMap<String, String>();

    /** List of messages to send on update. */
    private List<BasicMessage> messagesInjected = new ArrayList<BasicMessage>();

    /** The messages this device is receiving. */
    private List<BasicMessage> messages = new ArrayList<BasicMessage>();

    /** Default App ID. */
    private String appId = "RemoteApp";

    /** Proper ID. */
    private String properId = UUID.randomUUID().toString();

    /**
     * Gets the list of device messages. Must be synchronized with write operations because objects
     * of this class can be used between threads.
     * 
     * @return The list of messages for the device.
     */
    synchronized public List<BasicMessage> getMessages() {
        List<BasicMessage> retList = messages;
        messages = null;
        return retList;
    }

    /**
     * Place a command during the next time step.
     * 
     * @param command The command to send to the vehicle
     */
    public void addVehicleCommand(VehicleCommand command) {
        vehCommands.add(command);
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The commands being issued to the device this time step.
     */
    public List<VehicleCommand> getVehicleCommands() {
        return vehCommands;
    }

    /**
     * Add a signal command to the output of the app.
     * 
     * @param command The command to add to send to the device.
     */
    public void addSignalCommand(SignalCommand command) {
        sigCommands.add(command);
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The signal commands issued to the device this time step.
     */
    public List<SignalCommand> getSignalCommands() {
        return sigCommands;
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The map of log data for this time step.
     */
    public Map<String, String> getLogData() {
        return logData;
    }

    /**
     * Adds log data to the apps current time step.
     * 
     * @param key The key.
     * @param message The message.
     */
    public void addLogData(String key, String message) {
        logData.put(key, message);
    }

    /**
     * Getter. This is for internal use and not intended for normal users.
     * 
     * @return The list of messages injected.
     */
    public List<BasicMessage> getMessagesInjected() {
        return messagesInjected;
    }

    /**
     * Adds a message request to the current time step.
     * 
     * @param mr The message request.
     */
    public void addMessage(BasicMessage mr) {
        messagesInjected.add(mr);
    }

    /**
     * Discharges all the lists into an AppLayerOutput object and clears the originals.
     * 
     * @return AppLayerOutput object with all data to be injected.
     */
    @SuppressWarnings("unchecked")
    public AppLayerOutput discharge() {
        String str = "";
        long num = (long)0;
        DistanceImpl location = new DistanceImpl(0, 0, 0);
        AppLayerOutput alo = new AppLayerOutput(str, num, location);
        alo.outMessages = (ArrayList<BasicMessage>)((ArrayList<BasicMessage>)messagesInjected).clone();
        alo.vehCommands = (ArrayList<VehicleCommand>)((ArrayList<VehicleCommand>)vehCommands).clone();
        alo.sigCommands = (ArrayList<SignalCommand>)((ArrayList<SignalCommand>)sigCommands).clone();
        messagesInjected.clear();
        vehCommands.clear();
        sigCommands.clear();
        return alo;
    }

    @Override
    synchronized public void performUpdate(T device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        // NOTE: This method must be synchronized because objects of this class can be used between
        // threads.
        if (this.messages == null) {
            this.messages = new LinkedList<BasicMessage>(receive);
        }
        else {
            this.messages.addAll(receive);
        }
    }

    @Override
    public String getAppName() {
        return appId;
    }

    @Override
    public void init(String[] appConfigs) {
        // To get the appId remote apps should
        // pass in a REMOTE parameter with the app
        // id to be emulated.
        appId = appConfigs[0];
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    /**
     * Gets the proper ID.
     * 
     * @return the proper ID.
     */
    public String getProperId() {
        return properId;
    }
}