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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.etexascode.CoberturaIgnore;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.LogData;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.utils.UtilsSpecialEquals;
import org.etexascode.wavesim.Tx.MessageType;

/**
 * The output of an app to the apps layer.
 * 
 * @author ablatt
 * @author ttevendale
 */
public class AppLayerOutput {

    /** The MAC of the device the application is running on. */
    public final long mac;

    /** The id of the application */
    public final String appName;

    /** The location of the application when this output was created. */
    public final IDistanceable location;

    /** The vehicle commands output by the application. */
    List<VehicleCommand> vehCommands;

    /** The signal commands output by the application. */
    List<SignalCommand> sigCommands;

    /** The messages output by the application. */
    List<BasicMessage> outMessages;

    /** The logs output by the application. */
    List<LogData> logs;

    /** The message type that the apps are using */
    private MessageType messageType;

    /**
     * Constructor.
     *
     * @param appName the app name
     * @param mac the mac
     * @param location the location
     */
    public AppLayerOutput(String appName, long mac, IDistanceable location) {
        this.mac = mac;
        this.appName = appName;
        this.location = location;
        vehCommands = new ArrayList<VehicleCommand>();
        sigCommands = new ArrayList<SignalCommand>();
        outMessages = new ArrayList<BasicMessage>();
        logs = new ArrayList<LogData>();
        this.messageType = null;
    }

    /**
     * Instantiates a new app layer output.
     *
     * @param appName the app name
     * @param mac the mac
     * @param location the location
     * @param vehCommands the veh commands
     * @param sigCommands the sig commands
     * @param outMessages the out messages
     * @param logs the logs
     * @param messageType the type of the message(dsrc, cellular)
     */
    public AppLayerOutput(String appName, long mac, IDistanceable location, List<VehicleCommand> vehCommands, List<SignalCommand> sigCommands, List<BasicMessage> outMessages, List<LogData> logs,
            MessageType messageType) {
        this.mac = mac;
        this.appName = appName;
        this.location = location;
        this.vehCommands = vehCommands;
        this.sigCommands = sigCommands;
        this.outMessages = outMessages;
        this.logs = logs;
        this.messageType = messageType;
    }

    /**
     * Compares two applications for equality.
     * 
     * @param o The object to be compared.
     * @return True/False they are equal.
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof AppLayerOutput) {
            AppLayerOutput ao = (AppLayerOutput)o;

            return (mac == ao.mac) && UtilsSpecialEquals.equalsPossibleNull(appName, ao.appName) && UtilsSpecialEquals.equalsPossibleNull(vehCommands, ao.vehCommands)
                    && UtilsSpecialEquals.equalsPossibleNull(sigCommands, ao.sigCommands) && UtilsSpecialEquals.equalsPossibleNull(outMessages, ao.outMessages)
                    && UtilsSpecialEquals.equalsPossibleNull(logs, ao.logs) && UtilsSpecialEquals.equalsPossibleNull(location, ao.location)
                    && UtilsSpecialEquals.equalsPossibleNull(messageType, ao.messageType);
        }
        else {
            return false;
        }
    }

    /**
     * Creates the hashcode for the output to the application layer.
     * 
     * @return The hashcode appended with the MAC.
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder(37, 105).append(mac).hashCode();
    }

    /**
     * Gets the location of the application.
     * 
     * @return location The location of the application.
     */
    public IDistanceable getLocation() {
        return location;
    }

    /**
     * Gets the veh commands.
     *
     * @return the veh commands
     */
    public List<VehicleCommand> getVehCommands() {
        return this.vehCommands;
    }

    /**
     * Gets the sig commands.
     *
     * @return the sig commands
     */
    public List<SignalCommand> getSigCommands() {
        return this.sigCommands;
    }

    /**
     * Gets the out messages.
     *
     * @return the out messages
     */
    public List<BasicMessage> getOutMessages() {
        return this.outMessages;
    }

    /**
     * Gets the logs.
     *
     * @return the logs
     */
    public List<LogData> getLogs() {
        return this.logs;
    }

    /**
     * This method adds zero or more vehicle commands to the vehCommands instance variable except
     * when the list has become immutable.
     *
     * @param vehCommands the veh commands
     */
    @CoberturaIgnore
    public void addVehicleCommandsList(List<VehicleCommand> vehCommands) {
        this.vehCommands.addAll(vehCommands);
    }

    /**
     * This method adds zero or more signal commands to the sigCommmands instance variable except
     * when the list has become immutable.
     *
     * @param sigCommands the sig commands
     */
    @CoberturaIgnore
    public void addSignalCommandsList(List<SignalCommand> sigCommands) {
        this.sigCommands.addAll(sigCommands);
    }

    /**
     * This method adds zero or more messages to the messages instance variable except when the list
     * has become immutable.
     *
     * @param messages the messages
     */
    @CoberturaIgnore
    public void addMessagesList(List<BasicMessage> messages) {
        this.outMessages.addAll(messages);
    }

    /**
     * This method adds zero or more LogData to the logs instance variable except when the list has
     * become immutable.
     *
     * @param logs the new list to be added
     */
    @CoberturaIgnore
    public void addLogsList(List<LogData> logs) {
        this.logs.addAll(logs);
    }

    /**
     * Getter for the message type
     * 
     * @return the message type
     */
    public MessageType getMessageType() {
        return messageType;
    }

    /**
     * setter for the message type
     * 
     * @param messageType the new message type
     */
    public void setMessageType(MessageType messageType) {
        this.messageType = messageType;
    }

    /**
     * This method finalizes the class so the instance variables can't be changed.
     * 
     * @return An immutable copy of this class.
     */
    public AppLayerOutput createImmutableCopy() {
        List<VehicleCommand> newVehCommands = Collections.unmodifiableList(vehCommands);
        List<SignalCommand> newSigCommands = Collections.unmodifiableList(sigCommands);
        List<BasicMessage> newOutMessages = Collections.unmodifiableList(outMessages);
        List<LogData> newLogs = Collections.unmodifiableList(logs);
        return new AppLayerOutput(this.appName, this.mac, this.location, newVehCommands, newSigCommands, newOutMessages, newLogs, this.messageType);
    }
}
