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
package org.etexascode.datalayer.inmemory;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * Implementation of the Commands Component. Manages commands for a single intersection
 * implementation.
 * 
 * @author ablatt
 */
public class SingleIntersectionCommandsComponent implements ICommandsComponent {

    /**
     * Current managed signal commands.
     */
    List<SignalCommand> sigCommands = new LinkedList<SignalCommand>();

    /**
     * Current managed vehicle commands.
     */
    List<VehicleCommand> vehCommands = new LinkedList<VehicleCommand>();

    /**
     * Current managed vehicle injections.
     */
    List<VehicleInjectionRequest> vehInjections = new LinkedList<VehicleInjectionRequest>();

    /**
     * Gets the signal commands at the step number for the interrep id provided and returns the list
     * of commands
     * 
     * @param stepNum The step number from the simulation
     * @param interRepId The interrep id for the sim
     * @return ret The signal commands as a linked list
     */
    @Override
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {
        List<SignalCommand> ret = sigCommands;
        sigCommands = new LinkedList<SignalCommand>();
        return ret;
    }

    /**
     * Gets the vehicle commands at the step number for the interrep id provided
     * 
     * @param stepNum The step number from the simulation
     * @param interRepId The interrep id for the sim
     * @return ret The vehicle commands as a linked list
     */
    @Override
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {
        List<VehicleCommand> ret = vehCommands;
        vehCommands = new LinkedList<VehicleCommand>();
        return ret;
    }

    /**
     * Gets the vehicle injections at the step number for the interrep id provided
     * 
     * @param stepNum The step number from the simulation
     * @param interRepId The interrep id for the sim
     * @return ret The vehicle injections as a linked list
     */
    @Override
    public List<VehicleInjectionRequest> getVehicleInjections(int stepNum, int interRepId) {
        List<VehicleInjectionRequest> ret = vehInjections;
        vehInjections = new LinkedList<VehicleInjectionRequest>();
        return ret;
    }

    /**
     * Adds a signal command to the component for the step number and interrep id provided
     * 
     * @param stepNum The step number from the simulation
     * @param interRepId The interrep id for the simulation
     */
    @Override
    public void putSignalCommand(int stepNum, int interRepId, SignalCommand command) {
        sigCommands.add(command);
    }

    /**
     * Adds all signal commands to the component for the step number and interrep id provided
     * 
     * @param stepNum The step number from the simulation
     * @param interRepId The interrep id for the simulation
     */
    @Override
    public void putSignalCommands(int stepNum, int interRepId, List<SignalCommand> commands) {
        sigCommands.addAll(commands);
    }

    /**
     * Adds a single vehicle command to the component for the step number and interrep id provided
     * 
     * @param stepNum The step number for the simulation
     * @param interRepId The interrep id for the simulation
     */
    @Override
    public void putVehicleCommand(int stepNum, int interRepId, VehicleCommand command) {
        vehCommands.add(command);
    }

    /**
     * Adds all the vehicle commands to the component for the step number and interrep id provided
     * 
     * @param stepNum The step number for the simulation
     * @param interRepId The interrep id for the simulation
     */
    @Override
    public void putVehicleCommands(int stepNum, int interRepId, List<VehicleCommand> commands) {
        vehCommands.addAll(commands);
    }

    @Override
    public List<SignalCommand> peekSignalCommands(int stepNum, int interRepId) {
        // This list will never be null
        return Collections.unmodifiableList(sigCommands);
    }

    @Override
    public List<VehicleCommand> peekVehicleCommands(int stepNum, int interRepId) {
        // This list will never be null
        return Collections.unmodifiableList(vehCommands);
    }

    @Override
    public List<VehicleInjectionRequest> peekVehicleInjections(
            int stepNum, int interRepId) {
        // This list will never be null
        return Collections.unmodifiableList(vehInjections);
    }

    @Override
    public void putVehicleInjection(int stepNum, int interRepId,
            VehicleInjectionRequest injection) {
        vehInjections.add(injection);
    }

    @Override
    public void putVehicleInjections(int stepNum, int interRepId,
            List<VehicleInjectionRequest> injections) {
        vehInjections.addAll(injections);

    }

}
