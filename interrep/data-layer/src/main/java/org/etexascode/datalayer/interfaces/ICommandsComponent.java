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
package org.etexascode.datalayer.interfaces;

import java.util.Collection;
import java.util.List;

import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * Interface for accessing a container for the commands.
 * 
 * @author ablatt
 */
public interface ICommandsComponent {

    /**
     * Adds a single signal command to the container. Associates the command with step num and
     * interRep id.
     * 
     * @param stepNum The step number the command is being added for
     * @param interRepId The id of the interRep which should receive this command
     * @param command The command to add
     */
    public void putSignalCommand(int stepNum, int interRepId, SignalCommand command);

    /**
     * Adds multiple signal commands to the container. Associates the commands with step num and
     * interRep id.
     * 
     * @param stepNum The step number the command is being added for
     * @param interRepId The id of the interRep which should receive this command
     * @param commands The commands to add
     */
    public void putSignalCommands(int stepNum, int interRepId, List<SignalCommand> commands);

    /**
     * Adds a single vehicle command to the container. Associates the command with step num and
     * interRep id.
     * 
     * @param stepNum The step number the command is being added for
     * @param interRepId The id of the interRep which should receive this command
     * @param command The command to add
     */
    public void putVehicleCommand(int stepNum, int interRepId, VehicleCommand command);

    /**
     * Adds multiple vehicle commands to the container. Associates the commands with step num and
     * interRep id.
     * 
     * @param stepNum The step number the command is being added for
     * @param interRepId The id of the interRep which should receive this command
     * @param commands The commands to add
     */
    public void putVehicleCommands(int stepNum, int interRepId, List<VehicleCommand> commands);

    /**
     * Adds a single vehicle inject to the container. Associates the inject with step num and
     * interRep id.
     * 
     * @param stepNum The step number the command is being added for
     * @param interRepId The id of the interRep which should receive this injection
     * @param injection The vehicle injection request.
     */
    public void putVehicleInjection(int stepNum, int interRepId, VehicleInjectionRequest injection);

    /**
     * Adds multiple vehicle injections to the container. Associates the injections with step num
     * and interRep id.
     * 
     * @param stepNum The step number the injection is being added for
     * @param interRepId The id of the interRep which should receive this injection
     * @param injections The injections to add
     */
    public void putVehicleInjections(int stepNum, int interRepId, List<VehicleInjectionRequest> injections);

    /**
     * Gets the signal commands associated with step num and interRep id in the container.
     * 
     * @param stepNum The step num to get commands for
     * @param interRepId The interRep id to get commands for
     * @return The signal commands assocated with step num and interRep id
     */
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId);

    /**
     * Gets the vehicle commands associated with step num and interRep id in the container.
     * 
     * @param stepNum The step num to get commands for
     * @param interRepId The interRep id to get commands for
     * @return The signal commands assocated with step num and interRep id
     */
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId);

    /**
     * Gets the signal commands associated with step num and interRep id in the container.
     * 
     * @param stepNum The step num to get commands for
     * @param interRepId The interRep id to get commands for
     * @return The signal commands assocated with step num and interRep id
     */
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId);

    /**
     * Gets the vehicle commands associated with step num and interRep id in the container.
     * 
     * @param stepNum The step num to get commands for
     * @param interRepId The interRep id to get commands for
     * @return The vehicle commands assocated with step num and interRep id
     */
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId);

    /**
     * Gets the vehicle injections associated with step num and interRep id in the container.
     * 
     * @param stepNum The step num to get commands for
     * @param interRepId The interRep id to get commands for
     * @return The vehicle injections assocated with step num and interRep id
     */
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId);

    /**
     * Get the vehicle injections associated with the current time step
     * 
     * @param stepNum The step number.
     * @param interRepId The ID of the interRep object.
     * @return The vehicle injections associated with the current time step
     */
    public List<VehicleInjectionRequest> getVehicleInjections(int stepNum, int interRepId);
}
