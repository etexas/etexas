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
import java.util.Map;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.Command;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * The data layer used by the web app.
 * 
 * @author ablatt
 */
public interface IWebAppDataLayer {

    /**
     * Get all the vehicles in the data layer TODO: ablatt - should use the immutable interface this
     * returns an iterable so it is possible to make the implementation lazy in cases where there
     * are too many vehicles to fit in one table...
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The interRep to associate command with
     * @return The vehicles in the data layer
     */
    public Iterable<? extends IVehicle> getAllVehiclesForTable(int stepNum, int interRepId);

    /**
     * Get all the detectors in the data layer as of stepNum on interRepId.
     * 
     * @param stepNum The step number we are querying the data layer for.
     * @param interRepId The id of the interRep whose detectors we are interested in.
     * @return The detectors associated with stepNum and interRepId.
     */
    public Iterable<? extends IDetector> getAllDetectors(int stepNum, int interRepId);

    /**
     * Get all the devices in the data layer as of stepNum.
     * 
     * @param stepNum The step number we are querying the data layer for.
     * @return The devices associated with stepNum.
     */
    public Iterable<? extends IDeviceData> getCurrentDevices(int stepNum);

    /**
     * Get every transmitted message which was added to the Data Layer this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return Every transmitted message which was added to the Data Layer this time step
     */
    public Map<Long, List<BasicMessage>> getTxMessages(int stepNum);

    /**
     * Get every received message which was added to the Data Layer this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return Every received message which was added to the Data Layer this time step
     */
    public Map<Long, List<BasicMessage>> getRxMessages(int stepNum);

    /**
     * Get a specific lane a vehicle is in TODO: ablatt - should use the immutable interface
     * requires a vehicle so we can match the vehicle up with an intersection
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param vi The vehicle to get the lane for
     * @return The lane the vehicle is in (null if the vehicle is not in a lane)
     */
    public ILane getLaneFromVehicle(int stepNum, IVehicle vi);

    /**
     * Get the list of signal indications currently governing the lane TODO: ablatt - should use the
     * immutable interface Requires a lane so we can match the lane to the intersections to the
     * signal manager to the signal indications
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param li The lane to get the signal indications for
     * @return The signal indications governing the lane
     */
    public List<? extends ISignalIndication> getSignalIndication(int stepNum, ILane li);

    /**
     * Get the InterRepInfoModel produced by the given intersection at the given timestep
     * 
     * @param stepNum The timestep to get data for
     * @param interRepId The intersection to get data for
     * @return The interrep info model.
     */
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId);

    /**
     * Add a Signal Command to the data layer
     * 
     * @param source The source of the command.
     * @param stepNum The step number which the Data Layer should use
     * @param command The signal command to add to the data layer
     * @param interRepId The interRep to associate command with
     */
    public void addSignalCommand(String source, int stepNum, SignalCommand command, int interRepId);

    /**
     * Add a Vehicle Command to the data layer
     * 
     * @param source The source of the command.
     * @param stepNum The step number which the Data Layer should use
     * @param command The vehicle command to add to the data layer
     * @param interRepId The interRep to associate command with
     */
    public void addVehicleCommand(String source, int stepNum, VehicleCommand command, int interRepId);

    /**
     * Add a Vehicle Injection Request to the data layer
     * 
     * @param source The source of the request.
     * @param stepNum The step number which the Data Layer should use
     * @param request The vehicle injection request to add to the data layer
     * @param interRepId The interRep to associate request with
     */
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId);

    /**
     * Method to peek at signal commands in current time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The interRep containing commands
     * @return List of Signal Commands in InterRep at current time step.
     */
    public Collection<? extends Command> peekSignalCommands(int stepNum, int interRepId);

    /**
     * Method to peek at vehicle commands in current time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The interRep containing commands
     * @return List of Vehicle Commands in InterRep at current time step.
     */
    public Collection<? extends Command> peekVehicleCommands(int stepNum, int interRepId);

    /**
     * Method to peek at vehicle injections in current time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The interRep containing commands
     * @return List of Vehicle Commands in InterRep at current time step.
     */
    public Collection<? extends Command> peekVehicleInjections(int stepNum, int interRepId);

    /**
     * Gets the proper ID of a vehicle by global ID.
     * 
     * @param globalId The global ID to get the proper ID with.
     * @return The vehicle's proper ID.
     */
    public String getProperVehicleId(long globalId);

    /**
     * Returns the next ID for vehicle injection.
     * 
     * @return The next integer ID for vehicle injection.
     */
    public int nextInjectedVehicleId();
}
