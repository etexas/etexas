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

import java.util.List;

import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

/**
 * Data Layer for coordinating InterReps
 * 
 * @author ablatt
 * @author ttevendale
 */
public interface IInterRepCoordinationDataLayer {

    /**
     * Add the outputs of an InterRep
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The id of the interRep that is putting into the data layer
     * @param interRep The model to place into the data layer
     */
    public void putInterRepModel(int stepNum, int interRepId, InterRepInfoModel interRep);

    /**
     * Get the signal commands for this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The id of the interRep to get commands for
     * @return The signal commands for this time step
     */
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId);

    /**
     * Get the vehicle commands for this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The id of the interRep to get commands for
     * @return The vehicle commands for this time step
     */
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId);

    /**
     * Get the vehicle injection requests for this time step
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param interRepId The id of the interRep to get requests for
     * @return The vehicle injection requests for this time step
     */
    public List<VehicleInjectionRequest> getVehicleInjectionRequests(int stepNum, int interRepId);

    /**
     * Get the current time of the simulation
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The time in the simulation (in seconds)
     */
    public double getSimTime(int stepNum);
}
