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

import java.rmi.RemoteException;
import java.util.List;

/**
 * Simulator Interface class for getting common simulator information.
 * 
 * @author ablatt
 * @author bbadillo
 * @author ttevendale
 */
public interface SimulatorInterface extends BuilderInterface {

    /**
     * Gets the static data from the simulator interface.
     * 
     * @return The static data.
     * @throws RemoteException If a remote exception occurs.
     */
    public StaticData getStaticData() throws RemoteException;

    /**
     * Gets the step data from the simulator interface.
     * 
     * @param stepNum The current step number.
     * @return The step data.
     * @throws RemoteException If a remote exception occurs.
     */
    public StepData getStepData(long stepNum) throws RemoteException;

    /**
     * Closes the simulator interface.
     * 
     * @throws RemoteException If a remote exception occurs.
     */
    public void close() throws RemoteException;

    /**
     * Adds vehicle commands to the simulator interface.
     * 
     * @param command The vehicle command to add to the simulator interface.
     * @throws RemoteException If a remote exception occurs.
     */
    public void addVehicleCommand(VehicleCommand command) throws RemoteException;

    /**
     * Adds vehicle injection requests to the simulator interface.
     * 
     * @param request The vehicle injection request to add to the simulator interface.
     * @throws RemoteException If a remote exception occurs.
     */
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) throws RemoteException;

    /**
     * Adds signal commands to the simulator interface.
     * 
     * @param command The signal command to add to the simulator interface.
     * @throws RemoteException If a remote exception occurs.
     */
    public void addSignalCommand(SignalCommand command) throws RemoteException;

    /**
     * Checks for output errors in the simulator interface.
     * 
     * @param simDirectory The simulator directory for output.
     * @return The list of simulator messages
     * @throws RemoteException If a remote exception occurs.
     */
    public List<SimulatorMessage> checkForErrorOutput(String simDirectory) throws RemoteException;

    /**
     * Checks for output warnings in the simulator interface.
     * 
     * @param simDirectory The simulator directory for output.
     * @return The list of simulator messages
     * @throws RemoteException If a remote exception occurs.
     */
    public List<SimulatorMessage> checkForWarningOutput(String simDirectory) throws RemoteException;
}
