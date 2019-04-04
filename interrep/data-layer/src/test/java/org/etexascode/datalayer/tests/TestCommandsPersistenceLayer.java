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
package org.etexascode.datalayer.tests;

import java.util.LinkedList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.persistencelayer.interfaces.ICommandsPersistenceLayer;

/**
 * Test implementation of CommandsPersistenceLayer.
 * 
 * @author janway
 */
public class TestCommandsPersistenceLayer implements ICommandsPersistenceLayer {

    private List<VehicleCommand> vehCommands = new LinkedList<VehicleCommand>();

    private List<SignalCommand> sigCommands = new LinkedList<SignalCommand>();

    private List<VehicleInjectionRequest> injRequests = new LinkedList<VehicleInjectionRequest>();

    @Override
    public void persistVehicleCommands(String source, int intersectionId, double submittedTime, List<VehicleCommand> commands) {
        vehCommands.addAll(commands);
    }

    @Override
    public void persistSignalCommands(String source, int intersectionId, double submittedTime, List<SignalCommand> commands) {
        sigCommands.addAll(commands);
    }

    public List<VehicleCommand> getVehicleCommands() {
        return this.vehCommands;
    }

    public List<SignalCommand> getSignalCommands() {
        return this.sigCommands;
    }

    @Override
    public void persistVehicleInjectionRequests(String source, int intersectionId, double submittedTime, List<VehicleInjectionRequest> requests) {
        injRequests.addAll(requests);
    }

    public List<VehicleInjectionRequest> getVehicleInjectionRequests() {
        return this.injRequests;
    }
}
