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

import java.util.List;

import org.etexascode.devicedata.LogData;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.persistencelayer.interfaces.IAppLogsPersistenceLayer;
import org.etexascode.persistencelayer.interfaces.ICommandsPersistenceLayer;

/**
 * An implementation for testing that holds data in memory and provides access to it.
 * 
 * @author janway
 */
public class TestPersistenceLayer implements IPersistenceLayer {

    IAppLogsPersistenceLayer ipl;

    ICommandsPersistenceLayer icl;

    public TestPersistenceLayer() {
        ipl = new TestAppLogsPersistenceLayer();
        icl = new TestCommandsPersistenceLayer();
    }

    @Override
    public void persistAppLogs(List<LogData> logs) {
        ipl.persistAppLogs(logs);
    }

    public List<LogData> getAppLogs() {
        return ((TestAppLogsPersistenceLayer)ipl).getLogs();
    }

    @Override
    public void shutdown() {}

    @Override
    public void persistVehicleCommands(String source, int intersectionId, double submittedTime, List<VehicleCommand> commands) {
        icl.persistVehicleCommands(source, intersectionId, submittedTime, commands);
    }

    @Override
    public void persistSignalCommands(String source, int intersectionId, double submittedTime, List<SignalCommand> commands) {
        icl.persistSignalCommands(source, intersectionId, submittedTime, commands);
    }

    public List<VehicleCommand> getVehicleComamnds() {
        return ((TestCommandsPersistenceLayer)icl).getVehicleCommands();
    }

    public List<SignalCommand> getSignalCommands() {
        return ((TestCommandsPersistenceLayer)icl).getSignalCommands();
    }

    @Override
    public void persistVehicleInjectionRequests(String source, int intersectionId, double submittedTime, List<VehicleInjectionRequest> requests) {
        icl.persistVehicleInjectionRequests(source, intersectionId, submittedTime, requests);
    }

    public List<VehicleInjectionRequest> getVehicleInjectionRequests() {
        return ((TestCommandsPersistenceLayer)icl).getVehicleInjectionRequests();
    }
}
