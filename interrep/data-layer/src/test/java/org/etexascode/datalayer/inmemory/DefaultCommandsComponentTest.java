/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.datalayer.inmemory;

import static org.junit.Assert.assertTrue;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for the default commands component.
 * 
 * @author emyers
 */
public class DefaultCommandsComponentTest {

    /** The commands component. */
    private ICommandsComponent commandsComponent;

    /**
     * Creates a new default commands component before each test.
     */
    @Before
    public void init() {

        commandsComponent = new DefaultCommandsComponent();
    }

    /**
     * Tests the <code>put</code>, <code>peek</code>, and <code>get</code> functionality for signal
     * commands in the default commands component.
     */
    @Test
    public void testSignalCommands() {

        // confirm that empty lists are returned when no signal commands exist
        assertTrue(commandsComponent.peekSignalCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.getSignalCommands(0, 0).isEmpty());

        // add new signal commands
        SignalCommand signalCommand = new SignalCommand();
        SignalCommand signalCommand2 = new SignalCommand();
        commandsComponent.putSignalCommand(0, 0, signalCommand);
        commandsComponent.putSignalCommand(0, 10, signalCommand2);

        // confirm that the signal commands were added
        assertTrue(commandsComponent.peekSignalCommands(0, 0).contains(signalCommand));
        assertTrue(commandsComponent.getSignalCommands(0, 0).contains(signalCommand));
        assertTrue(commandsComponent.peekSignalCommands(0, 10).contains(signalCommand2));
        assertTrue(commandsComponent.getSignalCommands(0, 10).contains(signalCommand2));

        // confirm that signal commands were removed
        assertTrue(commandsComponent.peekSignalCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.getSignalCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.peekSignalCommands(0, 10).isEmpty());
        assertTrue(commandsComponent.getSignalCommands(0, 10).isEmpty());
    }

    /**
     * Tests the <code>put</code>, <code>peek</code>, and <code>get</code> functionality for vehicle
     * commands in the default commands component.
     */
    @Test
    public void testVehicleCommands() {

        // confirm that empty lists are returned when no vehicle commands exist
        assertTrue(commandsComponent.peekVehicleCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.getVehicleCommands(0, 0).isEmpty());

        // add new vehicle commands
        VehicleCommand vehicleCommand = new VehicleCommand();
        VehicleCommand vehicleCommand2 = new VehicleCommand();
        commandsComponent.putVehicleCommand(0, 0, vehicleCommand);
        commandsComponent.putVehicleCommand(0, 10, vehicleCommand2);

        // confirm that the vehicle commands were added
        assertTrue(commandsComponent.peekVehicleCommands(0, 0).contains(vehicleCommand));
        assertTrue(commandsComponent.getVehicleCommands(0, 0).contains(vehicleCommand));
        assertTrue(commandsComponent.peekVehicleCommands(0, 10).contains(vehicleCommand2));
        assertTrue(commandsComponent.getVehicleCommands(0, 10).contains(vehicleCommand2));

        // confirm that the vehicle commands were removed
        assertTrue(commandsComponent.peekVehicleCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.getVehicleCommands(0, 0).isEmpty());
        assertTrue(commandsComponent.peekVehicleCommands(0, 10).isEmpty());
        assertTrue(commandsComponent.getVehicleCommands(0, 10).isEmpty());
    }

    /**
     * Tests the <code>put</code>, <code>peek</code>, and <code>get</code> functionality for vehicle
     * injection requests in the default commands component.
     */
    @Test
    public void testVehicleInjections() {

        // confirm that empty lists are returned when no vehicle injections exist
        assertTrue(commandsComponent.peekVehicleInjections(0, 0).isEmpty());
        assertTrue(commandsComponent.getVehicleInjections(0, 0).isEmpty());

        // add new vehicle injections
        VehicleInjectionRequest vehicleInjection = new VehicleInjectionRequest();
        VehicleInjectionRequest vehicleInjection2 = new VehicleInjectionRequest();
        commandsComponent.putVehicleInjection(0, 0, vehicleInjection);
        commandsComponent.putVehicleInjection(0, 10, vehicleInjection2);

        // confirm that the vehicle injections were added
        assertTrue(commandsComponent.peekVehicleInjections(0, 0).contains(vehicleInjection));
        assertTrue(commandsComponent.getVehicleInjections(0, 0).contains(vehicleInjection));
        assertTrue(commandsComponent.peekVehicleInjections(0, 10).contains(vehicleInjection2));
        assertTrue(commandsComponent.getVehicleInjections(0, 10).contains(vehicleInjection2));

        // confirm that the vehicle injections were removed
        assertTrue(commandsComponent.peekVehicleInjections(0, 0).isEmpty());
        assertTrue(commandsComponent.getVehicleInjections(0, 0).isEmpty());
        assertTrue(commandsComponent.peekVehicleInjections(0, 10).isEmpty());
        assertTrue(commandsComponent.getVehicleInjections(0, 10).isEmpty());
    }
}
