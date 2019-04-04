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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author ablatt
 */
public class SingleIntersectionCommandsComponentTest {

    SingleIntersectionCommandsComponent sicc = null;

    SignalCommand sc = null;

    VehicleCommand vc = null;

    VehicleInjectionRequest vir = null;

    ArrayList<SignalCommand> scl = null;

    ArrayList<VehicleCommand> vcl = null;

    ArrayList<VehicleInjectionRequest> virl = null;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setup() {
        sicc = new SingleIntersectionCommandsComponent();
        sc = new SignalCommand();
        vc = new VehicleCommand();
        vir = new VehicleInjectionRequest();
        scl = new ArrayList<SignalCommand>(1);
        scl.add(sc);
        vcl = new ArrayList<VehicleCommand>(1);
        vcl.add(vc);
        virl = new ArrayList<VehicleInjectionRequest>(1);
        virl.add(vir);
    }

    @After
    public void teardown() {
        sicc = null;
        sc = null;
        vc = null;
        vir = null;
        scl = null;
        vcl = null;
        virl = null;
    }

    @Test
    public void testGetSignalCommands() {
        List<SignalCommand> comms = sicc.sigCommands;
        List<SignalCommand> reted = sicc.getSignalCommands(0, 0);
        assertTrue(comms == reted);
        assertFalse(comms == sicc.sigCommands); // make sure sigCommands was reinitialized
    }

    @Test
    public void testGetVehicleCommands() {
        List<VehicleCommand> comms = sicc.vehCommands;
        List<VehicleCommand> reted = sicc.getVehicleCommands(0, 0);
        assertTrue(comms == reted);
        assertFalse(comms == sicc.vehCommands); // make sure vehCommands was reinitialized
    }

    @Test
    public void testGetVehicleInjections() {
        List<VehicleInjectionRequest> comms = sicc.vehInjections;
        List<VehicleInjectionRequest> reted = sicc.getVehicleInjections(0, 0);
        assertTrue(comms == reted);
        assertFalse(comms == sicc.vehInjections); // make sure vehInjections was reinitialized
    }

    @Test
    public void testPutSignalCommand() {
        sicc.putSignalCommand(0, 0, sc);
        assertEquals(1, sicc.sigCommands.size());
        assertTrue(sicc.sigCommands.contains(sc));
    }

    @Test
    public void testPutSignalCommands() {
        sicc.putSignalCommands(0, 0, scl);
        assertEquals(scl, sicc.sigCommands);
    }

    @Test
    public void testPutVehicleCommand() {
        sicc.putVehicleCommand(0, 0, vc);
        assertEquals(1, sicc.vehCommands.size());
        assertTrue(sicc.vehCommands.contains(vc));
    }

    @Test
    public void testPutVehicleCommands() {
        sicc.putVehicleCommands(0, 0, vcl);
        assertEquals(vcl, sicc.vehCommands);
    }

    @Test
    public void testPeekSignalCommandsRemoveException() {
        List<SignalCommand> comms = sicc.peekSignalCommands(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.remove(0);
    }

    @Test
    public void testPeekSignalCommandsAddException() {
        List<SignalCommand> comms = sicc.peekSignalCommands(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.add(sc);
    }

    @Test
    public void testPeekVehicleCommandsRemoveException() {
        List<VehicleCommand> comms = sicc.peekVehicleCommands(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.remove(0);
    }

    @Test
    public void testPeekVehicleCommandsAddException() {
        List<VehicleCommand> comms = sicc.peekVehicleCommands(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.add(vc);
    }

    @Test
    public void testPeekVehicleInjectionsCommandsRemoveException() {
        List<VehicleInjectionRequest> comms = sicc.peekVehicleInjections(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.remove(0);
    }

    @Test
    public void testPeekVehicleInjectionsCommandsAddException() {
        List<VehicleInjectionRequest> comms = sicc.peekVehicleInjections(0, 0);
        thrown.expect(UnsupportedOperationException.class);
        comms.add(vir);
    }

    @Test
    public void testPutVehicleInjection() {
        sicc.putVehicleInjection(0, 0, vir);
        assertEquals(1, sicc.vehInjections.size());
        assertTrue(sicc.vehInjections.contains(vir));
    }

    @Test
    public void testPutVehicleInjections() {
        sicc.putVehicleInjections(0, 0, virl);
        assertEquals(virl, sicc.vehInjections);
    }
}
