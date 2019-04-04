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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.junit.Test;

/**
 * Abstract Proxy App Test.
 * 
 * @author jrutherford
 */
@SuppressWarnings("rawtypes")
public class AbstractProxyAppTest {

    @Test
    public void testGetMessages() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertNotNull(app.getMessages());
    }

    @Test
    public void testGetVehicleCommands() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getVehicleCommands() != null);
        assertTrue(app.getVehicleCommands().size() == 0);
    }

    @Test
    public void testGetSignalCommands() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getSignalCommands() != null);
        assertTrue(app.getSignalCommands().size() == 0);
    }

    @Test
    public void testGetLogData() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getLogData() != null);
        assertTrue(app.getLogData().size() == 0);
    }

    @Test
    public void testGetMessagesInjected() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getMessagesInjected() != null);
        assertTrue(app.getMessagesInjected().size() == 0);
    }

    @Test
    public void testAddVehicleCommand() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.addVehicleCommand(new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 20));
        assertTrue(app.getVehicleCommands().size() == 1);
    }

    @Test
    public void testAddSignalCommand() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.addSignalCommand(SignalCommand.createChangeCommand(100.0));
        assertTrue(app.getSignalCommands().size() == 1);
    }

    @Test
    public void testAddLogData() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.addLogData("Hello", "Test");
        assertTrue(app.getLogData().size() == 1);
        assertTrue(app.getLogData().get("Hello") == "Test");
    }

    @Test
    public void testAddMessageRequest() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.addMessage(new DSRCMessage(0, DSRCChannel.CH184, 0));
        assertTrue(app.getMessagesInjected().size() == 1);
    }

    @Test
    public void testDischarge() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.addVehicleCommand(new VehicleSpeedCommand(1, VehicleSpeedCommand.MAX_ACCELERATE_TO_XX, 20));
        app.addSignalCommand(SignalCommand.createChangeCommand(100.0));
        app.addMessage(new DSRCMessage(0, DSRCChannel.CH184, 0));
        AppLayerOutput alo = app.discharge();
        assertTrue(alo.vehCommands.size() == 1);
        assertTrue(alo.sigCommands.size() == 1);
        assertTrue(alo.outMessages.size() == 1);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testPerformUpdate() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.performUpdate(null, null, new ArrayList<BasicMessage>(0), null, null);
        assertNotNull(app);
    }

    @Test
    public void testGetAppId() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getAppName().equals("RemoteApp"));
    }

    @Test
    public void testInit() {
        RemoteProxyApp app = new RemoteProxyApp();
        app.init(new String[] { "BSM-producer" });
        assertTrue(app.getAppName().equals("BSM-producer"));
    }

    @Test
    public void testGetProperId() {
        RemoteProxyApp app = new RemoteProxyApp();
        assertTrue(app.getProperId() != null);
    }
}