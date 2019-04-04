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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.LogData;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleDestinationCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.wavesim.Tx.MessageType;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author jconnelly
 */

public class AppLayerOutputTest {

    IConnectedVehicleApp<String> app = null;

    List<VehicleCommand> vehCommands = null;

    List<SignalCommand> sigCommands = null;

    List<BasicMessage> outMessages = null;

    List<LogData> logs = null;

    IDeviceInfo input = null;

    private BigDecimal st = null;

    private long mac = -1;

    private String appId = null;

    private IDistanceable location = null;

    AppLayerOutput appOut = null;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setup() {
        st = BigDecimal.ONE;
        mac = 1234;
        appId = "AppID";
        input = produceIDeviceInfo();
        app = new IConnectedVehicleApp<String>() {

            @Override
            public void performUpdate(String device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {}
        };
        vehCommands = genVehList();
        sigCommands = genSigList();
        outMessages = genMsgList();
        logs = genLogList();
        location = genLocation();
        appOut = produceAppOut();
    }

    @After
    public void teardown() {
        vehCommands = null;
        sigCommands = null;
        outMessages = null;
        input = null;
        app = null;
        mac = -1;
        appId = null;
        location = null;
        st = null;
        logs = null;
        appOut = null;
    }

    @Test
    public void testConstructor() {
        assertEquals(appOut, produceAppOut());
    }

    @Test
    public void testEqualsTrue() {
        assertTrue(appOut.equals(produceAppOut()));
    }

    @Test
    public void testEqualsFalse() {
        AppLayerOutput testF = new AppLayerOutput("false", 0, null);
        assertFalse(appOut.equals(testF));
        AppLayerInput test3 = new AppLayerInput(app, input);
        assertFalse(appOut.equals(test3));
    }

    @Test
    public void testEqualsFalse2() {
        AppLayerOutput same = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertTrue(appOut.equals(same));

        AppLayerOutput diff1 = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, MessageType.CELLULAR);
        assertFalse(appOut.equals(diff1));

        List<LogData> diffLogs = new ArrayList<LogData>();
        diffLogs.addAll(appOut.logs);
        diffLogs.add(new LogData());
        AppLayerOutput diff2 = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, diffLogs, appOut.getMessageType());
        assertFalse(appOut.equals(diff2));

        List<BasicMessage> diffMessages = new ArrayList<BasicMessage>();
        diffMessages.addAll(appOut.outMessages);
        diffMessages.add(new DSRCMessage(0, DSRCChannel.CH184, 0));
        AppLayerOutput diff3 = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, appOut.vehCommands, appOut.sigCommands, diffMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff3));

        List<SignalCommand> diffSigCommands = new ArrayList<SignalCommand>();
        diffSigCommands.addAll(appOut.sigCommands);
        diffSigCommands.add(new SignalCommand());
        AppLayerOutput diff4 = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, appOut.vehCommands, diffSigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff4));

        List<VehicleCommand> diffVehCommands = new ArrayList<VehicleCommand>();
        diffVehCommands.addAll(appOut.vehCommands);
        diffVehCommands.add(new VehicleCommand());
        AppLayerOutput diff5 = new AppLayerOutput(appOut.appName, appOut.mac, appOut.location, diffVehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff5));

        IDistanceable loc = appOut.location;
        IDistanceable difLoc = new DistanceImpl(loc.getX() + 10, loc.getY() + 10, loc.getZ() + 10);
        AppLayerOutput diff6 = new AppLayerOutput(appOut.appName, appOut.mac, difLoc, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff6));

        AppLayerOutput diff7 = new AppLayerOutput(appOut.appName, appOut.mac + 10, appOut.location, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff7));

        AppLayerOutput diff8 = new AppLayerOutput(appOut.appName + 10, appOut.mac, appOut.location, appOut.vehCommands, appOut.sigCommands, appOut.outMessages, appOut.logs, appOut.getMessageType());
        assertFalse(appOut.equals(diff8));
    }

    @Test
    public void testHashCode() {
        assertEquals(appOut.hashCode(), produceAppOut().hashCode());
    }

    @Test
    public void testGetLocation() {
        assertEquals(appOut.getLocation(), location);
    }

    @Test
    public void testCreateImmutableCopy() {
        AppLayerOutput appLayerOutput = produceAppOut();
        AppLayerOutput immutableCopy = appLayerOutput.createImmutableCopy();
        assertTrue(appLayerOutput.equals(immutableCopy));
        try {
            immutableCopy.logs.clear();
            thrown.expect(UnsupportedOperationException.class);
        }
        catch (UnsupportedOperationException e) {}

    }

    private AppLayerOutput produceAppOut() {
        AppLayerOutput pao = new AppLayerOutput(appId, mac, location);
        pao.vehCommands.addAll(0, vehCommands);
        pao.sigCommands.addAll(0, sigCommands);
        pao.outMessages.addAll(0, outMessages);
        pao.logs.addAll(0, logs);
        return pao;
    }

    private List<VehicleCommand> genVehList() {
        List<VehicleCommand> vcl = new ArrayList<VehicleCommand>(2);
        VehicleSpeedCommand vscmd1 = new VehicleSpeedCommand(1, 3, 15.0);
        VehicleDestinationCommand vdcmd2 = new VehicleDestinationCommand(2, 2);
        vcl.add(0, vscmd1);
        vcl.add(1, vdcmd2);
        return vcl;
    }

    private List<SignalCommand> genSigList() {
        List<SignalCommand> scl = new ArrayList<SignalCommand>(1);
        SignalCommand scmd1 = new SignalCommand(1, 0.0);
        scl.add(0, scmd1);
        return scl;
    }

    private IDeviceInfo produceIDeviceInfo() {
        IDeviceInfo devinfo = new OBUDeviceInfo();
        return devinfo;
    }

    private BasicMessage genMsgReq() {
        BasicMessage mr = new DSRCMessage("message", DSRCChannel.CH184, 9876, 2);
        return mr;
    }

    private List<BasicMessage> genMsgList() {
        List<BasicMessage> mrl = new ArrayList<BasicMessage>(1);
        mrl.add(0, genMsgReq());
        return mrl;
    }

    private List<LogData> genLogList() {
        List<LogData> lld = new ArrayList<LogData>(1);
        LogData ld = new LogData(1234, "AppId", st, "Key", "Message");
        lld.add(0, ld);
        return lld;
    }

    private IDistanceable genLocation() {
        IDistanceable loc = new Vehicle(1, 10, 10, 15, 15, 0);
        return loc;
    }
}
