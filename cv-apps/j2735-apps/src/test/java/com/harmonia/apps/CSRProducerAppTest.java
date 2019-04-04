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
package com.harmonia.apps;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.RSEDevice;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.VehicleStatus;
import org.etexascode.j2735.VehicleStatus.VehicleData;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.test.GenVehicleFunctions;
import org.etexascode.test.TestException;
import org.etexascode.test.TestHarnessCase;
import org.etexascode.test.TestInterRep;
import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;

public class CSRProducerAppTest {

    private CSRProducerApp app;

    private final Vehicle vPrime = GenVehicleFunctions.genVehicle(26, -1600, 180, 180, 4, 2);

    private BasicSafetyMessage bsm;

    private BasicSafetyMessageVerbose bsmV;

    private CommonSafetyRequest csrExpected;

    private CommonSafetyRequest csrActual;

    private byte[] vehicleID;

    /**
     * TestHarness Case object creates TestInterrep form playback data, and test devices
     */
    TestHarnessCase thc = new TestHarnessCase();

    String devId = "2";

    RSEDevice dev = null;

    double simtime = 0.5;

    @Before
    public void setUp() {
        app = new CSRProducerApp();
        vPrime.setSpeed(0.0);
        vPrime.setAcceleration(0.0);

        try {
            TestInterRep ir = thc.getTestInterRep(true);
            Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
            signalMap.put(ir.getId(), ir.getSignalManager());
            Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
            laneMap.put(ir.getId(), ir.getLaneManager());
            Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
            detectorMap.put(ir.getId(), ir.getDetectorManager());
            RSEDeviceInfo rdi = new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[] {}, 1L, new DistanceImpl(0, 0, 0));
            dev = new RSEDevice(rdi);
        }
        catch (TestException e) {
            e.printStackTrace();
        }
        bsm = new BasicSafetyMessage();
        bsmV = new BasicSafetyMessageVerbose();

        try {
            vPrime.setHeading(0.0);
            vPrime.setAcceleration(0.0);
            vPrime.setSpeed(0.0);

            BSMProducerApp.getBasicSafetyMessage(simtime, vPrime, false, (short)0, bsm);
            BSMVerboseProducerApp.getBasicSafetyMessageVerbose(simtime, vPrime, true, (short)0, bsmV);
        }
        catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }

        byte[] blob = bsm.getBlob1();
        vehicleID = Arrays.copyOfRange(blob, 1, 5);
        try {
            csrActual = app.getCommonSafetyRequest(vehicleID);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        csrExpected = new CommonSafetyRequest();
        csrExpected.setId(vehicleID);
        csrExpected.setMsgCnt((short)1);
        csrExpected.setMsgID(DSRCMessageID.MSG_ID_CSR);
        csrExpected.getRequests().add(new String(new byte[] { (byte)0xD }));

        bsmV.setId(vehicleID);

    }

    @Test
    public void testPerformUpdate() {
        BasicMessage wsmTest = new DSRCMessage(0, DSRCChannel.CH184, 0);
        // Whitebox.setInternalState(wsmTest, "data", null);
        Whitebox.setInternalState(wsmTest, "peerMACAddress", 1L);
        List<BasicMessage> receive = new ArrayList<BasicMessage>(1);
        receive.add(wsmTest);
        Object[] messages = new Object[receive.size()];
        for (int i = 0; i < receive.size(); i++) {
            messages[i] = receive.get(i).getData();
        }
        app.performUpdate(dev, messages, receive, 0.0, null);
        List<BasicMessage> res = dev.getAppMessages();
        dev.resetAppMessages();
        assertEquals(0, res.size());
    }

    @Test
    public void testGetBsmData() {
        assertEquals(new HashMap<Long, VehicleData>(), app.getBsmData());
    }

    @Test
    public void testAnalyzeMessage() {
        bsm.setStatus(new VehicleStatus());
        assertNull(app.analyzeMessage(bsm, 1L));
        bsm.setStatus(null);
        CommonSafetyRequest csr = (CommonSafetyRequest)app.analyzeMessage(bsm, 2L);
        for (int i = 0; i < csrExpected.getId().length; i++) {
            assertEquals(csrExpected.getId()[i], csr.getId()[i]);
        }
        assertEquals(csrExpected.getMsgCnt(), csr.getMsgCnt());
        assertEquals(csrExpected.getMsgID(), csr.getMsgID());
        assertEquals(csrExpected.getRequests(), csr.getRequests());
    }

    @Test
    public void testAnalyzeMessageVerbose() {
        bsmV.setStatus(new VehicleStatus());
        assertNull(app.analyzeMessage(bsmV, 1L));
        bsmV.setStatus(null);
        CommonSafetyRequest csr = (CommonSafetyRequest)app.analyzeMessage(bsmV, 2L);
        for (int i = 0; i < csrExpected.getId().length; i++) {
            assertEquals(csrExpected.getId()[i], csr.getId()[i]);
        }
        assertEquals(csrExpected.getMsgCnt(), csr.getMsgCnt());
        assertEquals(csrExpected.getMsgID(), csr.getMsgID());
        assertEquals(csrExpected.getRequests(), csr.getRequests());
    }

    @Test
    public void testGetCommonSafetyRequest() {
        for (int i = 0; i < csrExpected.getId().length; i++) {
            assertEquals(csrExpected.getId()[i], csrActual.getId()[i]);
        }
        assertEquals(csrExpected.getMsgCnt(), csrActual.getMsgCnt());
        assertEquals(csrExpected.getMsgID(), csrActual.getMsgID());
        assertEquals(csrExpected.getRequests(), csrActual.getRequests());
    }

    @Test
    public void testGetAppId() {
        assertEquals(CSRProducerApp.APP_NAME_CSR_PRODUCER_APP, ((IAppName)app).getAppName());
    }
}
