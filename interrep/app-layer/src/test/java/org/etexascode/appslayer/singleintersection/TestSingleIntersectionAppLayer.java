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
package org.etexascode.appslayer.singleintersection;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.OBUDevice;
import org.etexascode.appslayer.TestIOBUBaseApp;
import org.etexascode.appslayer.TestIRSEBaseApp;
import org.etexascode.appslayer.TestIReportBaseApp;
import org.etexascode.appslayer.TestNativeAppManager;
import org.etexascode.appslayer.remote.TestRemoteAppsManager;
import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.datalayer.tests.TestAppsDataLayer;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.LogData;
import org.etexascode.driver.SimDriverException;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 * @author jrutherford
 */
public class TestSingleIntersectionAppLayer {

    TestAppsDataLayer data = null;

    TestRemoteAppsManager ral = null;

    TestNativeAppManager nam = null;

    AppLayerOutput ao1 = null;

    AppLayerOutput ao2 = null;

    TestIOBUBaseApp app1 = null;

    TestIRSEBaseApp app2 = null;

    TestIReportBaseApp app3 = null;

    OBUDeviceInfo di1 = null;

    RSEDeviceInfo di2 = null;

    ReportDeviceInfo di3 = null;

    SingleIntersectionAppLayer sial;

    long OBUDeviceId = 1;

    long RSEDeviceId = 2;

    long ReportDeviceId = 3;

    double simtime = 0.0;

    String key = "Key";

    String message = "Message";

    AppLayerInput obuindex = null;

    AppLayerInput rseindex = null;

    AppLayerInput reportindex = null;

    ArrayList<BasicMessage> valm = null;

    BasicMessage vmr = null;

    ArrayList<LogData> vlogs = null;

    ArrayList<VehicleCommand> vcmds = null;

    ArrayList<SignalCommand> scmds = null;

    BasicMessage smr = null;

    ArrayList<LogData> slogs = null;

    ArrayList<BasicMessage> salm = null;

    ArrayList<LogData> rlogs = null;

    BasicMessage rmr = null;

    ArrayList<BasicMessage> ralm = null;

    ArrayList<LogData> goa = null;

    List<BasicMessage> wsml = null;

    String exceptionMessage = null;

    ExceptionApp excApp = null;

    AppLayerInput ali = null;

    TestAppsDataLayer tadl = null;

    SingleIntersectionAppLayer execpTest = null;

    @Before
    public void setup() {
        data = new TestAppsDataLayer();
        ral = new TestRemoteAppsManager();
        nam = new TestNativeAppManager();
        ao1 = getAo1();
        app1 = new TestIOBUBaseApp();
        app2 = new TestIRSEBaseApp();
        app3 = new TestIReportBaseApp();

        di1 = getDi1();
        di2 = getDi2();
        di3 = getDi3();

        data.inputs = new ArrayList<AppLayerInput>(3);
        data.inputs.add(new AppLayerInput(app1, di1));
        data.inputs.add(new AppLayerInput(app2, di2));
        data.inputs.add(new AppLayerInput(app3, di3));

        sial = new SingleIntersectionAppLayer(data, ral, nam);

        obuindex = data.inputs.get(0);
        vcmds = new ArrayList<VehicleCommand>(1);
        vcmds.add(new VehicleSpeedCommand(1, 1, 20));
        vmr = new DSRCMessage("String", DSRCChannel.CH184, -400, -549765189);
        vmr.setOriginMACAddress(obuindex.input.getDeviceId());
        valm = new ArrayList<BasicMessage>(1);
        valm.add(vmr);
        vlogs = new ArrayList<LogData>(1);
        vlogs.add(new LogData(OBUDeviceId, data.inputs.get(0).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));

        rseindex = data.inputs.get(1);
        scmds = new ArrayList<SignalCommand>(1);
        scmds.add(new SignalCommand(1, 1.0));
        smr = new DSRCMessage("String", DSRCChannel.CH184, -400, -549765200);
        smr.setOriginMACAddress(rseindex.input.getDeviceId());
        salm = new ArrayList<BasicMessage>(1);
        salm.add(smr);
        slogs = new ArrayList<LogData>(1);
        slogs.add(new LogData(RSEDeviceId, data.inputs.get(1).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));

        reportindex = data.inputs.get(2);
        rlogs = new ArrayList<LogData>(1);
        rlogs.add(new LogData(ReportDeviceId, data.inputs.get(2).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));
        rmr = new DSRCMessage("String", DSRCChannel.CH184, -400, -549765300);
        rmr.setOriginMACAddress(reportindex.input.getDeviceId());
        ralm = new ArrayList<BasicMessage>(1);
        ralm.add(rmr);

        goa = new ArrayList<LogData>(3);
        goa.add(new LogData(OBUDeviceId, data.inputs.get(0).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));
        goa.add(new LogData(RSEDeviceId, data.inputs.get(1).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));
        goa.add(new LogData(ReportDeviceId, data.inputs.get(2).app.getClass().getSimpleName(), new BigDecimal(simtime), key, message));

        exceptionMessage = "A message to test against";
        excApp = new ExceptionApp();
        ali = new AppLayerInput(excApp, getDi1());
        tadl = new TestAppsDataLayer();
        tadl.inputs = new ArrayList<AppLayerInput>(1);
        tadl.inputs.add(ali);
        execpTest = new SingleIntersectionAppLayer(tadl, ral, nam);
    }

    @After
    public void teardown() {
        data = null;
        ao1 = null;
        ao2 = null;
        app1 = null;
        app2 = null;
        app3 = null;
        di1 = null;
        di2 = null;
        di3 = null;
        sial = null;
        obuindex = null;
        rseindex = null;
        reportindex = null;
        valm = null;
        vmr = null;
        vcmds = null;
        vlogs = null;
        salm = null;
        smr = null;
        vcmds = null;
        slogs = null;
        ralm = null;
        rmr = null;
        rlogs = null;
        goa = null;
        exceptionMessage = null;
        excApp = null;
        ali = null;
        tadl = null;
        execpTest = null;
    }

    @Test
    public void testConstructor() {
        SingleIntersectionAppLayer layer = new SingleIntersectionAppLayer(data, ral, nam);
        assertTrue(data == layer.data);
    }

    @Test
    public void testExecApps() {
        sial.execApps(0);
        List<AppLayerOutput> outputs = data.outputs.get(0);
        for (AppLayerOutput ao : outputs) {
            assertNotNull(ao);
        }
    }

    @Test
    public void testIOBUBaseApp() {
        AppLayerOutput alo = sial.processApp(obuindex, simtime);

        assertEquals(alo.getVehCommands(), vcmds);
        assertEquals(alo.getSigCommands(), new ArrayList<SignalCommand>(0));
        assertEquals(alo.getOutMessages(), valm);
        assertEquals(alo.getLogs(), vlogs);

    }

    @Test
    public void testIRSEBaseApp() {
        AppLayerOutput alo = sial.processApp(rseindex, simtime);

        assertEquals(alo.getVehCommands(), new ArrayList<VehicleCommand>(0));
        assertEquals(alo.getSigCommands(), scmds);
        assertEquals(alo.getOutMessages(), salm);
        assertEquals(alo.getLogs(), slogs);
    }

    @Test
    public void testIReportBaseApp() {
        AppLayerOutput alo = sial.processApp(reportindex, simtime);

        assertEquals(alo.getVehCommands(), new ArrayList<VehicleCommand>(0));
        assertEquals(alo.getSigCommands(), new ArrayList<SignalCommand>(0));
        assertEquals(alo.getOutMessages(), ralm);
        assertEquals(alo.getLogs(), rlogs);
    }

    @Test
    public void testExceptionFromApp() {
        try {
            execpTest.execApps(0);
            throw new RuntimeException();
        }
        catch (SimDriverException sde) {
            assertTrue(sde.getMessage().contains(exceptionMessage));
            assertTrue(sde.getMessage().contains("TestSingleIntersectionAppLayer$ExceptionApp.performUpdate"));
        }
    }

    AppLayerOutput getAo1() {
        return null;
    }

    OBUDeviceInfo getDi1() {
        return new OBUDeviceInfo(new Vehicle(0, 0.0, 0.0, 0.0, 0.0, 0.0), new ArrayList<BasicMessage>(0), OBUDeviceId);
    }

    RSEDeviceInfo getDi2() {

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        signalMap.put(0, new SignalManager());
        laneMap.put(0, new LaneManager());
        detectorMap.put(0, new DetectorManager());

        return new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[0], RSEDeviceId, new DistanceImpl(0, 0, 0));
    }

    ReportDeviceInfo getDi3() {

        Map<Integer, IVehicleManager> vehicleMap = new HashMap<Integer, IVehicleManager>();
        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        vehicleMap.put(0, new VehicleManager());
        signalMap.put(0, new SignalManager());
        laneMap.put(0, new LaneManager());
        detectorMap.put(0, new DetectorManager());

        return new ReportDeviceInfo(vehicleMap, signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ArrayList<BasicMessage>(0), ReportDeviceId, new DistanceImpl(0, 0, 0));
    }

    private class ExceptionApp implements IOBUBaseApp {

        @Override
        public void performUpdate(OBUDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
            throw new RuntimeException(exceptionMessage);
        }

    }
}
