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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.RSEDevice;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.interrep.datamodel.DistanceImpl;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.util.SignalLightState;
import org.etexascode.test.TestHarnessCase;
import org.etexascode.test.TestInterRep;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author ablatt
 * @author bmauldon
 */
public class SPATProducerAppTest {

    /** Create lists of Device Apps. */
    List<IConnectedVehicleApp<?>> deviceApps = new LinkedList<IConnectedVehicleApp<?>>();

    /** Create test harness object */
    TestHarnessCase thc = new TestHarnessCase();

    /** SPAT Producer App. */
    SPATProducerApp app = null;

    /** The RSE Device. */
    RSEDevice dev = null;

    /** The signal manager. */
    SignalManager sm = null;

    @Before
    public void setUp() throws Exception {
        // Create the apps.
        app = new SPATProducerApp();
        app.init(new String[] { "true", "1.0" });
        deviceApps.add(app);

        // Create test InterRep.
        TestInterRep testInterRep = null;
        try {
            testInterRep = thc.getTestInterRep(true);
        }
        catch (Exception e) {}

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>();
        signalMap.put(testInterRep.getId(), testInterRep.getSignalManager());
        Map<Integer, ILaneManager> laneMap = new HashMap<Integer, ILaneManager>();
        laneMap.put(testInterRep.getId(), testInterRep.getLaneManager());
        Map<Integer, IDetectorManager> detectorMap = new HashMap<Integer, IDetectorManager>();
        detectorMap.put(testInterRep.getId(), testInterRep.getDetectorManager());

        RSEDeviceInfo rdi = new RSEDeviceInfo(signalMap, laneMap, detectorMap, new ArrayList<BasicMessage>(0), new ReferencePoint[] {}, 2, new DistanceImpl());
        dev = new RSEDevice(rdi);
        // app.update(testInterRep.getSimTime(), rdi);
    }

    @After
    public void tearDown() throws Exception {
        app = null;
        thc = null;
    }

    @Test
    public void testLifecycle() {
        String[] str = new String[2];
        str[0] = "true";
        str[1] = "2.5";
        app.init(str);
        assertEquals(app.frequency, 2.5, 0.001);
        app.appShutdown(null);
    }

    @Test
    public void testGetAppId() {
        assertEquals(SPATProducerApp.APP_NAME_SPAT_PRODUCER_APP, app.getAppName());
    }

    @Test
    public void testGetFormattedSPATMessage1() {
        SignalManager sm = new SignalManager();
        SignalIndication si = new SignalIndication();

        si.setLaneId(42);
        si.setTimeToChange(15.0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        sm.addSignal(si);

        SPATProducerApp spa = new SPATProducerApp();
        spa.getFormattedSPATMessage(0, sm);
        SPAT res = spa.spatMessage;
        List<MovementState> movementStates = res.getIntersectionStates().getIntersectionState().get(0).getStates().getMovementState();

        assertEquals(1, movementStates.size());
        assertEquals(42, movementStates.get(0).getLaneSet()[0]);
        assertEquals(150, movementStates.get(0).getTimeToChange());
        assertEquals(SignalLightState.BALL_GREEN, (long)movementStates.get(0).getCurrState());
        assertNull(movementStates.get(0).getYellState());
    }

    @Test
    public void testGetFormattedSPATMessage2() {
        SPATProducerApp spa = new SPATProducerApp();
        spa.spatMessage = null;
        SignalManager sm = new SignalManager();
        SignalIndication si = new SignalIndication();

        si.setLaneId(42);
        si.setTimeToChange(15.0);
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setTypeIndication(SignalIndication.Type.BALL);
        sm.addSignal(si);

        spa.getFormattedSPATMessage(0, sm);
        SPAT res = spa.spatMessage;
        List<MovementState> movementStates = res.getIntersectionStates().getIntersectionState().get(0).getStates().getMovementState();

        assertEquals(1, movementStates.size());
        assertEquals(42, movementStates.get(0).getLaneSet()[0]);
        assertEquals(150, movementStates.get(0).getTimeToChange());
        assertEquals(SignalLightState.BALL_GREEN, (long)movementStates.get(0).getCurrState());
        assertNull(movementStates.get(0).getYellState());
    }

    @Test
    public void testConstructor() {
        SPATProducerApp app = new SPATProducerApp();
        assertTrue(app instanceof SPATProducerApp);
    }

    @Test
    public void testInit() {
        app.init(new String[] { "true", "0.42" });
        assertEquals(0.42, app.frequency, 0.05);
    }

    @Test
    public void testPerformUpdate1() {
        app.performUpdate(dev, new Object[] {}, null, 0.0, null);
        List<BasicMessage> res = dev.getAppMessages();
        assertNotNull(res);
    }

    @Test
    public void testPerformUpdate2() {
        app.performUpdate(dev, new Object[] {}, null, 0.0, null);
        List<BasicMessage> res = dev.getAppMessages();
        dev.resetAppMessages();
        app.performUpdate(dev, new Object[] {}, null, 0.5, null);
        res = dev.getAppMessages();
        assertEquals(0, res.size());
    }

    @Test
    public void testPerformUpdate3() {

        app.performUpdate(dev, new Object[] {}, null, 0.0, null);
        dev.resetAppMessages();
        app.performUpdate(dev, new Object[] {}, null, 1.5, null);
        List<BasicMessage> res = dev.getAppMessages();
        assertEquals(1, res.size());
    }

    @Test
    public void testConvertToSignalState1() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.STOP_SIGN);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState2() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.YIELD_SIGN);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState3() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.UNCONTROLLED);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState4() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.UNKNOWN);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState5() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.BALL_GREEN, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState6() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.BALL_GREEN | SignalLightState.BALL_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState7() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState8() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.BALL_YELLOW, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState9() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.BALL_YELLOW | SignalLightState.BALL_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState10() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState11() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.BALL_RED, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState12() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);
        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.BALL_RED | SignalLightState.BALL_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState13() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState14() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.LEFT_ARROW_GREEN, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState15() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.LEFT_ARROW_GREEN | SignalLightState.LEFT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState16() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState17() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.LEFT_ARROW_YELLOW, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState18() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.LEFT_ARROW_YELLOW | SignalLightState.LEFT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState19() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState20() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.LEFT_ARROW_RED, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState21() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.LEFT_ARROW_RED | SignalLightState.LEFT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState22() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState23() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.RIGHT_ARROW_GREEN, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState24() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.RIGHT_ARROW_GREEN | SignalLightState.RIGHT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState25() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState26() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.RIGHT_ARROW_YELLOW, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState27() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.RIGHT_ARROW_YELLOW | SignalLightState.RIGHT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState28() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState29() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.STRAIGHT_GREEN, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState30() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.STRAIGHT_GREEN | SignalLightState.STRAIGHT_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState31() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState32() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.STRAIGHT_YELLOW, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState33() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.STRAIGHT_YELLOW | SignalLightState.STRAIGHT_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState34() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState35() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.STRAIGHT_RED, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState36() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.STRAIGHT_RED | SignalLightState.STRAIGHT_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState37() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState38() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.UTURN_ARROW_GREEN, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState39() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(SignalLightState.UTURN_ARROW_GREEN | SignalLightState.UTURN_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState40() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.GREEN);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState41() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.UTURN_ARROW_YELLOW, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState42() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(SignalLightState.UTURN_ARROW_YELLOW | SignalLightState.UTURN_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState43() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState44() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.UTURN_ARROW_RED, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState45() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.UTURN_ARROW_RED | SignalLightState.UTURN_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState46() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState47() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState48() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.BALL);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState49() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState50() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState51() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.NONE);

        assertEquals(SignalLightState.DARK, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState52() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.STEADY);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.RIGHT_ARROW_RED, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState53() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.FLASHING);
        si.setColorIndication(SignalIndication.Color.RED);

        assertEquals(SignalLightState.RIGHT_ARROW_RED | SignalLightState.RIGHT_ARROW_FLASHING, app.convertToSignalState(si, tempSize).lightState);
    }

    @Test
    public void testConvertToSignalState54() {
        SignalIndication si = new SignalIndication();
        Double tempSize = new Double(0.0);

        si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        si.setLaneId(42);
        si.setTimeToChange(50);
        si.setStateIndication(SignalIndication.State.SOFT);
        si.setColorIndication(SignalIndication.Color.YELLOW);

        assertEquals(0, app.convertToSignalState(si, tempSize).lightState);
    }
}