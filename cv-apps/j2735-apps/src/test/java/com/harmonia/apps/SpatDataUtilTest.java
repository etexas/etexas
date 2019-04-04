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

import java.util.LinkedList;
import java.util.List;

import org.etexascode.apps.SpatDataUtil;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.j2735.SPAT;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests SpatDataUtil.java
 * 
 * @author cdeisher
 */
public class SpatDataUtilTest {

    /** Will add app config for Spat to device */
    // AppConfig appSPAT = new AppConfig("SPAT-producer",
    // TestHarnessCase.SPAT_frequency);
    /** Uses a bsm app to test MatchApp method */
    // AppConfig appBSM = new AppConfig("BSM-producer",
    // TestHarnessCase.BMS_frequency);
    String devId = "2"; // is the vehicle id

    Double simTime = 1.00; // 1st timestep in playback data step10*1/10 sec per

    private SignalIndication expectedSignal;

    private Color color;

    private Type type;

    private State state;

    @Before
    public void setUp() throws Exception {
        expectedSignal = new SignalIndication();
        expectedSignal.setLaneId(1);
        expectedSignal.setTimeToChange(1.4);
    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testConvertLongToSignalIndication0L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 0L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication32L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 32L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication64L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 64L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication128L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 128L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication256L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 256L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication512L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 512L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1024L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1024L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2048L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2048L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4096L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.STRAIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4096L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8192L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.STRAIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8192L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16384L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.STRAIGHT_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16384L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication32768L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.STRAIGHT_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 32768L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication65536L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 65536L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication131072L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 131072L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication262144L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 262144L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication524288L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.LEFT_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 524288L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1048576L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1048576L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2097152L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2097152L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4194304L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.SOFT;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4194304L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8388608L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.RIGHT_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8388608L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16777216L() {
        color = SignalIndication.Color.GREEN;
        type = SignalIndication.Type.UTURN_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16777216L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication33554432L() {
        color = SignalIndication.Color.YELLOW;
        type = SignalIndication.Type.UTURN_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 33554432L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication67108864L() {
        color = SignalIndication.Color.RED;
        type = SignalIndication.Type.UTURN_ARROW;
        state = SignalIndication.State.STEADY;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 67108864L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication134217728L() {
        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.UTURN_ARROW;
        state = SignalIndication.State.FLASHING;
        expectedSignal.setColorIndication(color);
        expectedSignal.setTypeIndication(type);
        expectedSignal.setStateIndication(state);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 134217728L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testGetSignalInfoFromSpat() {
        SignalIndication si1 = new SignalIndication();
        si1.setLaneId(1);
        si1.setColorIndication(SignalIndication.Color.GREEN);
        si1.setStateIndication(SignalIndication.State.STEADY);
        si1.setTypeIndication(SignalIndication.Type.BALL);
        SignalIndication si2 = new SignalIndication();
        si2.setLaneId(2);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        SignalManager sm = new SignalManager();
        sm.addSignal(si1);
        sm.addSignal(si2);
        // SignalManager smi = new SignalManager(sm);
        SPATProducerApp spa = new SPATProducerApp();
        spa.getFormattedSPATMessage(0, sm);
        SPAT spat = spa.spatMessage;
        List<SignalIndication> expected = new LinkedList<SignalIndication>();
        expected.add(si1);
        expected.add(si2);

        List<SignalIndication> actual = SpatDataUtil.getSignalInfoFromSpat(spat);
        assertEquals(expected, actual);
    }

}
