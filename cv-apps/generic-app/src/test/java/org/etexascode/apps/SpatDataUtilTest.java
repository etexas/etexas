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
package org.etexascode.apps;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.j2735.IntersectionState;
import org.etexascode.j2735.IntersectionState.States;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.SPAT.IntersectionStates;
import org.etexascode.j2735.util.SignalLightState;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SpatDataUtilTest {

    private SignalIndication expectedSignal;

    @Before
    public void setUp() throws Exception {

        expectedSignal = new SignalIndication();
        expectedSignal.setLaneId(1);
        expectedSignal.setTimeToChange(1.4);
    }

    @After
    public void tearDown() throws Exception {

        expectedSignal = null;
    }

    @Test
    public void testConvertLongToSignalIndication0L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 0L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.BALL);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication32L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 32L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication64L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 64L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication128L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 128L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication256L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 256L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication512L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 512L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1024L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1024L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2048L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2048L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4096L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4096L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8192L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8192L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16384L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16384L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication32768L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 32768L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication65536L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 65536L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication131072L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 131072L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication262144L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 262144L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication524288L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.LEFT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 524288L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication1048576L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 1048576L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication2097152L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 2097152L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication4194304L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.SOFT);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 4194304L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication8388608L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 8388608L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication16777216L() {

        expectedSignal.setColorIndication(SignalIndication.Color.GREEN);
        expectedSignal.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 16777216L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication33554432L() {

        expectedSignal.setColorIndication(SignalIndication.Color.YELLOW);
        expectedSignal.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 33554432L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication67108864L() {

        expectedSignal.setColorIndication(SignalIndication.Color.RED);
        expectedSignal.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.STEADY);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 67108864L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testConvertLongToSignalIndication134217728L() {

        expectedSignal.setColorIndication(SignalIndication.Color.NONE);
        expectedSignal.setTypeIndication(SignalIndication.Type.UTURN_ARROW);
        expectedSignal.setStateIndication(SignalIndication.State.FLASHING);

        SignalIndication actual = SpatDataUtil.convertLongToSignalIndication(1, 1.4, 134217728L);
        assertEquals(expectedSignal, actual);
    }

    @Test
    public void testGetSignalInfoFromSpat() {

        SPAT spat = new SPAT();
        spat.setMsgID("13");

        IntersectionStates intersectionStates = new IntersectionStates();

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setId(new byte[] { (byte)0x00, (byte)0x01 });
        intersectionState.setStatus(new byte[] { (byte)0x00 });

        States states = new States();
        MovementState movementState = new MovementState();
        movementState.setLaneSet(new byte[] { (byte)0x01 });
        movementState.setCurrState(SignalLightState.BALL_GREEN);
        movementState.setTimeToChange(100);
        states.getMovementState().add(movementState);

        SignalIndication si1 = new SignalIndication();
        si1.setLaneId(1);
        si1.setColorIndication(SignalIndication.Color.GREEN);
        si1.setStateIndication(SignalIndication.State.STEADY);
        si1.setTypeIndication(SignalIndication.Type.BALL);
        si1.setTimeToChange(10);

        movementState = new MovementState();
        movementState.setLaneSet(new byte[] { (byte)0x02 });
        movementState.setCurrState(SignalLightState.BALL_RED);
        movementState.setTimeToChange(50);
        states.getMovementState().add(movementState);

        SignalIndication si2 = new SignalIndication();
        si2.setLaneId(2);
        si2.setColorIndication(SignalIndication.Color.RED);
        si2.setStateIndication(SignalIndication.State.STEADY);
        si2.setTypeIndication(SignalIndication.Type.BALL);
        si2.setTimeToChange(5);

        intersectionState.setStates(states);

        intersectionStates.getIntersectionState().add(intersectionState);
        spat.setIntersectionStates(intersectionStates);

        List<SignalIndication> actualSignals = SpatDataUtil.getSignalInfoFromSpat(spat);
        List<SignalIndication> expectedSignals = new ArrayList<SignalIndication>();
        expectedSignals.add(si1);
        expectedSignals.add(si2);

        assertTrue(actualSignals.size() == expectedSignals.size());
        assertTrue(actualSignals.get(0).equals(expectedSignals.get(0)));
        assertTrue(actualSignals.get(1).equals(expectedSignals.get(1)));

    }
}