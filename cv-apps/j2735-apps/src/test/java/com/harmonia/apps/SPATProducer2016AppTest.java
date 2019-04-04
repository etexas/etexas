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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import org.etexascode.apps.RSEDevice;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.frames.IntersectionState;
import org.etexascode.j2735_2016.frames.MovementEvent;
import org.etexascode.j2735_2016.frames.MovementState;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.etexascode.j2735_2016.messages.SPAT;
import org.junit.Test;

/**
 * Unit tests for the SPATProducer2016App.
 * 
 * @author ttevendale
 */
public class SPATProducer2016AppTest {

    @Test
    public void testGetAppName() {

        assertTrue(SPATProducer2016App.APP_NAME_SPAT_PRODUCER_2016_APP.equals(new SPATProducer2016App().getAppName()));
    }

    @Test
    public void testInitFrequency() {

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(1, createExam1SignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));

        spatProducer.init(new String[] { "1.0" });

        // The device will send a message.
        spatProducer.performUpdate(device, null, null, 0.0, null);
        assertFalse(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will not send a message since 1 second has not elapsed since the last time.
        spatProducer.performUpdate(device, null, null, 0.5, null);
        assertTrue(device.getAppMessages().isEmpty());

        device.getAppMessages().clear();

        // The device will send a message since its been a second.
        spatProducer.performUpdate(device, null, null, 1.0, null);
        assertFalse(device.getAppMessages().isEmpty());
    }

    @Test
    public void testPerformUpdateExam13() {

        int exam13IntersectionId = 1;

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(exam13IntersectionId, createExam13SignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        spatProducer.performUpdate(device, null, null, 80.0, null);

        String bytes = new String((byte[])device.getAppMessages().get(0).getData());
        SPAT spat = MessageFrame.decodeSPAT(bytes);

        // 80.0 converted to minutes and rounded down equals 1
        assertTrue(spat.getTimeStamp().getValue() == 1);

        IntersectionState[] intersections = spat.getIntersections().getIntersectionStateArray();
        assertTrue(intersections.length == signalMap.size());

        IntersectionState exam13 = intersections[0];
        assertTrue(exam13.getId().getId().getValue() == exam13IntersectionId);

        MovementState[] movementStates = exam13.getStates().getMovementArray();
        Arrays.sort(movementStates, new sortMovementStateBySignalGroupId());

        // 1 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(1, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[0], 1, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 2 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(2, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[1], 2, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 3 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(3, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[2], 3, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 7 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(7, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[3], 7, MovementPhase.STOP_AND_REMAIN, 82);

        // 8 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(8, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[4], 8, MovementPhase.STOP_AND_REMAIN, 82);

        // 9 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(9, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[5], 9, MovementPhase.STOP_AND_REMAIN, 82);

        // 13 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(13, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[6], 13, MovementPhase.STOP_AND_REMAIN, 49);

        // 14 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(14, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[7], 14, MovementPhase.STOP_AND_REMAIN, 49);

        // 15 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(15, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[8], 15, MovementPhase.STOP_AND_REMAIN, 49);

        // 19 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(19, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[9], 19, MovementPhase.STOP_AND_REMAIN, 16);

        // 20 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(20, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[10], 20, MovementPhase.STOP_AND_REMAIN, 16);

        // 21 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(21, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[11], 21, MovementPhase.STOP_AND_REMAIN, 16);

        // 25 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(25, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[12], 25, MovementPhase.STOP_AND_REMAIN, 16);

        // 26 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(26, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[13], 26, MovementPhase.STOP_AND_REMAIN, 16);

        // 27 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(27, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[14], 27, MovementPhase.STOP_AND_REMAIN, 16);

        // 31 = signalGroupID, GREEN LEFT_ARROW = PROTECTED_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(31, Type.LEFT_ARROW, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[15], 31, MovementPhase.PROTECTED_MOVEMENT_ALLOWED, 13);

        // 31 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(31, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[16], 31, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 32 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(32, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[17], 32, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 33 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(33, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[18], 33, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);
    }

    @Test
    public void testPerformUpdateExam1() {

        int exam1IntersectionId = 2;

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(exam1IntersectionId, createExam1SignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        spatProducer.performUpdate(device, null, null, 400.0, null);

        String bytes = new String((byte[])device.getAppMessages().get(0).getData());
        SPAT spat = MessageFrame.decodeSPAT(bytes);

        // 400.0 converted to minutes and rounded down equals 6
        assertTrue(spat.getTimeStamp().getValue() == 6);

        IntersectionState[] intersections = spat.getIntersections().getIntersectionStateArray();
        assertTrue(intersections.length == signalMap.size());

        IntersectionState exam1 = intersections[0];
        assertTrue(exam1.getId().getId().getValue() == exam1IntersectionId);

        MovementState[] movementStates = exam1.getStates().getMovementArray();
        Arrays.sort(movementStates, new sortMovementStateBySignalGroupId());

        // 1 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(1, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[0], 1, MovementPhase.UNAVAILABLE, 0);

        // 2 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(2, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[1], 2, MovementPhase.UNAVAILABLE, 0);

        // 5 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(5, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[2], 5, MovementPhase.UNAVAILABLE, 0);

        // 6 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(6, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[3], 6, MovementPhase.UNAVAILABLE, 0);

        // 9 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(9, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[4], 9, MovementPhase.UNAVAILABLE, 0);

        // 10 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(10, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[5], 10, MovementPhase.UNAVAILABLE, 0);

        // 13 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(13, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[6], 13, MovementPhase.UNAVAILABLE, 0);

        // 14 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(14, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[7], 14, MovementPhase.UNAVAILABLE, 0);
    }

    @Test
    public void testPerformUpdateMultipleExams() {

        int exam13IntersectionId = 1;
        int exam1IntersectionId = 2;

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(exam13IntersectionId, createExam13SignalManager());
        signalMap.put(exam1IntersectionId, createExam1SignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        spatProducer.performUpdate(device, null, null, 8000.0, null);

        String bytes = new String((byte[])device.getAppMessages().get(0).getData());
        SPAT spat = MessageFrame.decodeSPAT(bytes);

        // 8000.0 converted to minutes and rounded down equals 133
        assertTrue(spat.getTimeStamp().getValue() == 133);

        IntersectionState[] intersections = spat.getIntersections().getIntersectionStateArray();
        assertTrue(intersections.length == signalMap.size());

        IntersectionState exam13 = intersections[0];
        assertTrue(exam13.getId().getId().getValue() == exam13IntersectionId);

        MovementState[] movementStates = exam13.getStates().getMovementArray();
        Arrays.sort(movementStates, new sortMovementStateBySignalGroupId());

        // 1 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(1, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[0], 1, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 2 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(2, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[1], 2, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 3 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(3, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[2], 3, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 7 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(7, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[3], 7, MovementPhase.STOP_AND_REMAIN, 82);

        // 8 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(8, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[4], 8, MovementPhase.STOP_AND_REMAIN, 82);

        // 9 = signalGroupID, RED BALL = STOP_AND_REMAIN, 82 = minTimeToChange
        // createSignal(9, Type.BALL, State.STEADY, Color.RED, 82)
        testMovementState(movementStates[5], 9, MovementPhase.STOP_AND_REMAIN, 82);

        // 13 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(13, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[6], 13, MovementPhase.STOP_AND_REMAIN, 49);

        // 14 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(14, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[7], 14, MovementPhase.STOP_AND_REMAIN, 49);

        // 15 = signalGroupID, RED BALL = STOP_AND_REMAIN, 49 = minTimeToChange
        // createSignal(15, Type.BALL, State.STEADY, Color.RED, 49)
        testMovementState(movementStates[8], 15, MovementPhase.STOP_AND_REMAIN, 49);

        // 19 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(19, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[9], 19, MovementPhase.STOP_AND_REMAIN, 16);

        // 20 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(20, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[10], 20, MovementPhase.STOP_AND_REMAIN, 16);

        // 21 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(21, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[11], 21, MovementPhase.STOP_AND_REMAIN, 16);

        // 25 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(25, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[12], 25, MovementPhase.STOP_AND_REMAIN, 16);

        // 26 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(26, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[13], 26, MovementPhase.STOP_AND_REMAIN, 16);

        // 27 = signalGroupID, RED BALL = STOP_AND_REMAIN, 16 = minTimeToChange
        // createSignal(27, Type.BALL, State.STEADY, Color.RED, 16)
        testMovementState(movementStates[14], 27, MovementPhase.STOP_AND_REMAIN, 16);

        // 31 = signalGroupID, GREEN LEFT_ARROW = PROTECTED_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(31, Type.LEFT_ARROW, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[15], 31, MovementPhase.PROTECTED_MOVEMENT_ALLOWED, 13);

        // 31 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(31, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[16], 31, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 32 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(32, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[17], 32, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        // 33 = signalGroupID, GREEN BALL = PERMISSIVE_MOVEMENT_ALLOWED, 13 = minTimeToChange
        // createSignal(33, Type.BALL, State.STEADY, Color.GREEN, 13)
        testMovementState(movementStates[18], 33, MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED, 13);

        IntersectionState exam1 = intersections[1];
        assertTrue(exam1.getId().getId().getValue() == exam1IntersectionId);

        movementStates = exam1.getStates().getMovementArray();
        Arrays.sort(movementStates, new sortMovementStateBySignalGroupId());

        // 1 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(1, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[0], 1, MovementPhase.UNAVAILABLE, 0);

        // 2 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(2, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[1], 2, MovementPhase.UNAVAILABLE, 0);

        // 5 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(5, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[2], 5, MovementPhase.UNAVAILABLE, 0);

        // 6 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(6, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[3], 6, MovementPhase.UNAVAILABLE, 0);

        // 9 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(9, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[4], 9, MovementPhase.UNAVAILABLE, 0);

        // 10 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(10, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[5], 10, MovementPhase.UNAVAILABLE, 0);

        // 13 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(13, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[6], 13, MovementPhase.UNAVAILABLE, 0);

        // 14 = signalGroupID, UNCONTROLLED = UNAVAILABLE, 0 = minTimeToChange
        // createSignal(14, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0)
        testMovementState(movementStates[7], 14, MovementPhase.UNAVAILABLE, 0);
    }

    @Test
    public void testPerformUpdateAllValidSignals() {

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(1, createAllMovementPhaseSignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        spatProducer.performUpdate(device, null, null, 80.0, null);

        String bytes = new String((byte[])device.getAppMessages().get(0).getData());

        SPAT spat = MessageFrame.decodeSPAT(bytes);
        MovementState[] movements = spat.getIntersections().getIntersectionStateArray()[0].getStates().getMovementArray();

        boolean hasCautionConflictingTraffic = false;
        boolean hasPermissiveClearance = false;
        boolean hasPermissiveMovementAllowed = false;
        boolean hasProtectedClearance = false;
        boolean hasProtectedMovementAllwed = false;
        boolean hasStopAndRemain = false;
        boolean hasStopThenProceed = false;
        boolean hasUnavailable = false;

        for (int i = 0; i < movements.length; i++) {

            MovementPhase eventPhase = movements[i].getStateTimeSpeed().getMovementEventArray()[0].getEventState().getEnumeration();

            switch (eventPhase) {

                case CAUTION_CONFLICTING_TRAFFIC:
                    hasCautionConflictingTraffic = true;
                    break;
                case PERMISSIVE_CLEARANCE:
                    hasPermissiveClearance = true;
                    break;
                case PERMISSIVE_MOVEMENT_ALLOWED:
                    hasPermissiveMovementAllowed = true;
                    break;
                case PROTECTED_CLEARANCE:
                    hasProtectedClearance = true;
                    break;
                case PROTECTED_MOVEMENT_ALLOWED:
                    hasProtectedMovementAllwed = true;
                    break;
                case STOP_AND_REMAIN:
                    hasStopAndRemain = true;
                    break;
                case STOP_THEN_PROCEED:
                    hasStopThenProceed = true;
                    break;
                case UNAVAILABLE:
                    hasUnavailable = true;
                    break;
                default:
                    fail("No other event phase should happen");
            }
        }

        assertTrue(hasCautionConflictingTraffic);
        assertTrue(hasPermissiveClearance);
        assertTrue(hasPermissiveMovementAllowed);
        assertTrue(hasProtectedClearance);
        assertTrue(hasProtectedMovementAllwed);
        assertTrue(hasStopAndRemain);
        assertTrue(hasStopThenProceed);
        assertTrue(hasUnavailable);
    }

    @Test
    public void testPerformUpdateWithUnavailableSignals() {

        Map<Integer, ISignalManager> signalMap = new HashMap<Integer, ISignalManager>(1);
        signalMap.put(1, createUnavailableSignalManager());
        SPATProducer2016App spatProducer = new SPATProducer2016App();

        RSEDevice device = new RSEDevice(
                new RSEDeviceInfo(signalMap, new HashMap<Integer, ILaneManager>(), new HashMap<Integer, IDetectorManager>(), new ArrayList<BasicMessage>(), new ReferencePoint[] {}, 123, null));
        spatProducer.performUpdate(device, null, null, 8000.0, null);

        String bytes = new String((byte[])device.getAppMessages().get(0).getData());

        SPAT spat = MessageFrame.decodeSPAT(bytes);
        MovementState[] movements = spat.getIntersections().getIntersectionStateArray()[0].getStates().getMovementArray();

        for (int i = 0; i < movements.length; i++) {

            MovementPhase eventPhase = movements[i].getStateTimeSpeed().getMovementEventArray()[0].getEventState().getEnumeration();

            if (!eventPhase.equals(MovementPhase.UNAVAILABLE)) {

                fail("All of the movement phases must be unavailable");
            }
        }
    }

    /**
     * Creates a signal manager that was based off the Exam 13 TEXAS signals state.
     * 
     * @return The signal manager.
     */
    private ISignalManager createExam13SignalManager() {

        SignalManager signalManager = new SignalManager();

        signalManager.addSignal(createSignal(1, Type.BALL, State.STEADY, Color.GREEN, 13));
        signalManager.addSignal(createSignal(2, Type.BALL, State.STEADY, Color.GREEN, 13));
        signalManager.addSignal(createSignal(3, Type.BALL, State.STEADY, Color.GREEN, 13));

        signalManager.addSignal(createSignal(7, Type.BALL, State.STEADY, Color.RED, 82));
        signalManager.addSignal(createSignal(8, Type.BALL, State.STEADY, Color.RED, 82));
        signalManager.addSignal(createSignal(9, Type.BALL, State.STEADY, Color.RED, 82));

        signalManager.addSignal(createSignal(13, Type.BALL, State.STEADY, Color.RED, 49));
        signalManager.addSignal(createSignal(14, Type.BALL, State.STEADY, Color.RED, 49));
        signalManager.addSignal(createSignal(15, Type.BALL, State.STEADY, Color.RED, 49));

        signalManager.addSignal(createSignal(19, Type.BALL, State.STEADY, Color.RED, 16));
        signalManager.addSignal(createSignal(20, Type.BALL, State.STEADY, Color.RED, 16));
        signalManager.addSignal(createSignal(21, Type.BALL, State.STEADY, Color.RED, 16));
        signalManager.addSignal(createSignal(25, Type.BALL, State.STEADY, Color.RED, 16));
        signalManager.addSignal(createSignal(26, Type.BALL, State.STEADY, Color.RED, 16));
        signalManager.addSignal(createSignal(27, Type.BALL, State.STEADY, Color.RED, 16));

        signalManager.addSignal(createSignal(31, Type.LEFT_ARROW, State.STEADY, Color.GREEN, 13));
        signalManager.addSignal(createSignal(31, Type.BALL, State.STEADY, Color.GREEN, 13));
        signalManager.addSignal(createSignal(32, Type.BALL, State.STEADY, Color.GREEN, 13));
        signalManager.addSignal(createSignal(33, Type.BALL, State.STEADY, Color.GREEN, 13));

        return signalManager;
    }

    /**
     * Creates a signal manager that was based off the Exam 1 TEXAS signals state.
     * 
     * @return The signal manager.
     */
    private ISignalManager createExam1SignalManager() {

        SignalManager signalManager = new SignalManager();

        signalManager.addSignal(createSignal(1, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(2, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(5, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(6, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(9, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(10, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(13, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));
        signalManager.addSignal(createSignal(14, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));

        return signalManager;
    }

    /**
     * Creates a signal manager which will contain all the movement phases used by the
     * SPATProducer2016App.
     * 
     * @return The signal manager.
     */
    private ISignalManager createAllMovementPhaseSignalManager() {

        SignalManager signalManager = new SignalManager();

        signalManager.addSignal(createSignal(1, Type.UNCONTROLLED, State.STEADY, Color.NONE, 0));

        signalManager.addSignal(createSignal(2, Type.BALL, State.FLASHING, Color.RED, 10.5));
        signalManager.addSignal(createSignal(3, Type.BALL, State.STEADY, Color.RED, 10));

        signalManager.addSignal(createSignal(4, Type.BALL, State.FLASHING, Color.YELLOW, 10));
        signalManager.addSignal(createSignal(5, Type.BALL, State.STEADY, Color.YELLOW, 10));
        signalManager.addSignal(createSignal(6, Type.LEFT_ARROW, State.STEADY, Color.YELLOW, 10));

        signalManager.addSignal(createSignal(7, Type.BALL, State.STEADY, Color.GREEN, 10));
        signalManager.addSignal(createSignal(8, Type.LEFT_ARROW, State.STEADY, Color.GREEN, 10));

        return signalManager;
    }

    /**
     * Creates a signal manager with multiple different signals would will should make a
     * MovementPhase of unavailable.
     * 
     * @return The signal manager.
     */
    private ISignalManager createUnavailableSignalManager() {

        SignalManager signalManager = new SignalManager();

        signalManager.addSignal(createSignal(1, Type.UNCONTROLLED, State.STEADY, Color.RED, 0));
        signalManager.addSignal(createSignal(2, Type.STOP_SIGN, State.STEADY, Color.RED, 0));
        signalManager.addSignal(createSignal(3, Type.UNKNOWN, State.STEADY, Color.RED, 0));
        signalManager.addSignal(createSignal(4, Type.YIELD_SIGN, State.STEADY, Color.RED, 0));

        signalManager.addSignal(createSignal(5, Type.BALL, State.SOFT, Color.RED, 0));
        signalManager.addSignal(createSignal(6, Type.BALL, State.SOFT, Color.YELLOW, 0));

        signalManager.addSignal(createSignal(8, Type.BALL, State.STEADY, Color.NONE, 0));

        return signalManager;
    }

    /**
     * Creates a signal.
     * 
     * @param laneId The lane ID the signal is connected to.
     * @param type The type of the signal.
     * @param state The state of the signal.
     * @param color The color of the signal.
     * @param timeToChange The time left before the signal changes.
     * @return The signal.
     */
    private SignalIndication createSignal(int laneId, Type type, State state, Color color, double timeToChange) {

        SignalIndication signal = new SignalIndication();
        signal.setLaneId(laneId);
        signal.setTypeIndication(type);
        signal.setStateIndication(state);
        signal.setColorIndication(color);
        signal.setTimeToChange(timeToChange);

        return signal;
    }

    /**
     * Tests that a movement state matches the provided signald ID, event phase, and time to change.
     * NOTE: This method was only created to reduce the amount of lines needed to test a movement
     * state, since there are many movement states being tested in this class
     * 
     * @param movementState The movement state to test.
     * @param signalId The signal ID to test against the movement state.
     * @param eventPhase The event phase to test against the movement state.
     * @param timeToChange The time to change to test against the movement state.
     */
    private void testMovementState(MovementState movementState, int signalId, MovementPhase eventPhase, int timeToChange) {

        assertTrue(movementState.getSignalGroup().getValue() == signalId);

        MovementEvent event = movementState.getStateTimeSpeed().getMovementEventArray()[0];
        assertTrue(event.getEventState().getEnumeration().equals(eventPhase));
        assertTrue(event.getTiming().getMinEndTime().getValue() == timeToChange);
    }

    /**
     * A class which sorts movement states by their signal group ID using the Comparator interface.
     * 
     * @author ttevendale
     */
    private class sortMovementStateBySignalGroupId implements Comparator<MovementState> {

        @Override
        public int compare(MovementState state1, MovementState state2) {

            return state1.getSignalGroup().getValue() - state2.getSignalGroup().getValue();
        }
    }
}