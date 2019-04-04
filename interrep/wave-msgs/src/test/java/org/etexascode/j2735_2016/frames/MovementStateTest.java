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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.elements.SignalGroupID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the movement state frame.
 * 
 * @author ttevendale
 */
public class MovementStateTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    MovementState movementState;

    @Before
    public void init() {

        SignalGroupID signalGroup = new SignalGroupID(32);
        MovementEventList stateTimeSpeed = new MovementEventList(2);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);
        stateTimeSpeed.getMovementEventArray()[1] = new MovementEvent(MovementPhase.STOP_AND_REMAIN);
        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);
        maneuverAssistList.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(22);

        movementState = new MovementState(signalGroup, stateTimeSpeed);
        movementState.setManeuverAssistList(maneuverAssistList);
    }

    @Test
    public void testConstructor() {

        SignalGroupID signalGroup = new SignalGroupID(3);
        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState movementState = new MovementState(signalGroup, stateTimeSpeed);

        assertNull(movementState.getManeuverAssistList());
        assertTrue(signalGroup.equals(movementState.getSignalGroup()));
        assertTrue(stateTimeSpeed.equals(movementState.getStateTimeSpeed()));
    }

    @Test
    public void testConstructorNullSignalGroup() {

        thrown.expect(NullPointerException.class);
        new MovementState(null, new MovementEventList());
    }

    @Test
    public void testConstructorNullStateTimeSpeed() {

        thrown.expect(NullPointerException.class);
        new MovementState(new SignalGroupID(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int signalGroup = 55;
        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState movementState = new MovementState(signalGroup, stateTimeSpeed);

        assertNull(movementState.getManeuverAssistList());
        assertTrue(signalGroup == movementState.getSignalGroup().getValue());
        assertTrue(stateTimeSpeed.equals(movementState.getStateTimeSpeed()));
    }

    @Test
    public void testSetSignalGroup() {

        SignalGroupID signalGroup = new SignalGroupID(241);

        MovementState movementState = new MovementState();
        movementState.setSignalGroup(signalGroup);

        assertTrue(signalGroup.equals(movementState.getSignalGroup()));

        thrown.expect(NullPointerException.class);
        movementState.setSignalGroup(null);
    }

    @Test
    public void testSetSignalGroupPrimitive() {

        int signalGroup = 24;

        MovementState movementState = new MovementState();
        movementState.setSignalGroup(signalGroup);

        assertTrue(signalGroup == movementState.getSignalGroup().getValue());
    }

    @Test
    public void testSetStateTimeSpeed() {

        MovementEventList stateTimeSpeed = new MovementEventList(2);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        stateTimeSpeed.getMovementEventArray()[1] = new MovementEvent(MovementPhase.PROTECTED_CLEARANCE);

        MovementState movementState = new MovementState();
        movementState.setStateTimeSpeed(stateTimeSpeed);

        assertTrue(stateTimeSpeed.equals(movementState.getStateTimeSpeed()));

        thrown.expect(NullPointerException.class);
        movementState.setStateTimeSpeed(null);
    }

    @Test
    public void testEncodeUPERMin() {

        SignalGroupID signalGroup = new SignalGroupID(32);
        MovementEventList stateTimeSpeed = new MovementEventList(2);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        stateTimeSpeed.getMovementEventArray()[1] = new MovementEvent(MovementPhase.UNAVAILABLE);

        MovementState movementState = new MovementState(signalGroup, stateTimeSpeed);

        String movementStateOptionals = "0000";
        String remainingBits = signalGroup.encodeUPER() + stateTimeSpeed.encodeUPER();
        assertTrue((movementStateOptionals + remainingBits).equals(movementState.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        SignalGroupID signalGroup = new SignalGroupID(111);
        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PERMISSIVE_CLEARANCE);
        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);
        maneuverAssistList.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(1);

        MovementState movementState = new MovementState(signalGroup, stateTimeSpeed);
        movementState.setManeuverAssistList(maneuverAssistList);

        String movementStateOptionals = "0010";
        String remainingBits = signalGroup.encodeUPER() + stateTimeSpeed.encodeUPER() + maneuverAssistList.encodeUPER();
        assertTrue((movementStateOptionals + remainingBits).equals(movementState.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        SignalGroupID signalGroup = new SignalGroupID(32);
        MovementEventList stateTimeSpeed = new MovementEventList(2);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        stateTimeSpeed.getMovementEventArray()[1] = new MovementEvent(MovementPhase.UNAVAILABLE);

        String movementStateOptionals = "0000";

        MovementState movementState = new MovementState();
        String remainingBits = movementState.decodeUPER(movementStateOptionals + signalGroup.encodeUPER() + stateTimeSpeed.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(movementState.getManeuverAssistList());
        assertTrue(signalGroup.equals(movementState.getSignalGroup()));
        assertTrue(stateTimeSpeed.equals(movementState.getStateTimeSpeed()));
    }

    @Test
    public void testDecodeUPERMax() {

        SignalGroupID signalGroup = new SignalGroupID(111);
        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PERMISSIVE_CLEARANCE);
        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);
        maneuverAssistList.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(1);

        String movementStateOptionals = "0010";

        MovementState movementState = new MovementState();
        String remainingBits = movementState.decodeUPER(movementStateOptionals + signalGroup.encodeUPER() + stateTimeSpeed.encodeUPER() + maneuverAssistList.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(signalGroup.equals(movementState.getSignalGroup()));
        assertTrue(stateTimeSpeed.equals(movementState.getStateTimeSpeed()));
        assertTrue(maneuverAssistList.equals(movementState.getManeuverAssistList()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String movementStateOptionals = "1000";

        MovementState movementState = new MovementState();
        thrown.expect(IllegalArgumentException.class);
        movementState.decodeUPER(movementStateOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String movementStateOptionals = "0100";

        MovementState movementState = new MovementState();
        thrown.expect(IllegalArgumentException.class);
        movementState.decodeUPER(movementStateOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String movementStateOptionals = "0001";

        MovementState movementState = new MovementState();
        thrown.expect(IllegalArgumentException.class);
        movementState.decodeUPER(movementStateOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String movementStateOptionals = "010";

        MovementState movementState = new MovementState();
        thrown.expect(IllegalArgumentException.class);
        movementState.decodeUPER(movementStateOptionals);
    }

    @Test
    public void testHashCode() {

        int signalGroup = movementState.getSignalGroup().getValue();
        MovementEventList stateTimeSpeed = movementState.getStateTimeSpeed();
        ManeuverAssistList maneuverAssistList = movementState.getManeuverAssistList();

        MovementEventList diffStateTimeSpeed = new MovementEventList(stateTimeSpeed.getMovementEventArray().length + 1);
        ManeuverAssistList diffManeuverAssistList = new ManeuverAssistList(maneuverAssistList.getManeuverAssistArray().length + 1);

        MovementState movementState2 = new MovementState(signalGroup + 1, diffStateTimeSpeed);
        movementState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(movementState.hashCode() == movementState2.hashCode());
        assertTrue(movementState.hashCode() == movementState.hashCode());
        assertTrue(movementState2.hashCode() == movementState2.hashCode());

        MovementState movementState3 = new MovementState(signalGroup, stateTimeSpeed);
        movementState3.setManeuverAssistList(maneuverAssistList);

        assertTrue(movementState.hashCode() == movementState3.hashCode());
        assertFalse(movementState2.hashCode() == movementState3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(movementState.equals(movementState));
        assertFalse(movementState.equals(null));
        assertFalse(movementState.equals(new String()));

        int signalGroup = movementState.getSignalGroup().getValue();
        MovementEventList stateTimeSpeed = movementState.getStateTimeSpeed();
        ManeuverAssistList maneuverAssistList = movementState.getManeuverAssistList();

        MovementEventList diffStateTimeSpeed = new MovementEventList(stateTimeSpeed.getMovementEventArray().length + 1);
        ManeuverAssistList diffManeuverAssistList = new ManeuverAssistList(maneuverAssistList.getManeuverAssistArray().length + 1);

        // different
        MovementState movementState2 = new MovementState(signalGroup + 1, diffStateTimeSpeed);
        movementState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(movementState.equals(movementState2));

        // different signal group
        movementState2 = new MovementState(signalGroup + 1, stateTimeSpeed);
        movementState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(movementState.equals(movementState2));

        // different state time speed
        movementState2 = new MovementState(signalGroup, diffStateTimeSpeed);
        movementState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(movementState.equals(movementState2));

        // different maneuver assist list
        movementState2 = new MovementState(signalGroup, stateTimeSpeed);
        movementState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(movementState.equals(movementState2));

        // same
        movementState2 = new MovementState(signalGroup, stateTimeSpeed);
        movementState2.setManeuverAssistList(maneuverAssistList);

        assertTrue(movementState.equals(movementState2));
    }
}
