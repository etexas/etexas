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
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the intersection state list frame.
 * 
 * @author ttevendale
 */
public class IntersectionStateListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        IntersectionStateList intersectionStates = new IntersectionStateList(IntersectionStateList.MIN_LIST_SIZE);
        assertTrue(intersectionStates.getIntersectionStateArray().length == IntersectionStateList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        intersectionStates = new IntersectionStateList(IntersectionStateList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        IntersectionStateList intersectionStates = new IntersectionStateList(IntersectionStateList.MAX_LIST_SIZE);
        assertTrue(intersectionStates.getIntersectionStateArray().length == IntersectionStateList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        intersectionStates = new IntersectionStateList(IntersectionStateList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numItems = 12;
        IntersectionStateList intersectionStates = new IntersectionStateList(numItems);
        assertTrue(intersectionStates.getIntersectionStateArray().length == numItems);
    }

    @Test
    public void testEncodeUPERMin() {

        IntersectionStateList intersectionStates = new IntersectionStateList(IntersectionStateList.MIN_LIST_SIZE);

        IntersectionReferenceID id = new IntersectionReferenceID(23);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidSpatAvailableAtThisTime(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        IntersectionState intersectionState = new IntersectionState(id, 38, status, states);
        IntersectionState[] intersectionStateArray = intersectionStates.getIntersectionStateArray();
        intersectionStateArray[0] = intersectionState;

        String listSize = "00000";
        String remainingBits = intersectionState.encodeUPER();

        assertTrue((listSize + remainingBits).equals(intersectionStates.encodeUPER()));

        intersectionStates = new IntersectionStateList(IntersectionStateList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        intersectionStates.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111";
        String remainingBits = "";

        IntersectionStateList intersectionStates = new IntersectionStateList(IntersectionStateList.MAX_LIST_SIZE);

        IntersectionState[] intersectionStateArray = intersectionStates.getIntersectionStateArray();
        for (int i = 0; i < intersectionStateArray.length; i++) {

            IntersectionReferenceID id = new IntersectionReferenceID(23 + 1);
            IntersectionStatusObject status = new IntersectionStatusObject();
            status.setNoValidSpatAvailableAtThisTime(true);

            MovementList states = new MovementList(1);

            MovementEventList events = new MovementEventList(1);
            events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

            MovementState state = new MovementState(1 + i, events);

            states.getMovementArray()[0] = state;

            IntersectionState intersectionState = new IntersectionState(id, 1 + i, status, states);
            intersectionStateArray[i] = intersectionState;
            remainingBits += intersectionState.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(intersectionStates.encodeUPER()));

        intersectionStates = new IntersectionStateList(IntersectionStateList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        intersectionStates.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        IntersectionStateList intersectionStates = new IntersectionStateList();
        thrown.expect(IllegalStateException.class);
        intersectionStates.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        IntersectionReferenceID id = new IntersectionReferenceID(23);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidSpatAvailableAtThisTime(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        IntersectionState intersectionState = new IntersectionState(id, 38, status, states);
        String listSize = "00000";
        String remainingBits = intersectionState.encodeUPER();

        IntersectionStateList intersectionStates = new IntersectionStateList();
        intersectionStates.decodeUPER(listSize + remainingBits);
        IntersectionState[] intersectionStateArray = intersectionStates.getIntersectionStateArray();
        assertTrue(IntersectionStateList.MIN_LIST_SIZE == intersectionStateArray.length);
        assertTrue(intersectionState.equals(intersectionStateArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        String listSize = "11111";
        String remainingBits = "";

        IntersectionStateList intersectionStatesExpected = new IntersectionStateList(IntersectionStateList.MAX_LIST_SIZE);

        for (int i = 0; i < IntersectionStateList.MAX_LIST_SIZE; i++) {

            IntersectionReferenceID id = new IntersectionReferenceID(23 + 1);
            IntersectionStatusObject status = new IntersectionStatusObject();
            status.setNoValidSpatAvailableAtThisTime(true);

            MovementList states = new MovementList(1);

            MovementEventList events = new MovementEventList(1);
            events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

            MovementState state = new MovementState(1 + i, events);

            states.getMovementArray()[0] = state;

            IntersectionState intersectionState = new IntersectionState(id, 1 + i, status, states);
            intersectionStatesExpected.getIntersectionStateArray()[i] = intersectionState;
            remainingBits += intersectionState.encodeUPER();
        }

        IntersectionStateList intersectionStates = new IntersectionStateList();
        intersectionStates.decodeUPER(listSize + remainingBits);

        assertTrue(intersectionStatesExpected.equals(intersectionStates));

    }

    @Test
    public void testDecodeUPERLessBits() {

        IntersectionStateList intersectionStates = new IntersectionStateList();
        thrown.expect(IllegalArgumentException.class);
        intersectionStates.decodeUPER("010");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        IntersectionStateList intersectionStates = new IntersectionStateList();
        thrown.expect(IllegalArgumentException.class);
        // 11111 = 32 objects, but there's none
        intersectionStates.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        IntersectionStateList intersectionStates = new IntersectionStateList(1);
        intersectionStates.getIntersectionStateArray()[0] = createIntersectionState();

        assertTrue(intersectionStates.hashCode() == intersectionStates.hashCode());

        IntersectionStateList intersectionStates2 = new IntersectionStateList(2);

        assertFalse(intersectionStates.hashCode() == intersectionStates2.hashCode());

        intersectionStates2 = new IntersectionStateList(1);
        intersectionStates2.getIntersectionStateArray()[0] = createIntersectionState2();

        assertFalse(intersectionStates.hashCode() == intersectionStates2.hashCode());

        intersectionStates2.getIntersectionStateArray()[0] = createIntersectionState();

        assertTrue(intersectionStates.hashCode() == intersectionStates2.hashCode());
    }

    @Test
    public void testEquals() {

        IntersectionStateList intersectionStates = new IntersectionStateList(1);
        intersectionStates.getIntersectionStateArray()[0] = createIntersectionState();

        assertTrue(intersectionStates.equals(intersectionStates));
        assertFalse(intersectionStates.equals(null));
        assertFalse(intersectionStates.equals(new String()));

        IntersectionStateList intersectionStates2 = new IntersectionStateList(2);

        assertFalse(intersectionStates.equals(intersectionStates2));

        intersectionStates2 = new IntersectionStateList(1);
        intersectionStates2.getIntersectionStateArray()[0] = createIntersectionState2();

        assertFalse(intersectionStates.equals(intersectionStates2));

        intersectionStates2.getIntersectionStateArray()[0] = createIntersectionState();

        assertTrue(intersectionStates.equals(intersectionStates2));
    }

    private IntersectionState createIntersectionState() {

        IntersectionReferenceID id = new IntersectionReferenceID(2);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setPreemptActive(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.STOP_AND_REMAIN);

        MovementState state = new MovementState(1, events);

        states.getMovementArray()[0] = state;

        return new IntersectionState(id, 20, status, states);
    }

    private IntersectionState createIntersectionState2() {

        IntersectionReferenceID id = new IntersectionReferenceID(5);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setFixedTimeOperation(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(2);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PERMISSIVE_CLEARANCE);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);

        MovementState state = new MovementState(1, events);

        states.getMovementArray()[0] = state;

        return new IntersectionState(id, 10, status, states);
    }
}
