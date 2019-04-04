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

import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the movement event list frame.
 * 
 * @author ttevendale
 */
public class MovementEventListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        MovementEventList events = new MovementEventList(MovementEventList.MIN_LIST_SIZE);
        assertTrue(events.getMovementEventArray().length == MovementEventList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        events = new MovementEventList(MovementEventList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        MovementEventList events = new MovementEventList(MovementEventList.MAX_LIST_SIZE);
        assertTrue(events.getMovementEventArray().length == MovementEventList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        events = new MovementEventList(MovementEventList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numEvents = 12;
        MovementEventList events = new MovementEventList(numEvents);
        assertTrue(events.getMovementEventArray().length == numEvents);
    }

    @Test
    public void testEncodeUPERMin() {

        MovementEventList events = new MovementEventList(MovementEventList.MIN_LIST_SIZE);
        MovementEvent event = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        MovementEvent[] eventArray = events.getMovementEventArray();
        eventArray[0] = event;

        String listSize = "0000";
        String remainingBits = event.encodeUPER();

        assertTrue((listSize + remainingBits).equals(events.encodeUPER()));

        events = new MovementEventList(MovementEventList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        events.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        MovementEventList events = new MovementEventList(MovementEventList.MAX_LIST_SIZE);

        MovementEvent[] eventArray = events.getMovementEventArray();
        for (int i = 0; i < eventArray.length; i++) {

            MovementEvent event = new MovementEvent(MovementPhase.DARK);
            eventArray[i] = event;
            remainingBits += event.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(events.encodeUPER()));

        events = new MovementEventList(MovementEventList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        events.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        MovementEventList events = new MovementEventList();
        thrown.expect(IllegalStateException.class);
        events.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        MovementEvent event = new MovementEvent(MovementPhase.PERMISSIVE_CLEARANCE);
        String listSize = "0000";
        String remainingBits = event.encodeUPER();

        MovementEventList events = new MovementEventList();
        events.decodeUPER(listSize + remainingBits);
        MovementEvent[] eventArray = events.getMovementEventArray();
        assertTrue(MovementEventList.MIN_LIST_SIZE == eventArray.length);
        assertTrue(event.equals(eventArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        MovementEvent event = new MovementEvent(MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED);
        MovementEvent event2 = new MovementEvent(MovementPhase.PRE_MOVEMENT);

        String listSize = "1111";
        String remainingBits = event.encodeUPER();

        for (int i = 0; i < MovementEventList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += event2.encodeUPER();
        }

        MovementEventList events = new MovementEventList();
        events.decodeUPER(listSize + remainingBits);

        MovementEvent[] eventArray = events.getMovementEventArray();
        assertTrue(MovementEventList.MAX_LIST_SIZE == eventArray.length);
        assertTrue(event.equals(eventArray[0]));
        for (int i = 1; i < MovementEventList.MAX_LIST_SIZE; i++) {

            assertTrue(event2.equals(eventArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        MovementEventList events = new MovementEventList();
        thrown.expect(IllegalArgumentException.class);
        events.decodeUPER("010");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        MovementEventList events = new MovementEventList();
        thrown.expect(IllegalArgumentException.class);
        // 1011 = 12 objects, but there's none
        events.decodeUPER("10111010100");
    }

    @Test
    public void testHashCode() {

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PROTECTED_CLEARANCE);

        assertTrue(events.hashCode() == events.hashCode());

        MovementEventList events2 = new MovementEventList(2);

        assertFalse(events.hashCode() == events2.hashCode());

        events2 = new MovementEventList(1);
        events2.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PROTECTED_MOVEMENT_ALLOWED);

        assertFalse(events.hashCode() == events2.hashCode());

        events2.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PROTECTED_CLEARANCE);

        assertTrue(events.hashCode() == events2.hashCode());
    }

    @Test
    public void testEquals() {

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.STOP_AND_REMAIN);

        assertTrue(events.equals(events));
        assertFalse(events.equals(null));
        assertFalse(events.equals(new String()));

        MovementEventList events2 = new MovementEventList(2);

        assertFalse(events.equals(events2));

        events2 = new MovementEventList(1);
        events2.getMovementEventArray()[0] = new MovementEvent(MovementPhase.STOP_THEN_PROCEED);

        assertFalse(events.equals(events2));

        events2.getMovementEventArray()[0] = new MovementEvent(MovementPhase.STOP_AND_REMAIN);

        assertTrue(events.equals(events2));
    }
}
