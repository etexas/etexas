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

import org.etexascode.j2735_2016.elements.AdvisorySpeedType;
import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.etexascode.j2735_2016.elements.MovementPhaseState;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the movement event frame.
 * 
 * @author ttevendale
 */
public class MovementEventTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    MovementEvent event;

    @Before
    public void init() {

        MovementPhase eventState = MovementPhase.DARK;
        TimeChangeDetails timing = new TimeChangeDetails(1);
        timing.setStartTime(10);
        timing.setMaxEndTime(38);
        timing.setLikelyTime(2);
        timing.setConfidence(3);
        timing.setNextTime(80);
        AdvisorySpeedList speeds = new AdvisorySpeedList(1);
        speeds.getAdvisorySpeedArray()[0] = new AdvisorySpeed(new AdvisorySpeedType(Advisory.NONE));

        event = new MovementEvent(eventState);
        event.setTiming(timing);
        event.setSpeeds(speeds);
    }

    @Test
    public void testConstructor() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.PERMISSIVE_CLEARANCE);

        MovementEvent event = new MovementEvent(eventState);

        assertNull(event.getTiming());
        assertNull(event.getSpeeds());
        assertTrue(eventState.equals(event.getEventState()));

        thrown.expect(NullPointerException.class);
        new MovementEvent((MovementPhaseState)null);
    }

    @Test
    public void testConstructorPrimitive() {

        MovementPhase eventState = MovementPhase.PROTECTED_CLEARANCE;

        MovementEvent event = new MovementEvent(eventState);

        assertTrue(eventState.equals(event.getEventState().getEnumeration()));

        thrown.expect(NullPointerException.class);
        new MovementEvent((MovementPhase)null);
    }

    @Test
    public void testSetEventState() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.PERMISSIVE_CLEARANCE);

        MovementEvent event = new MovementEvent();
        event.setEventState(eventState);

        assertTrue(eventState.equals(event.getEventState()));

        thrown.expect(NullPointerException.class);
        event.setEventState((MovementPhaseState)null);
    }

    @Test
    public void testSetEventStatePrimitive() {

        MovementPhase eventState = MovementPhase.PROTECTED_CLEARANCE;

        MovementEvent event = new MovementEvent();
        event.setEventState(eventState);

        assertTrue(eventState.equals(event.getEventState().getEnumeration()));

        thrown.expect(NullPointerException.class);
        event.setEventState((MovementPhase)null);
    }

    @Test
    public void testEncodeUPERMin() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.STOP_AND_REMAIN);

        MovementEvent event = new MovementEvent(eventState);

        String eventOptionals = "0000";
        String remainingBits = eventState.encodeUPER();
        assertTrue((eventOptionals + remainingBits).equals(event.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.DARK);
        TimeChangeDetails timing = new TimeChangeDetails(1);
        timing.setStartTime(10);
        timing.setMaxEndTime(5);
        timing.setConfidence(3);
        timing.setNextTime(80);
        AdvisorySpeedList speeds = new AdvisorySpeedList(1);
        speeds.getAdvisorySpeedArray()[0] = new AdvisorySpeed(new AdvisorySpeedType(Advisory.NONE));

        MovementEvent event = new MovementEvent(eventState);
        event.setTiming(timing);
        event.setSpeeds(speeds);

        String eventOptionals = "0110";
        String remainingBits = eventState.encodeUPER() + timing.encodeUPER() + speeds.encodeUPER();
        assertTrue((eventOptionals + remainingBits).equals(event.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.DARK);

        String eventOptionals = "0000";

        MovementEvent event = new MovementEvent();
        String remainingBits = event.decodeUPER(eventOptionals + eventState.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(event.getTiming());
        assertNull(event.getSpeeds());
        assertTrue(eventState.equals(event.getEventState()));
    }

    @Test
    public void testDecodeUPERMax() {

        MovementPhaseState eventState = new MovementPhaseState(MovementPhase.DARK);
        TimeChangeDetails timing = new TimeChangeDetails(10);
        timing.setStartTime(1);
        timing.setNextTime(800);
        AdvisorySpeedList speeds = new AdvisorySpeedList(1);
        speeds.getAdvisorySpeedArray()[0] = new AdvisorySpeed(new AdvisorySpeedType(Advisory.GREENWAVE));

        String eventOptionals = "0110";

        MovementEvent event = new MovementEvent();
        String remainingBits = event.decodeUPER(eventOptionals + eventState.encodeUPER() + timing.encodeUPER() + speeds.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(eventState.equals(event.getEventState()));
        assertTrue(timing.equals(event.getTiming()));
        assertTrue(speeds.equals(event.getSpeeds()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String eventOptionals = "1000";

        MovementEvent event = new MovementEvent();
        thrown.expect(IllegalArgumentException.class);
        event.decodeUPER(eventOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String eventOptionals = "0001";

        MovementEvent event = new MovementEvent();
        thrown.expect(IllegalArgumentException.class);
        event.decodeUPER(eventOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String eventOptionals = "010";

        MovementEvent event = new MovementEvent();
        thrown.expect(IllegalArgumentException.class);
        event.decodeUPER(eventOptionals);
    }

    @Test
    public void testHashCode() {

        MovementPhase eventState = event.getEventState().getEnumeration();
        TimeChangeDetails timing = event.getTiming();
        AdvisorySpeedList speeds = event.getSpeeds();

        TimeChangeDetails diffTiming = new TimeChangeDetails(timing.getMinEndTime().getValue() + 5);
        AdvisorySpeedList diffSpeeds = new AdvisorySpeedList(speeds.getAdvisorySpeedArray().length + 1);

        MovementEvent event2 = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        event2.setTiming(diffTiming);
        event2.setSpeeds(diffSpeeds);

        assertFalse(event.hashCode() == event2.hashCode());
        assertTrue(event.hashCode() == event.hashCode());
        assertTrue(event2.hashCode() == event2.hashCode());

        MovementEvent event3 = new MovementEvent(eventState);
        event3.setTiming(timing);
        event3.setSpeeds(speeds);

        assertTrue(event.hashCode() == event3.hashCode());
        assertFalse(event2.hashCode() == event3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(event.equals(event));
        assertFalse(event.equals(null));
        assertFalse(event.equals(new String()));

        MovementPhase eventState = event.getEventState().getEnumeration();
        TimeChangeDetails timing = event.getTiming();
        AdvisorySpeedList speeds = event.getSpeeds();

        TimeChangeDetails diffTiming = new TimeChangeDetails(timing.getMinEndTime().getValue() + 5);
        AdvisorySpeedList diffSpeeds = new AdvisorySpeedList(speeds.getAdvisorySpeedArray().length + 1);

        // different
        MovementEvent event2 = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        event2.setTiming(diffTiming);
        event2.setSpeeds(diffSpeeds);

        assertFalse(event.equals(event2));

        // different event state
        event2 = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        event2.setTiming(timing);
        event2.setSpeeds(speeds);

        assertFalse(event.equals(event2));

        // different timing
        event2 = new MovementEvent(eventState);
        event2.setTiming(diffTiming);
        event2.setSpeeds(speeds);

        assertFalse(event.equals(event2));

        // different speeds
        event2 = new MovementEvent(eventState);
        event2.setTiming(timing);
        event2.setSpeeds(diffSpeeds);

        assertFalse(event.equals(event2));

        // same
        event2 = new MovementEvent(eventState);
        event2.setTiming(timing);
        event2.setSpeeds(speeds);

        assertTrue(event.equals(event2));
    }

    @Test
    public void testEqualsNull() {

        MovementPhase eventState = event.getEventState().getEnumeration();
        TimeChangeDetails timing = event.getTiming();
        AdvisorySpeedList speeds = event.getSpeeds();

        MovementEvent event2 = new MovementEvent(eventState);

        assertFalse(event.equals(event2));

        event2.setTiming(timing);

        assertFalse(event.equals(event2));

        event2.setSpeeds(speeds);

        assertTrue(event.equals(event2));
    }
}
