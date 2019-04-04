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
package org.etexascode.j2735_2016.messages;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.IntersectionState;
import org.etexascode.j2735_2016.frames.IntersectionStateList;
import org.etexascode.j2735_2016.frames.MovementEvent;
import org.etexascode.j2735_2016.frames.MovementEventList;
import org.etexascode.j2735_2016.frames.MovementList;
import org.etexascode.j2735_2016.frames.MovementState;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the advisory speed frame.
 * 
 * @author ttevendale
 */
public class SPATTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    SPAT spat;

    IntersectionStateList intersections;

    @Before
    public void init() {

        intersections = new IntersectionStateList(1);

        MovementList states = new MovementList(1);

        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PERMISSIVE_CLEARANCE);

        states.getMovementArray()[0] = new MovementState(208, stateTimeSpeed);

        intersections.getIntersectionStateArray()[0] = new IntersectionState(new IntersectionReferenceID(1), 123, new IntersectionStatusObject(), states);

        spat = new SPAT(intersections);
        spat.setTimeStamp(383);
    }

    @Test
    public void testConstructor() {

        SPAT spat = new SPAT(intersections);

        assertNull(spat.getTimeStamp());
        assertTrue(intersections.equals(spat.getIntersections()));

        thrown.expect(NullPointerException.class);
        new SPAT(null);
    }

    @Test
    public void testSetTimeStampPrimitive() {

        int timeStamp = 8004;

        SPAT spat = new SPAT();
        spat.setTimeStamp(timeStamp);

        assertTrue(timeStamp == spat.getTimeStamp().getValue());

        timeStamp = 12452;

        spat.setTimeStamp(timeStamp);

        assertTrue(timeStamp == spat.getTimeStamp().getValue());
    }

    @Test
    public void testSetIntersections() {

        SPAT spat = new SPAT();
        spat.setIntersections(intersections);

        assertTrue(intersections.equals(spat.getIntersections()));

        thrown.expect(NullPointerException.class);
        spat.setIntersections(null);
    }

    @Test
    public void testEncodeUPERMin() {

        SPAT spat = new SPAT(intersections);

        String spatOptionals = "0000";
        String remainingBits = intersections.encodeUPER();
        assertTrue((spatOptionals + remainingBits).equals(spat.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(1123);

        SPAT spat = new SPAT(intersections);
        spat.setTimeStamp(timeStamp);

        String spatOptionals = "0100";
        String remainingBits = timeStamp.encodeUPER() + intersections.encodeUPER();
        assertTrue((spatOptionals + remainingBits).equals(spat.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        String spatOptionals = "0000";

        SPAT spat = new SPAT();
        String remainingBits = spat.decodeUPER(spatOptionals + intersections.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(spat.getTimeStamp());
        assertTrue(intersections.equals(spat.getIntersections()));
    }

    @Test
    public void testDecodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(321);

        String spatOptionals = "0100";

        SPAT spat = new SPAT();
        String remainingBits = spat.decodeUPER(spatOptionals + timeStamp.encodeUPER() + intersections.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(timeStamp.equals(spat.getTimeStamp()));
        assertTrue(intersections.equals(spat.getIntersections()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String spatOptionals = "1000";

        SPAT spat = new SPAT();
        thrown.expect(IllegalArgumentException.class);
        spat.decodeUPER(spatOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String spatOptionals = "0010";

        SPAT spat = new SPAT();
        thrown.expect(IllegalArgumentException.class);
        spat.decodeUPER(spatOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String spatOptionals = "0001";

        SPAT spat = new SPAT();
        thrown.expect(IllegalArgumentException.class);
        spat.decodeUPER(spatOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String spatOptionals = "010";

        SPAT spat = new SPAT();
        thrown.expect(IllegalArgumentException.class);
        spat.decodeUPER(spatOptionals);
    }

    @Test
    public void testHashCode() {

        int timeStamp = spat.getTimeStamp().getValue();
        IntersectionStateList intersections = spat.getIntersections();

        IntersectionStateList diffIntersections = new IntersectionStateList(intersections.getIntersectionStateArray().length + 1);

        SPAT spat2 = new SPAT(diffIntersections);
        spat2.setTimeStamp(timeStamp + 1);

        assertFalse(spat.hashCode() == spat2.hashCode());
        assertTrue(spat.hashCode() == spat.hashCode());
        assertTrue(spat2.hashCode() == spat2.hashCode());

        SPAT spat3 = new SPAT(intersections);
        spat3.setTimeStamp(timeStamp);

        assertTrue(spat.hashCode() == spat3.hashCode());
        assertFalse(spat2.hashCode() == spat3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(spat.equals(spat));
        assertFalse(spat.equals(null));
        assertFalse(spat.equals(new String()));

        int timeStamp = spat.getTimeStamp().getValue();
        IntersectionStateList intersections = spat.getIntersections();

        IntersectionStateList diffIntersections = new IntersectionStateList(intersections.getIntersectionStateArray().length + 1);

        // different
        SPAT spat2 = new SPAT(diffIntersections);
        spat2.setTimeStamp(timeStamp + 1);

        assertFalse(spat.equals(spat2));

        // different intersections
        spat2 = new SPAT(diffIntersections);
        spat2.setTimeStamp(timeStamp);

        assertFalse(spat.equals(spat2));

        // different time stamp
        spat2 = new SPAT(intersections);
        spat2.setTimeStamp(timeStamp + 1);

        assertFalse(spat.equals(spat2));

        // same
        spat2 = new SPAT(intersections);
        spat2.setTimeStamp(timeStamp);

        assertTrue(spat.equals(spat2));
    }

    @Test
    public void testEqualsNull() {

        int timeStamp = spat.getTimeStamp().getValue();
        IntersectionStateList intersections = spat.getIntersections();

        SPAT spat2 = new SPAT(intersections);

        assertFalse(spat.equals(spat2));

        spat2.setTimeStamp(timeStamp);

        assertTrue(spat.equals(spat2));
    }
}
