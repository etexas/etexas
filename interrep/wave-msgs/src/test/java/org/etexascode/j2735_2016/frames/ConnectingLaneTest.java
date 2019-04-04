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

import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.LaneID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the connecting lane frame.
 * 
 * @author ttevendale
 */
public class ConnectingLaneTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    ConnectingLane connectingLane;

    @Before
    public void init() {

        LaneID lane = new LaneID(10);
        AllowedManeuvers maneuver = new AllowedManeuvers();
        maneuver.setManeuverLeftTurnOnRedAllowed(true);

        connectingLane = new ConnectingLane(lane);
        connectingLane.setManeuver(maneuver);
    }

    @Test
    public void testConstructor() {

        LaneID lane = new LaneID(5);
        ConnectingLane connectingLane = new ConnectingLane(lane);

        assertNull(connectingLane.getManeuver());
        assertTrue(lane.equals(connectingLane.getLane()));

        thrown.expect(NullPointerException.class);
        new ConnectingLane(null);
    }

    @Test
    public void testConstructorPrimitive() {

        int lane = 32;
        ConnectingLane connectingLane = new ConnectingLane(lane);

        assertNull(connectingLane.getManeuver());
        assertTrue(lane == connectingLane.getLane().getValue());
    }

    @Test
    public void testSetLane() {

        LaneID lane = new LaneID(44);

        ConnectingLane connectingLane = new ConnectingLane();
        connectingLane.setLane(lane);

        assertTrue(lane.equals(connectingLane.getLane()));

        thrown.expect(NullPointerException.class);
        connectingLane.setLane(null);
    }

    @Test
    public void testSetLanePrimitive() {

        int lane = 99;

        ConnectingLane connectingLane = new ConnectingLane();
        connectingLane.setLane(lane);

        assertTrue(lane == connectingLane.getLane().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        LaneID lane = new LaneID(78);

        ConnectingLane connectingLane = new ConnectingLane(lane);

        String connectingLaneOptionals = "0";
        String remainingBits = lane.encodeUPER();
        assertTrue((connectingLaneOptionals + remainingBits).equals(connectingLane.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        LaneID lane = new LaneID(1);
        AllowedManeuvers maneuver = new AllowedManeuvers();

        ConnectingLane connectingLane = new ConnectingLane(lane);
        connectingLane.setManeuver(maneuver);

        String connectingLaneOptionals = "1";
        String remainingBits = lane.encodeUPER() + maneuver.encodeUPER();
        assertTrue((connectingLaneOptionals + remainingBits).equals(connectingLane.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        LaneID lane = new LaneID(2);

        String connectingLaneOptionals = "0";

        ConnectingLane connectingLane = new ConnectingLane();
        String remainingBits = connectingLane.decodeUPER(connectingLaneOptionals + lane.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(connectingLane.getManeuver());
        assertTrue(lane.equals(connectingLane.getLane()));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneID lane = new LaneID(3);
        AllowedManeuvers maneuver = new AllowedManeuvers();

        String connectingLaneOptionals = "1";

        ConnectingLane connectingLane = new ConnectingLane();
        String remainingBits = connectingLane.decodeUPER(connectingLaneOptionals + lane.encodeUPER() + maneuver.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(lane.equals(connectingLane.getLane()));
        assertTrue(maneuver.equals(connectingLane.getManeuver()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        String connectingLaneOptionals = "";

        ConnectingLane connectingLane = new ConnectingLane();
        thrown.expect(IllegalArgumentException.class);
        connectingLane.decodeUPER(connectingLaneOptionals);
    }

    @Test
    public void testHashCode() {

        int lane = connectingLane.getLane().getValue();
        AllowedManeuvers maneuver = connectingLane.getManeuver();

        AllowedManeuvers diffManeuver = new AllowedManeuvers();
        diffManeuver.setManeuverRightTurnOnRedAllowed(!maneuver.isManeuverRightTurnOnRedAllowed());

        ConnectingLane connectingLane2 = new ConnectingLane(lane + 1);
        connectingLane2.setManeuver(diffManeuver);

        assertFalse(connectingLane.hashCode() == connectingLane2.hashCode());
        assertTrue(connectingLane.hashCode() == connectingLane.hashCode());
        assertTrue(connectingLane2.hashCode() == connectingLane2.hashCode());

        ConnectingLane connectingLane3 = new ConnectingLane(lane);
        connectingLane3.setManeuver(maneuver);

        assertTrue(connectingLane.hashCode() == connectingLane3.hashCode());
        assertFalse(connectingLane2.hashCode() == connectingLane3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(connectingLane.equals(connectingLane));
        assertFalse(connectingLane.equals(null));
        assertFalse(connectingLane.equals(new String()));

        int lane = connectingLane.getLane().getValue();
        AllowedManeuvers maneuver = connectingLane.getManeuver();

        AllowedManeuvers diffManeuver = new AllowedManeuvers();
        diffManeuver.setManeuverRightTurnOnRedAllowed(!maneuver.isManeuverRightTurnOnRedAllowed());

        // different
        ConnectingLane connectingLane2 = new ConnectingLane(lane + 1);
        connectingLane2.setManeuver(diffManeuver);

        assertFalse(connectingLane.equals(connectingLane2));

        // different lane
        connectingLane2 = new ConnectingLane(lane + 1);
        connectingLane2.setManeuver(maneuver);

        assertFalse(connectingLane.equals(connectingLane2));

        // different maneuver
        connectingLane2 = new ConnectingLane(lane);
        connectingLane2.setManeuver(diffManeuver);

        assertFalse(connectingLane.equals(connectingLane2));

        // same
        connectingLane2 = new ConnectingLane(lane);
        connectingLane2.setManeuver(maneuver);

        assertTrue(connectingLane.equals(connectingLane2));
    }
}
