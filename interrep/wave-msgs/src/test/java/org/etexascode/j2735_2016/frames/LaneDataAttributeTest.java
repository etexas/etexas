/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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

import org.etexascode.j2735_2016.elements.DeltaAngle;
import org.etexascode.j2735_2016.elements.MergeDivergeNodeAngle;
import org.etexascode.j2735_2016.elements.RoadwayCrownAngle;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane data attribute frame (Choice).
 * 
 * @author ttevendale
 */
public class LaneDataAttributeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorDeltaAngle() {

        DeltaAngle angle = new DeltaAngle(11);

        LaneDataAttribute attribute = new LaneDataAttribute(angle);

        assertTrue(angle.equals(attribute.getPathEndPointAngle()));
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((DeltaAngle)null);
    }

    @Test
    public void testConstructorRoadwayCrownAngleCenter() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(121);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 0);

        assertTrue(angle.equals(attribute.getLaneCrownPointCenter()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((RoadwayCrownAngle)null, 0);
    }

    @Test
    public void testConstructorRoadwayCrownAngleLeft() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(22);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 1);

        assertTrue(angle.equals(attribute.getLaneCrownPointLeft()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((RoadwayCrownAngle)null, 1);
    }

    @Test
    public void testConstructorRoadwayCrownAngleRight() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(-15);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 2);

        assertTrue(angle.equals(attribute.getLaneCrownPointRight()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((RoadwayCrownAngle)null, 2);
    }

    @Test
    public void testConstructorRoadwayCrownAngleInvalidIndex() {

        thrown.expect(IllegalArgumentException.class);
        new LaneDataAttribute(new RoadwayCrownAngle(), 3);
    }

    @Test
    public void testConstructorMergeDivergeNodeAngle() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(111);

        LaneDataAttribute attribute = new LaneDataAttribute(angle);

        assertTrue(angle.equals(attribute.getLaneAngle()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getSpeedLimits());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((MergeDivergeNodeAngle)null);
    }

    @Test
    public void testConstructorSpeedLimitList() {

        SpeedLimitList speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 3883);

        LaneDataAttribute attribute = new LaneDataAttribute(speedLimits);

        assertTrue(speedLimits.equals(attribute.getSpeedLimits()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());

        thrown.expect(NullPointerException.class);
        new LaneDataAttribute((SpeedLimitList)null);
    }

    @Test
    public void testEncodeUPERDeltaAngle() {

        DeltaAngle angle = new DeltaAngle(101);

        LaneDataAttribute attribute = new LaneDataAttribute(angle);

        String laneDataAttributeChoice = "0000";
        String remainingBits = angle.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERRoadwayCrownCenter() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(-88);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 0);

        String laneDataAttributeChoice = "0001";
        String remainingBits = angle.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERRoadwayCrownLeft() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(-74);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 1);

        String laneDataAttributeChoice = "0010";
        String remainingBits = angle.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERRoadwayCrownRight() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(111);

        LaneDataAttribute attribute = new LaneDataAttribute(angle, 2);

        String laneDataAttributeChoice = "0011";
        String remainingBits = angle.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMergeDivergeNodeAngle() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(0);

        LaneDataAttribute attribute = new LaneDataAttribute(angle);

        String laneDataAttributeChoice = "0100";
        String remainingBits = angle.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERSpeedLimitList() {

        SpeedLimitList speedLimits = new SpeedLimitList(2);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 111);
        speedLimits.getSpeedLimitArray()[1] = new RegulatorySpeedLimit(SpeedLimit.TRUCK_MIN_SPEED, 8191);

        LaneDataAttribute attribute = new LaneDataAttribute(speedLimits);

        String laneDataAttributeChoice = "0101";
        String remainingBits = speedLimits.encodeUPER();

        assertTrue((laneDataAttributeChoice + remainingBits).equals(attribute.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        LaneDataAttribute attribute = new LaneDataAttribute();
        thrown.expect(IllegalStateException.class);
        attribute.encodeUPER();
    }

    @Test
    public void testDecodeUPERDeltaAngle() {

        DeltaAngle angle = new DeltaAngle(101);

        String laneDataAttributeChoice = "0000";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + angle.encodeUPER());

        assertTrue(angle.equals(attribute.getPathEndPointAngle()));
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());
    }

    @Test
    public void testDecodeUPERRoadwayCrownAngleCenter() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(-78);

        String laneDataAttributeChoice = "0001";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + angle.encodeUPER());

        assertTrue(angle.equals(attribute.getLaneCrownPointCenter()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());
    }

    @Test
    public void testDecodeUPERRoadwayCrownAngleLeft() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(-1);

        String laneDataAttributeChoice = "0010";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + angle.encodeUPER());

        assertTrue(angle.equals(attribute.getLaneCrownPointLeft()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());
    }

    @Test
    public void testDecodeUPERRoadwayCrownAngleRight() {

        RoadwayCrownAngle angle = new RoadwayCrownAngle(1);

        String laneDataAttributeChoice = "0011";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + angle.encodeUPER());

        assertTrue(angle.equals(attribute.getLaneCrownPointRight()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneAngle());
        assertNull(attribute.getSpeedLimits());
    }

    @Test
    public void testDecodeUPERMergeDivergeNodeAngle() {

        MergeDivergeNodeAngle angle = new MergeDivergeNodeAngle(1);

        String laneDataAttributeChoice = "0100";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + angle.encodeUPER());

        assertTrue(angle.equals(attribute.getLaneAngle()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getSpeedLimits());
    }

    @Test
    public void testDecodeUPERSpeedLimitList() {

        SpeedLimitList speedLimits = new SpeedLimitList(2);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 111);
        speedLimits.getSpeedLimitArray()[1] = new RegulatorySpeedLimit(SpeedLimit.TRUCK_MIN_SPEED, 8191);

        String laneDataAttributeChoice = "0101";

        LaneDataAttribute attribute = new LaneDataAttribute();
        attribute.decodeUPER(laneDataAttributeChoice + speedLimits.encodeUPER());

        assertTrue(speedLimits.equals(attribute.getSpeedLimits()));
        assertNull(attribute.getPathEndPointAngle());
        assertNull(attribute.getLaneCrownPointCenter());
        assertNull(attribute.getLaneCrownPointLeft());
        assertNull(attribute.getLaneCrownPointRight());
        assertNull(attribute.getLaneAngle());
    }

    @Test
    public void testDecodeUPERExtension() {

        String choice = "1000";

        LaneDataAttribute attribute = new LaneDataAttribute();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String choice = "0110";

        LaneDataAttribute attribute = new LaneDataAttribute();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String choice = "";

        LaneDataAttribute attribute = new LaneDataAttribute();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERAboveKnown() {

        String choice = "0111";

        LaneDataAttribute attribute = new LaneDataAttribute();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER(choice);
    }

    @Test
    public void testHashCode() {

        DeltaAngle angle = new DeltaAngle(101);
        LaneDataAttribute attribute = new LaneDataAttribute(angle);

        LaneDataAttribute attribute2 = new LaneDataAttribute(new DeltaAngle(angle.getValue() + 1));

        assertFalse(attribute.hashCode() == attribute2.hashCode());
        assertTrue(attribute.hashCode() == attribute.hashCode());
        assertTrue(attribute2.hashCode() == attribute2.hashCode());

        LaneDataAttribute attribute3 = new LaneDataAttribute(angle);

        assertTrue(attribute.hashCode() == attribute3.hashCode());
        assertFalse(attribute2.hashCode() == attribute3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneDataAttribute attribute = new LaneDataAttribute(new DeltaAngle(27));

        assertTrue(attribute.equals(attribute));
        assertFalse(attribute.equals(null));
        assertFalse(attribute.equals(new String()));

        // different delta angle
        LaneDataAttribute attribute2 = new LaneDataAttribute(new DeltaAngle(attribute.getPathEndPointAngle().getValue() + 1));
        assertFalse(attribute.equals(attribute2));

        // same delta angle
        attribute2 = new LaneDataAttribute(new DeltaAngle(attribute.getPathEndPointAngle().getValue()));
        assertTrue(attribute.equals(attribute2));

        // different roadway crown angle (center)
        attribute = new LaneDataAttribute(new RoadwayCrownAngle(38), 0);
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointCenter().getValue() + 1), 0);
        assertFalse(attribute.equals(attribute2));

        // same roadway crown angle (center)
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointCenter().getValue()), 0);
        assertTrue(attribute.equals(attribute2));

        // different roadway crown angle (left)
        attribute = new LaneDataAttribute(new RoadwayCrownAngle(-77), 1);
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointLeft().getValue() + 1), 1);
        assertFalse(attribute.equals(attribute2));

        // same roadway crown angle (left)
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointLeft().getValue()), 1);
        assertTrue(attribute.equals(attribute2));

        // different roadway crown angle (right)
        attribute = new LaneDataAttribute(new RoadwayCrownAngle(111), 2);
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointRight().getValue() + 1), 2);
        assertFalse(attribute.equals(attribute2));

        // same roadway crown angle (right)
        attribute2 = new LaneDataAttribute(new RoadwayCrownAngle(attribute.getLaneCrownPointRight().getValue()), 2);
        assertTrue(attribute.equals(attribute2));

        // different merge diverge node angle
        attribute = new LaneDataAttribute(new MergeDivergeNodeAngle(-5));
        attribute2 = new LaneDataAttribute(new MergeDivergeNodeAngle(attribute.getLaneAngle().getValue() + 1));
        assertFalse(attribute.equals(attribute2));

        // same merge diverge node angle
        attribute2 = new LaneDataAttribute(new MergeDivergeNodeAngle(attribute.getLaneAngle().getValue()));
        assertTrue(attribute.equals(attribute2));

        // different speed limit list
        attribute = new LaneDataAttribute(new SpeedLimitList(1));
        attribute2 = new LaneDataAttribute(new SpeedLimitList(attribute.getSpeedLimits().getSpeedLimitArray().length + 1));
        assertFalse(attribute.equals(attribute2));

        // same speed limit list
        attribute2 = new LaneDataAttribute(new SpeedLimitList(attribute.getSpeedLimits().getSpeedLimitArray().length));
        assertTrue(attribute.equals(attribute2));
    }
}
