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

import org.etexascode.j2735_2016.elements.Angle;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.elements.ScaleB12;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the computed lane frame.
 * 
 * @author ttevendale
 */
public class ComputedLaneTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    ComputedLane computedLane;

    @Before
    public void init() {

        LaneID referenceLaneId = new LaneID(23);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-500));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(19019));
        Angle rotateXY = new Angle(1234);
        ScaleB12 scaleXAxis = new ScaleB12(29);
        ScaleB12 scaleYAxis = new ScaleB12(-1000);

        computedLane = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane.setRotateXY(rotateXY);
        computedLane.setScaleXAxis(scaleXAxis);
        computedLane.setScaleYAxis(scaleYAxis);
    }

    @Test
    public void testConstructor() {

        LaneID referenceLaneId = new LaneID(5);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-1000));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(5419));

        ComputedLane computedLane = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);

        assertNull(computedLane.getRotateXY());
        assertNull(computedLane.getScaleXAxis());
        assertNull(computedLane.getScaleYAxis());
        assertTrue(referenceLaneId.equals(computedLane.getReferenceLaneId()));
        assertTrue(offsetXAxis.equals(computedLane.getOffsetXAxis()));
        assertTrue(offsetYAxis.equals(computedLane.getOffsetYAxis()));
    }

    @Test
    public void testConstructorNullReferenceLaneId() {

        thrown.expect(NullPointerException.class);
        new ComputedLane(null, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-5)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(1010)));
    }

    @Test
    public void testConstructorNullOffsetXAxis() {

        thrown.expect(NullPointerException.class);
        new ComputedLane(new LaneID(38), null, new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(1010)));
    }

    @Test
    public void testConstructorNullOffsetYAxis() {

        thrown.expect(NullPointerException.class);
        new ComputedLane(new LaneID(38), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-5)), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int referenceLaneId = 123;
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(-7800));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(419));

        ComputedLane computedLane = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);

        assertNull(computedLane.getRotateXY());
        assertNull(computedLane.getScaleXAxis());
        assertNull(computedLane.getScaleYAxis());
        assertTrue(referenceLaneId == computedLane.getReferenceLaneId().getValue());
        assertTrue(offsetXAxis.equals(computedLane.getOffsetXAxis()));
        assertTrue(offsetYAxis.equals(computedLane.getOffsetYAxis()));
    }

    @Test
    public void testConstructorPrimitiveNullOffsetXAxis() {

        thrown.expect(NullPointerException.class);
        new ComputedLane(38, null, new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(1010)));
    }

    @Test
    public void testConstructorPrimitiveNullOffsetYAxis() {

        thrown.expect(NullPointerException.class);
        new ComputedLane(38, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-5)), null);
    }

    @Test
    public void testSetReferenceLaneId() {

        LaneID laneId = new LaneID(31);

        ComputedLane computedLane = new ComputedLane();
        computedLane.setReferenceLaneId(laneId);

        assertTrue(laneId.equals(computedLane.getReferenceLaneId()));

        thrown.expect(NullPointerException.class);
        computedLane.setReferenceLaneId(null);
    }

    @Test
    public void testSetReferenceLaneIdPrimitive() {

        int laneId = 39;

        ComputedLane computedLane = new ComputedLane();
        computedLane.setReferenceLaneId(laneId);

        assertTrue(laneId == computedLane.getReferenceLaneId().getValue());
    }

    @Test
    public void testSetOffsetXAxis() {

        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(388));

        ComputedLane computedLane = new ComputedLane();
        computedLane.setOffsetXAxis(offsetXAxis);

        assertTrue(offsetXAxis.equals(computedLane.getOffsetXAxis()));

        thrown.expect(NullPointerException.class);
        computedLane.setOffsetXAxis(null);
    }

    @Test
    public void testSetOffsetYAxis() {

        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(31788));

        ComputedLane computedLane = new ComputedLane();
        computedLane.setOffsetYAxis(offsetYAxis);

        assertTrue(offsetYAxis.equals(computedLane.getOffsetYAxis()));

        thrown.expect(NullPointerException.class);
        computedLane.setOffsetYAxis(null);
    }

    @Test
    public void testSetRotateXYPrimitive() {

        int rotateXY = 1245;

        ComputedLane computedLane = new ComputedLane();
        computedLane.setRotateXY(rotateXY);

        assertTrue(rotateXY == computedLane.getRotateXY().getValue());

        rotateXY = 11141;

        computedLane.setRotateXY(rotateXY);

        assertTrue(rotateXY == computedLane.getRotateXY().getValue());
    }

    @Test
    public void testSetScaleXAxisPrimitive() {

        int scaleXAxis = -1541;

        ComputedLane computedLane = new ComputedLane();
        computedLane.setScaleXAxis(scaleXAxis);

        assertTrue(scaleXAxis == computedLane.getScaleXAxis().getValue());

        scaleXAxis = 2000;

        computedLane.setScaleXAxis(scaleXAxis);

        assertTrue(scaleXAxis == computedLane.getScaleXAxis().getValue());
    }

    @Test
    public void testSetScaleYAxisPrimitive() {

        int scaleYAxis = -1541;

        ComputedLane computedLane = new ComputedLane();
        computedLane.setScaleYAxis(scaleYAxis);

        assertTrue(scaleYAxis == computedLane.getScaleYAxis().getValue());

        scaleYAxis = 2000;

        computedLane.setScaleYAxis(scaleYAxis);

        assertTrue(scaleYAxis == computedLane.getScaleYAxis().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        LaneID referenceLaneId = new LaneID(2);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-5));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(3199));

        ComputedLane computedLane = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);

        String computedLaneOptionals = "00000";
        String remainingBits = referenceLaneId.encodeUPER() + offsetXAxis.encodeUPER() + offsetYAxis.encodeUPER();
        assertTrue((computedLaneOptionals + remainingBits).equals(computedLane.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        LaneID referenceLaneId = new LaneID(3);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(5100));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(-19019));
        Angle rotateXY = new Angle(22287);
        ScaleB12 scaleXAxis = new ScaleB12(121);
        ScaleB12 scaleYAxis = new ScaleB12(-2);

        ComputedLane computedLane = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane.setRotateXY(rotateXY);
        computedLane.setScaleXAxis(scaleXAxis);
        computedLane.setScaleYAxis(scaleYAxis);

        String computedLaneOptionals = "01110";
        String remainingBits = referenceLaneId.encodeUPER() + offsetXAxis.encodeUPER() + offsetYAxis.encodeUPER() + rotateXY.encodeUPER() + scaleXAxis.encodeUPER() + scaleYAxis.encodeUPER();
        assertTrue((computedLaneOptionals + remainingBits).equals(computedLane.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        LaneID referenceLaneId = new LaneID(112);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(554));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(3199));

        String computedLaneOptionals = "00000";

        ComputedLane computedLane = new ComputedLane();
        String remainingBits = computedLane.decodeUPER(computedLaneOptionals + referenceLaneId.encodeUPER() + offsetXAxis.encodeUPER() + offsetYAxis.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(computedLane.getRotateXY());
        assertNull(computedLane.getScaleXAxis());
        assertNull(computedLane.getScaleYAxis());
        assertTrue(referenceLaneId.equals(computedLane.getReferenceLaneId()));
        assertTrue(offsetXAxis.equals(computedLane.getOffsetXAxis()));
        assertTrue(offsetYAxis.equals(computedLane.getOffsetYAxis()));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneID referenceLaneId = new LaneID(44);
        ComputedLaneOffsetXAxis offsetXAxis = new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(-50));
        ComputedLaneOffsetYAxis offsetYAxis = new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(222));
        Angle rotateXY = new Angle(987);
        ScaleB12 scaleXAxis = new ScaleB12(-1);
        ScaleB12 scaleYAxis = new ScaleB12(-11);

        String computedLaneOptionals = "01110";

        ComputedLane computedLane = new ComputedLane();
        String remainingBits = computedLane.decodeUPER(
                computedLaneOptionals + referenceLaneId.encodeUPER() + offsetXAxis.encodeUPER() + offsetYAxis.encodeUPER() + rotateXY.encodeUPER() + scaleXAxis.encodeUPER() + scaleYAxis.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(referenceLaneId.equals(computedLane.getReferenceLaneId()));
        assertTrue(offsetXAxis.equals(computedLane.getOffsetXAxis()));
        assertTrue(offsetYAxis.equals(computedLane.getOffsetYAxis()));
        assertTrue(rotateXY.equals(computedLane.getRotateXY()));
        assertTrue(scaleXAxis.equals(computedLane.getScaleXAxis()));
        assertTrue(scaleYAxis.equals(computedLane.getScaleYAxis()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String computedLaneOptionals = "10000";

        ComputedLane computedLane = new ComputedLane();
        thrown.expect(IllegalArgumentException.class);
        computedLane.decodeUPER(computedLaneOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String computedLaneOptionals = "00111";

        ComputedLane computedLane = new ComputedLane();
        thrown.expect(IllegalArgumentException.class);
        computedLane.decodeUPER(computedLaneOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String computedLaneOptionals = "0110";

        ComputedLane computedLane = new ComputedLane();
        thrown.expect(IllegalArgumentException.class);
        computedLane.decodeUPER(computedLaneOptionals);
    }

    @Test
    public void testHashCode() {

        int referenceLaneId = computedLane.getReferenceLaneId().getValue();
        ComputedLaneOffsetXAxis offsetXAxis = computedLane.getOffsetXAxis();
        ComputedLaneOffsetYAxis offsetYAxis = computedLane.getOffsetYAxis();
        int rotateXY = computedLane.getRotateXY().getValue();
        int scaleXAxis = computedLane.getScaleXAxis().getValue();
        int scaleYAxis = computedLane.getScaleYAxis().getValue();

        ComputedLane computedLane2 = new ComputedLane(referenceLaneId + 1, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3838)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50)));
        computedLane2.setRotateXY(rotateXY + 1);
        computedLane2.setScaleXAxis(scaleXAxis + 1);
        computedLane2.setScaleYAxis(scaleYAxis + 1);

        assertFalse(computedLane.hashCode() == computedLane2.hashCode());
        assertTrue(computedLane.hashCode() == computedLane.hashCode());
        assertTrue(computedLane2.hashCode() == computedLane2.hashCode());

        ComputedLane computedLane3 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane3.setRotateXY(rotateXY);
        computedLane3.setScaleXAxis(scaleXAxis);
        computedLane3.setScaleYAxis(scaleYAxis);

        assertTrue(computedLane.hashCode() == computedLane3.hashCode());
        assertFalse(computedLane2.hashCode() == computedLane3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(computedLane.equals(computedLane));
        assertFalse(computedLane.equals(null));
        assertFalse(computedLane.equals(new String()));

        int referenceLaneId = computedLane.getReferenceLaneId().getValue();
        ComputedLaneOffsetXAxis offsetXAxis = computedLane.getOffsetXAxis();
        ComputedLaneOffsetYAxis offsetYAxis = computedLane.getOffsetYAxis();
        int rotateXY = computedLane.getRotateXY().getValue();
        int scaleXAxis = computedLane.getScaleXAxis().getValue();
        int scaleYAxis = computedLane.getScaleYAxis().getValue();

        // different
        ComputedLane computedLane2 = new ComputedLane(referenceLaneId + 1, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3838)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50)));
        computedLane2.setRotateXY(rotateXY + 1);
        computedLane2.setScaleXAxis(scaleXAxis + 1);
        computedLane2.setScaleYAxis(scaleYAxis + 1);

        assertFalse(computedLane.equals(computedLane2));

        // different reference lane ID
        computedLane2 = new ComputedLane(referenceLaneId + 1, offsetXAxis, offsetYAxis);
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertFalse(computedLane.equals(computedLane2));

        // different offset x axis
        computedLane2 = new ComputedLane(referenceLaneId, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(3838)), offsetYAxis);
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertFalse(computedLane.equals(computedLane2));

        // different offset y axis
        computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50)));
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertFalse(computedLane.equals(computedLane2));

        // different rotate xy
        computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane2.setRotateXY(rotateXY + 1);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertFalse(computedLane.equals(computedLane2));

        // different scale x axis
        computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis + 1);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertFalse(computedLane.equals(computedLane2));

        // different scale y axis
        computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis + 1);

        assertFalse(computedLane.equals(computedLane2));

        // same
        computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        computedLane2.setRotateXY(rotateXY);
        computedLane2.setScaleXAxis(scaleXAxis);
        computedLane2.setScaleYAxis(scaleYAxis);

        assertTrue(computedLane.equals(computedLane2));
    }

    @Test
    public void testEqualsNull() {

        int referenceLaneId = computedLane.getReferenceLaneId().getValue();
        ComputedLaneOffsetXAxis offsetXAxis = computedLane.getOffsetXAxis();
        ComputedLaneOffsetYAxis offsetYAxis = computedLane.getOffsetYAxis();
        int rotateXY = computedLane.getRotateXY().getValue();
        int scaleXAxis = computedLane.getScaleXAxis().getValue();
        int scaleYAxis = computedLane.getScaleYAxis().getValue();

        ComputedLane computedLane2 = new ComputedLane(referenceLaneId, offsetXAxis, offsetYAxis);
        assertFalse(computedLane.equals(computedLane2));

        computedLane2.setRotateXY(rotateXY);
        assertFalse(computedLane.equals(computedLane2));

        computedLane2.setScaleXAxis(scaleXAxis);
        assertFalse(computedLane.equals(computedLane2));

        computedLane2.setScaleYAxis(scaleYAxis);
        assertTrue(computedLane.equals(computedLane2));
    }
}
