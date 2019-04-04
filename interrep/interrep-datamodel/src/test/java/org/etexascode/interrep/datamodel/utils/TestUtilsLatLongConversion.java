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

package org.etexascode.interrep.datamodel.utils;

import junit.framework.TestCase;

import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * Testing for UtilsLatLongConversion Note: ablatt - Here are the 8 possible cases 4 cases (+ 4
 * special cases) Normal cases: 1) upper right quadrant 2) lower right quadrant 3) upper left
 * quadrant 4) lower left quadrant Rules for dealing with normal cases: 1) Theta between -180 and 0
 * indicates that the x offset should be negative 2) Theta between 0 and 180 indicates that the x
 * offset should be positive 3) Theta between -90 and 90 indicates that the y offset should be
 * positive 4) Theta not between -90 and 90 indicates that the y offset should be negative Special
 * Cases: 1) Theta is 0 indicates that the hypotenuse is the y offset while the x offset is 0 2)
 * Theta is 180 or -180 indicates that the negative hypotenuse is the y offset while the x offset is
 * 0 3) Theta is 90 indicates that the hypotenuse is the x offset while the y offset is 0 4) Theta
 * is -90 indicates that the negative hypotenuse is the x offset while the y offset is 0 Note:
 * ablatt - will conduct 2 different tests for accuracy 1) Ensure that x and y have the appropriate
 * sign (positive or negative) 2) Ensure that the ratio of x to y matches the ratio the expected
 * ratio given the lat, longs For special cases, absolute values can be measured
 * 
 * @author ablatt
 */
public class TestUtilsLatLongConversion extends TestCase {

    final double acceptableRatioDrift = 0.02; // must be positive

    final double acceptableOffsetDrift = 0.005; // must be positive

    public TestUtilsLatLongConversion(String name) {
        super(name);
    }

    /**
     * Tests moving from a 2 (lat, long) pairs to an (x, y) offset from the first (lat, long) pair
     */
    public void testLatLongsToMeters() {
        // upper right quadrant
        double[] offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(
                0.0, 0.0, 0.0005, 0.0005, 1); // +x, +y and 1:1

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        double ratio = offsets[0] / offsets[1];
        double ratioTestDouble = 1 - ratio;
        boolean ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 1",
                ratioTest);

        // test 2
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.00025, 0.0005, 1); // +x, +y and 1:2

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        ratio = offsets[0] / offsets[1];
        ratioTestDouble = 2 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 2",
                ratioTest);

        // test 3
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.0005, 0.00025, 1); // +x, +y and 2:1

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        ratio = offsets[0] / offsets[1];
        ratioTestDouble = 0.5 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 0.5",
                ratioTest);

        // lower right quadrant
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.0005, 0.0005, 1); // +x, -y and 1:1

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = offsets[0] / Math.abs(offsets[1]);
        ratioTestDouble = 1 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 1",
                ratioTest);

        // test 2
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.00025, 0.0005, 1); // +x, -y and 1:2

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = offsets[0] / Math.abs(offsets[1]);
        ratioTestDouble = 2 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 2",
                ratioTest);

        // test 3
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.0005, 0.00025, 1); // +x, -y and 1:2

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = offsets[0] / Math.abs(offsets[1]);
        ratioTestDouble = 0.5 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 0.5",
                ratioTest);

        // upper left quadrant
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.0005, -0.0005, 1); // -x, +y and 1:1

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        ratio = Math.abs(offsets[0]) / offsets[1];
        ratioTestDouble = 1 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 1",
                ratioTest);

        // test 2
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.00025, -0.0005, 1); // -x, +y and 1:2

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        ratio = Math.abs(offsets[0]) / offsets[1];
        ratioTestDouble = 2 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 2",
                ratioTest);

        // test 3
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.0005, -0.00025, 1); // -x, +y and 2:1

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        ratio = Math.abs(offsets[0]) / offsets[1];
        ratioTestDouble = 0.5 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 0.5",
                ratioTest);

        // lower left quadrant
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.0005, -0.0005, 1); // -x, -y and 1:1

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = Math.abs(offsets[0]) / Math.abs(offsets[1]);
        ratioTestDouble = 1 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 1",
                ratioTest);

        // test 2
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.00025, -0.0005, 1); // -x, -y and 1:2

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = Math.abs(offsets[0]) / Math.abs(offsets[1]);
        ratioTestDouble = 2 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 2",
                ratioTest);

        // test 3
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.0005, -0.00025, 1); // -x, -y and 2:1

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        ratio = Math.abs(offsets[0]) / Math.abs(offsets[1]);
        ratioTestDouble = 0.5 - ratio;
        ratioTest = ratioTestDouble <= acceptableRatioDrift
                && ratioTestDouble >= -acceptableRatioDrift;

        assertTrue("Test that the ratio of " + ratio + " is approximately 0.5",
                ratioTest);

        // distance is purely north (special case 1)
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0.0005, 0, 1); // x=0, +y

        boolean xCloseToZero = offsets[0] <= acceptableRatioDrift
                && offsets[0] >= -acceptableRatioDrift;
        assertTrue("Test x offset " + offsets[0] + " is close to 0",
                xCloseToZero);
        assertTrue("Test y offset " + offsets[1] + " is positive",
                offsets[1] > 0.0);

        // distance is purely south (special case 2)
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                -0.0005, 0, 1); // x=0, -y

        xCloseToZero = offsets[0] <= acceptableRatioDrift
                && offsets[0] >= -acceptableRatioDrift;
        assertTrue("Test x offset " + offsets[0] + " is close to 0",
                xCloseToZero);
        assertTrue("Test y offset " + offsets[1] + " is negative",
                offsets[1] < 0.0);

        // distance is purely east (special case 3)
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0, 0.0005, 1); // +x, y=0

        assertTrue("Test x offset " + offsets[0] + " is positive",
                offsets[0] > 0.0);
        boolean yCloseToZero = offsets[1] <= acceptableRatioDrift
                && offsets[1] >= -acceptableRatioDrift;
        assertTrue("Test y offset " + offsets[1] + " is close to 0",
                yCloseToZero);

        // distance is purely west (special case 4)
        offsets = UtilsLatLongConversion.convertLatLongToMeterOffset(0.0, 0.0,
                0, -0.0005, 1); // -x, y=0

        assertTrue("Test x offset " + offsets[0] + " is negative",
                offsets[0] < 0.0);
        yCloseToZero = offsets[1] <= acceptableRatioDrift
                && offsets[1] >= -acceptableRatioDrift;
        assertTrue("Test y offset " + offsets[1] + " is close to 0",
                yCloseToZero);
    }

    /**
     * Tests a bi-directional swap. First, convert from meter (x, y) offsets to a new (lat, long)
     * pair. Second, take this new (lat, long) pair and convert it back to a meter (x, y) offset
     * using the original center. If all goes well, we should get back the original meter (x, y)
     * offsets. Note: ablatt - using only meters for 2 reasons. 1) The feet and centimeter
     * conversion methods actually use the meter conversion methods and convert the units (at the
     * appropriate time) Hence testing the feet or centimeter methods really only tests the meter
     * methods combined with methods which can be visually confirmed (unit conversion methods) 2)
     * The geodetic calculator only uses meters when performing calculations
     */
    public void testMeterToLatLongsToMeter() {
        /*
         * Test the 4 main cases
         */
        // test 1 - upper right corner
        double[] intermediate = UtilsLatLongConversion
                .convertMeterOffsetToLatLong(1200.0, 1000.0, 45.0, 45.0, 1);
        double[] ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0,
                45.0, intermediate[0], intermediate[1], 1);

        boolean xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        boolean yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        // test 2 - upper left quadrant
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, -1000.0, 45.0, 45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, 45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - (-1000.0) <= acceptableOffsetDrift
                && ans[1] - (-1000.0) >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of -1000.0", yAcceptable);

        // test 3 - lower left quadrant
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                -1200.0, -1000.0, 10.0, 4.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(10.0, 4.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - (-1200.0) <= acceptableOffsetDrift
                && ans[0] - (-1200.0) >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - (-1000.0) <= acceptableOffsetDrift
                && ans[1] - (-1000.0) >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of -1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of -1000.0", yAcceptable);

        // test 4 - lower right quadrant
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                -1200.0, 1000.0, 45.0, 45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, 45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - (-1200.0) <= acceptableOffsetDrift
                && ans[0] - (-1200.0) >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of -1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        /*
         * Test moving the center point to the 4 quadrants of the globe
         */
        // test 5 - below the equator - on the right
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                100.0, 10.0, -45.0, 45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(-45.0, 45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 100.0 <= acceptableOffsetDrift
                && ans[0] - 100.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 10.0 <= acceptableOffsetDrift
                && ans[1] - 10.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 100.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 10.0", yAcceptable);

        // test 6 - below the equator - on the left
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 1000.0, -45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(-45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        // test 7 - above the equator - on the right
        // See the 4 main tests

        // test 8 - above the equator - on the left
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 1000.0, 45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        /*
         * Test the 4 special cases
         */
        // test 9 - moving straight north
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(0.0,
                1000.0, 45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 0.0 <= acceptableOffsetDrift
                && ans[0] - 0.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 0.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        // test 10 - moving straight south
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(0.0,
                -1000.0, 45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 0.0 <= acceptableOffsetDrift
                && ans[0] - 0.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - (-1000.0) <= acceptableOffsetDrift
                && ans[1] - (-1000.0) >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 0.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of -1000.0", yAcceptable);

        // test 11 - moving straight east
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 0.0, 45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 0.0 <= acceptableOffsetDrift
                && ans[1] - 0.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 0.0", yAcceptable);

        // test 12 - moving straight west
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                -1200.0, 0.0, 45.0, -45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(45.0, -45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - (-1200.0) <= acceptableOffsetDrift
                && ans[0] - (-1200.0) >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 0.0 <= acceptableOffsetDrift
                && ans[1] - 0.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of -1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 0.0", yAcceptable);

        /*
         * Additional special cases based on how the actual calculator works Note: the north pole -
         * Unless we're planning on building an intersection in the middle of the ocean (at the
         * north pole) or on a solid sheet of ice (at the south pole), this case will not happen in
         * nature. (The real special case here is a user being a jerk) This case failing is hardly a
         * serious problem, though we should prevent these or similar values if this case fails
         * Testing +80 and -80 latitude to ensure that our current limits are sound
         */
        // test 13 - +80 latitude
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 1000.0, 80.0, 45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(80.0, 45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        // test 14 - -80 latitude
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 1000.0, -80.0, 45.0, 1);
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(-80.0, 45.0,
                intermediate[0], intermediate[1], 1);

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);

        // test 15 - test at the north pole
        // TODO: ablatt - figure out why the south pole works but not the north
        // pole
        // intermediate =
        // UtilsLatLongConversion.convertMeterOffsetToLatLong(1200.0, 1000.0,
        // 90.0, 45.0, 1); // the north pole
        // ans = UtilsLatLongConversion.convertLatLongToMeterOffset(90.0, 45.0,
        // intermediate[0], intermediate[1], 1);
        //
        // xAcceptable = ((ans[0] - 1200.0) <= acceptableOffsetDrift) &&
        // ((ans[0] - 1200.0) >= (-acceptableOffsetDrift));
        // yAcceptable = ((ans[1] - 1000.0) <= acceptableOffsetDrift) &&
        // ((ans[1] - 1000.0) >= (-acceptableOffsetDrift));
        //
        // assertTrue("Test if " + ans[0] +
        // " is close to the expected value of 1200.0", xAcceptable);
        // assertTrue("Test if " + ans[1] +
        // " is close to the expected value of 1000.0", yAcceptable);

        // test 16 - test at the south pole
        intermediate = UtilsLatLongConversion.convertMeterOffsetToLatLong(
                1200.0, 1000.0, -90.0, 45.0, 1); // the south pole
        ans = UtilsLatLongConversion.convertLatLongToMeterOffset(-90.0, 45.0,
                intermediate[0], intermediate[1], 1);

        /*
         * try { PrintWriter pw = new PrintWriter("/home/tony/mvn_details");
         * pw.println("intermedite = (" + intermediate[0] + ", " + intermediate[1] + ")");
         * pw.println("ans = (" + ans[0] + ", " + ans[1] + ")"); pw.close(); } catch(Exception e) {}
         */

        xAcceptable = ans[0] - 1200.0 <= acceptableOffsetDrift
                && ans[0] - 1200.0 >= -acceptableOffsetDrift;
        yAcceptable = ans[1] - 1000.0 <= acceptableOffsetDrift
                && ans[1] - 1000.0 >= -acceptableOffsetDrift;

        assertTrue("Test if " + ans[0]
                + " is close to the expected value of 1200.0", xAcceptable);
        assertTrue("Test if " + ans[1]
                + " is close to the expected value of 1000.0", yAcceptable);
    }

    public void testConvertLatLongToFeetOffset() {
        double lat1 = 2;
        double long1 = 3;
        double lat2 = 4;
        double long2 = 5;
        int type = UtilsLatLongConversion.GEODETIC3D;

        double[] ret = UtilsLatLongConversion.convertLatLongToMeterOffset(lat1, long1, lat2, long2, type);
        ret[0] = UtilsUnitConversion.convertMetersToFeet(ret[0]);
        ret[1] = UtilsUnitConversion.convertMetersToFeet(ret[1]);

        double[] res = UtilsLatLongConversion.convertLatLongToFeetOffset(lat1, long1, lat2, long2, type);

        assertEquals(ret[0], res[0]);
        assertEquals(ret[1], res[1]);
    }

    public void testConvertLatLongToCentimeterOffset() {
        double lat1 = 2;
        double long1 = 3;
        double lat2 = 4;
        double long2 = 5;
        int type = UtilsLatLongConversion.SPHERICAL;

        double[] ret = UtilsLatLongConversion.convertLatLongToMeterOffset(lat1, long1, lat2, long2, type);
        ret[0] = UtilsUnitConversion.convertMetersToCentimeters(ret[0]);
        ret[1] = UtilsUnitConversion.convertMetersToCentimeters(ret[1]);

        double[] res = UtilsLatLongConversion.convertLatLongToCentimeterOffset(lat1, long1, lat2, long2, type);

        assertEquals(ret[0], res[0]);
        assertEquals(ret[1], res[1]);
    }

    public void testConvertfeettOffsetToLatLong() {
        double x = 2;
        double y = 3;
        double startLat = 4;
        double startLong = 5;
        int type = UtilsLatLongConversion.CARTESIAN;

        double[] ret = UtilsLatLongConversion.convertMeterOffsetToLatLong(UtilsUnitConversion.convertFeetToMeters(x),
                UtilsUnitConversion.convertFeetToMeters(y), startLat, startLong, type);
        double[] res = UtilsLatLongConversion.convertFeetOffsetToLatLong(x, y, startLat, startLong, type);

        assertEquals(ret[0], res[0]);
        assertEquals(ret[1], res[1]);
    }

    public void testConvertCentimeterOffsetToLatLong() {
        double x = 2;
        double y = 3;
        double startLat = 4;
        double startLong = 5;
        int type = UtilsLatLongConversion.CARTESIAN;

        double[] ret = UtilsLatLongConversion.convertMeterOffsetToLatLong(UtilsUnitConversion.convertCentimetersToMeters(x),
                UtilsUnitConversion.convertCentimetersToMeters(y), startLat, startLong, type);
        double[] res = UtilsLatLongConversion.convertCentimeterOffsetToLatLong(x, y, startLat, startLong, type);

        assertEquals(ret[0], res[0]);
        assertEquals(ret[1], res[1]);
    }

    public void testConvertCalculatorType() {
        assertEquals("Geodetic 2D", UtilsLatLongConversion.convertCalculatorType(UtilsLatLongConversion.GEODETIC2D));
        assertEquals("Geodetic 3D", UtilsLatLongConversion.convertCalculatorType(UtilsLatLongConversion.GEODETIC3D));
        assertEquals("Spherical", UtilsLatLongConversion.convertCalculatorType(UtilsLatLongConversion.SPHERICAL));
        assertEquals("Cartesian", UtilsLatLongConversion.convertCalculatorType(UtilsLatLongConversion.CARTESIAN));

        try {
            UtilsLatLongConversion.convertCalculatorType(10);
            fail("Expected to throw exception but did not.");
        }
        catch (RuntimeException e) {}

        assertEquals(UtilsLatLongConversion.GEODETIC2D, UtilsLatLongConversion.convertCalculatorType("Geodetic 2D"));
        assertEquals(UtilsLatLongConversion.GEODETIC3D, UtilsLatLongConversion.convertCalculatorType("Geodetic 3D"));
        assertEquals(UtilsLatLongConversion.SPHERICAL, UtilsLatLongConversion.convertCalculatorType("Spherical"));
        assertEquals(UtilsLatLongConversion.CARTESIAN, UtilsLatLongConversion.convertCalculatorType("Cartesian"));

        try {
            UtilsLatLongConversion.convertCalculatorType("not a type, lol");
            fail("Expected to throw exception but did not.");
        }
        catch (RuntimeException e) {}
    }

    public void testGetCoordinateReferenceSystem() {
        int type = 10; // not a type
        try {
            UtilsLatLongConversion.convertLatLongToMeterOffset(1, 2, 3, 4, type);
            fail("Expected to throw exception but did not.");
        }
        catch (RuntimeException e) {}
    }

    public void testConvertFeetToCentimeters() {
        assertEquals(30.48, UtilsLatLongConversion.convertFeetToCentimeters(1.0), .0005);
    }

    public void testConstructor() {
        try {
            UtilsLatLongConversion obj = new UtilsLatLongConversion();
        }
        catch (Exception e) {
            fail();
        }
    }
}
