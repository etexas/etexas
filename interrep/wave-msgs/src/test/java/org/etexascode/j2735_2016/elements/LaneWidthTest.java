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
package org.etexascode.j2735_2016.elements;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the lane width element.
 * 
 * @author ttevendale
 */
public class LaneWidthTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        LaneWidth width = new LaneWidth(LaneWidth.MIN);
        assertTrue(LaneWidth.MIN == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width = new LaneWidth(LaneWidth.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        LaneWidth width = new LaneWidth(LaneWidth.MAX);
        assertTrue(LaneWidth.MAX == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width = new LaneWidth(LaneWidth.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 32132;
        LaneWidth width = new LaneWidth(randomNum);
        assertTrue(randomNum == width.getValue());
    }

    @Test
    public void testSetValueMin() {

        LaneWidth width = new LaneWidth();
        width.setValue(LaneWidth.MIN);
        assertTrue(LaneWidth.MIN == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width.setValue(LaneWidth.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        LaneWidth width = new LaneWidth();
        width.setValue(LaneWidth.MAX);
        assertTrue(LaneWidth.MAX == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width.setValue(LaneWidth.MAX + 1);
    }

    @Test
    public void testSetValue() {

        LaneWidth width = new LaneWidth();
        int randomNum = 111;
        width.setValue(randomNum);
        assertTrue(randomNum == width.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        LaneWidth width = new LaneWidth(LaneWidth.MIN);
        String encodedWidth = width.encodeUPER();
        assertTrue("000000000000000".equals(encodedWidth));

        // test max
        width = new LaneWidth(LaneWidth.MAX);
        encodedWidth = width.encodeUPER();
        assertTrue("111111111111111".equals(encodedWidth));

        int randomNumber = 20149;
        width = new LaneWidth(randomNumber);
        encodedWidth = width.encodeUPER();
        assertTrue("100111010110101".equals(encodedWidth));
    }

    @Test
    public void testDecodeUPER() {

        LaneWidth width = new LaneWidth();

        // test min
        String remainingBits = width.decodeUPER("000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneWidth.MIN == width.getValue());

        // test max
        remainingBits = width.decodeUPER("111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneWidth.MAX == width.getValue());

        int expectedNumber = 25878;
        remainingBits = width.decodeUPER("110010100010110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == width.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneWidth width = new LaneWidth();
        thrown.expect(IllegalArgumentException.class);
        width.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneWidth width = new LaneWidth();
        String remainingBits = width.decodeUPER("000000000000001110");
        assertTrue("110".equals(remainingBits));
        assertTrue(width.getValue() == 1);
    }

    @Test
    public void testHashCode() {

        LaneWidth width = new LaneWidth(5);
        LaneWidth width2 = new LaneWidth(10);

        assertFalse(width.hashCode() == width2.hashCode());
        assertTrue(width.hashCode() == width.hashCode());
        assertTrue(width2.hashCode() == width2.hashCode());

        LaneWidth width3 = new LaneWidth(width.getValue());

        assertTrue(width.hashCode() == width3.hashCode());
        assertFalse(width2.hashCode() == width3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneWidth width = new LaneWidth(5);

        assertFalse(width.equals(null));

        assertTrue(width.equals(width));

        LaneWidth width2 = new LaneWidth(width.getValue() + 5);

        assertFalse(width.equals(new String()));
        assertFalse(width.equals(width2));

        width2.setValue(width.getValue());
        assertTrue(width.equals(width2));
    }
}
