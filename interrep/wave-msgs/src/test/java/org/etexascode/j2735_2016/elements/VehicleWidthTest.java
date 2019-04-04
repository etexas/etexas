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
 * Unit tests for the vehicle width element.
 * 
 * @author ttevendale
 */
public class VehicleWidthTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        VehicleWidth width = new VehicleWidth(VehicleWidth.MIN);
        assertTrue(VehicleWidth.MIN == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width = new VehicleWidth(VehicleWidth.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        VehicleWidth width = new VehicleWidth(VehicleWidth.MAX);
        assertTrue(VehicleWidth.MAX == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width = new VehicleWidth(VehicleWidth.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 500;
        VehicleWidth width = new VehicleWidth(randomNum);
        assertTrue(randomNum == width.getValue());
    }

    @Test
    public void testSetValueMin() {

        VehicleWidth width = new VehicleWidth();
        width.setValue(VehicleWidth.MIN);
        assertTrue(VehicleWidth.MIN == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width.setValue(VehicleWidth.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        VehicleWidth width = new VehicleWidth();
        width.setValue(VehicleWidth.MAX);
        assertTrue(VehicleWidth.MAX == width.getValue());

        thrown.expect(IllegalArgumentException.class);
        width.setValue(VehicleWidth.MAX + 1);
    }

    @Test
    public void testSetValue() {

        VehicleWidth width = new VehicleWidth();
        int randomNum = 184;
        width.setValue(randomNum);
        assertTrue(randomNum == width.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        VehicleWidth width = new VehicleWidth(VehicleWidth.MIN);
        String encodedWidth = width.encodeUPER();
        assertTrue("0000000000".equals(encodedWidth));

        // test max
        width = new VehicleWidth(VehicleWidth.MAX);
        encodedWidth = width.encodeUPER();
        assertTrue("1111111111".equals(encodedWidth));

        int randomNumber = 350;
        width = new VehicleWidth(randomNumber);
        encodedWidth = width.encodeUPER();
        assertTrue("0101011110".equals(encodedWidth));
    }

    @Test
    public void testDecodeUPER() {

        VehicleWidth width = new VehicleWidth();

        // test min
        String remainingBits = width.decodeUPER("0000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(VehicleWidth.MIN == width.getValue());

        // test max
        remainingBits = width.decodeUPER("1111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(VehicleWidth.MAX == width.getValue());

        int expectedNumber = 750;
        remainingBits = width.decodeUPER("1011101110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == width.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        VehicleWidth width = new VehicleWidth();
        thrown.expect(IllegalArgumentException.class);
        width.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        VehicleWidth width = new VehicleWidth();
        String remainingBits = width.decodeUPER("0111000111111");
        assertTrue("111".equals(remainingBits));
        assertTrue(width.getValue() == 455);
    }

    @Test
    public void testHashCode() {

        VehicleWidth width = new VehicleWidth(484);
        VehicleWidth width2 = new VehicleWidth(181);

        assertFalse(width.hashCode() == width2.hashCode());
        assertTrue(width.hashCode() == width.hashCode());
        assertTrue(width2.hashCode() == width2.hashCode());

        VehicleWidth width3 = new VehicleWidth(width.getValue());

        assertTrue(width.hashCode() == width3.hashCode());
        assertFalse(width2.hashCode() == width3.hashCode());
    }

    @Test
    public void testEquals() {

        VehicleWidth width = new VehicleWidth(400);

        assertFalse(width.equals(null));

        assertTrue(width.equals(width));

        VehicleWidth width2 = new VehicleWidth(10);

        assertFalse(width.equals(new String()));
        assertFalse(width.equals(width2));

        width2.setValue(width.getValue());
        assertTrue(width.equals(width2));
    }
}
