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
 * Unit tests for the vehicle length element.
 * 
 * @author ttevendale
 */
public class VehicleLengthTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        VehicleLength length = new VehicleLength(VehicleLength.MIN);
        assertTrue(VehicleLength.MIN == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length = new VehicleLength(VehicleLength.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        VehicleLength length = new VehicleLength(VehicleLength.MAX);
        assertTrue(VehicleLength.MAX == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length = new VehicleLength(VehicleLength.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 2525;
        VehicleLength length = new VehicleLength(randomNum);
        assertTrue(randomNum == length.getValue());
    }

    @Test
    public void testSetValueMin() {

        VehicleLength length = new VehicleLength();
        length.setValue(VehicleLength.MIN);
        assertTrue(VehicleLength.MIN == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length.setValue(VehicleLength.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        VehicleLength length = new VehicleLength();
        length.setValue(VehicleLength.MAX);
        assertTrue(VehicleLength.MAX == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length.setValue(VehicleLength.MAX + 1);
    }

    @Test
    public void testSetValue() {

        VehicleLength length = new VehicleLength();
        int randomNum = 184;
        length.setValue(randomNum);
        assertTrue(randomNum == length.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        VehicleLength length = new VehicleLength(VehicleLength.MIN);
        String encodedLength = length.encodeUPER();
        assertTrue("000000000000".equals(encodedLength));

        // test max
        length = new VehicleLength(VehicleLength.MAX);
        encodedLength = length.encodeUPER();
        assertTrue("111111111111".equals(encodedLength));

        int randomNumber = 200;
        length = new VehicleLength(randomNumber);
        encodedLength = length.encodeUPER();
        assertTrue("000011001000".equals(encodedLength));
    }

    @Test
    public void testDecodeUPER() {

        VehicleLength length = new VehicleLength();

        // test min
        String remainingBits = length.decodeUPER("000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(VehicleLength.MIN == length.getValue());

        // test max
        remainingBits = length.decodeUPER("111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(VehicleLength.MAX == length.getValue());

        int expectedNumber = 800;
        remainingBits = length.decodeUPER("001100100000");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == length.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        VehicleLength length = new VehicleLength();
        thrown.expect(IllegalArgumentException.class);
        length.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        VehicleLength length = new VehicleLength();
        String remainingBits = length.decodeUPER("1101101011000101");
        assertTrue("0101".equals(remainingBits));
        assertTrue(length.getValue() == 3500);
    }

    @Test
    public void testHashCode() {

        VehicleLength length = new VehicleLength(444);
        VehicleLength length2 = new VehicleLength(111);

        assertFalse(length.hashCode() == length2.hashCode());
        assertTrue(length.hashCode() == length.hashCode());
        assertTrue(length2.hashCode() == length2.hashCode());

        VehicleLength length3 = new VehicleLength(length.getValue());

        assertTrue(length.hashCode() == length3.hashCode());
        assertFalse(length2.hashCode() == length3.hashCode());
    }

    @Test
    public void testEquals() {

        VehicleLength length = new VehicleLength(50);

        assertFalse(length.equals(null));

        assertTrue(length.equals(length));

        VehicleLength length2 = new VehicleLength(800);

        assertFalse(length.equals(new String()));
        assertFalse(length.equals(length2));

        length2.setValue(length.getValue());
        assertTrue(length.equals(length2));
    }
}
