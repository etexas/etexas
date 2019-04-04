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
 * Unit tests for the zone length element.
 * 
 * @author ttevendale
 */
public class ZoneLengthTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        ZoneLength length = new ZoneLength(ZoneLength.MIN);
        assertTrue(ZoneLength.MIN == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length = new ZoneLength(ZoneLength.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        ZoneLength length = new ZoneLength(ZoneLength.MAX);
        assertTrue(ZoneLength.MAX == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length = new ZoneLength(ZoneLength.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 555;
        ZoneLength length = new ZoneLength(randomNum);
        assertTrue(randomNum == length.getValue());
    }

    @Test
    public void testSetValueMin() {

        ZoneLength length = new ZoneLength();
        length.setValue(ZoneLength.MIN);
        assertTrue(ZoneLength.MIN == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length.setValue(ZoneLength.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        ZoneLength length = new ZoneLength();
        length.setValue(ZoneLength.MAX);
        assertTrue(ZoneLength.MAX == length.getValue());

        thrown.expect(IllegalArgumentException.class);
        length.setValue(ZoneLength.MAX + 1);
    }

    @Test
    public void testSetValue() {

        ZoneLength length = new ZoneLength();
        int randomNum = 9784;
        length.setValue(randomNum);
        assertTrue(randomNum == length.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        ZoneLength length = new ZoneLength(ZoneLength.MIN);
        String encodedLength = length.encodeUPER();
        assertTrue("00000000000000".equals(encodedLength));

        // test max
        length = new ZoneLength(ZoneLength.MAX);
        encodedLength = length.encodeUPER();
        assertTrue("10011100010000".equals(encodedLength));

        int randomNumber = 5555;
        length = new ZoneLength(randomNumber);
        encodedLength = length.encodeUPER();
        assertTrue("01010110110011".equals(encodedLength));
    }

    @Test
    public void testDecodeUPER() {

        ZoneLength length = new ZoneLength();

        // test min
        String remainingBits = length.decodeUPER("00000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(ZoneLength.MIN == length.getValue());

        // test max
        remainingBits = length.decodeUPER("10011100010000");
        assertTrue("".equals(remainingBits));
        assertTrue(ZoneLength.MAX == length.getValue());

        int expectedNumber = 5555;
        remainingBits = length.decodeUPER("01010110110011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == length.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        ZoneLength length = new ZoneLength();
        thrown.expect(IllegalArgumentException.class);
        length.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        ZoneLength length = new ZoneLength();
        String remainingBits = length.decodeUPER("100110100101000000");
        assertTrue("0000".equals(remainingBits));
        assertTrue(length.getValue() == 9876);
    }

    @Test
    public void testHashCode() {

        ZoneLength length = new ZoneLength(5);
        ZoneLength length2 = new ZoneLength(10);

        assertFalse(length.hashCode() == length2.hashCode());
        assertTrue(length.hashCode() == length.hashCode());
        assertTrue(length2.hashCode() == length2.hashCode());

        ZoneLength length3 = new ZoneLength(length.getValue());

        assertTrue(length.hashCode() == length3.hashCode());
        assertFalse(length2.hashCode() == length3.hashCode());
    }

    @Test
    public void testEquals() {

        ZoneLength length = new ZoneLength(5);

        assertFalse(length.equals(null));

        assertTrue(length.equals(length));

        ZoneLength length2 = new ZoneLength(length.getValue() + 5);

        assertFalse(length.equals(new String()));
        assertFalse(length.equals(length2));

        length2.setValue(length.getValue());
        assertTrue(length.equals(length2));
    }
}
