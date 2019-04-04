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
 * Unit tests for the longitude element.
 * 
 * @author ttevendale
 */
public class LongitudeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Longitude longitude = new Longitude(Longitude.MIN);
        assertTrue(Longitude.MIN == longitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        longitude = new Longitude(Longitude.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Longitude longitude = new Longitude(Longitude.MAX);
        assertTrue(Longitude.MAX == longitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        longitude = new Longitude(Longitude.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 180106;
        Longitude longitude = new Longitude(randomNum);
        assertTrue(randomNum == longitude.getValue());
    }

    @Test
    public void testSetValueMin() {

        Longitude longitude = new Longitude();
        longitude.setValue(Longitude.MIN);
        assertTrue(Longitude.MIN == longitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        longitude.setValue(Longitude.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Longitude longitude = new Longitude();
        longitude.setValue(Longitude.MAX);
        assertTrue(Longitude.MAX == longitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        longitude.setValue(Longitude.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Longitude longitude = new Longitude();
        int randomNum = 184;
        longitude.setValue(randomNum);
        assertTrue(randomNum == longitude.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Longitude longitude = new Longitude(Longitude.MIN);
        String encodedLongitude = longitude.encodeUPER();
        assertTrue("00000000000000000000000000000000".equals(encodedLongitude));

        // test max
        longitude = new Longitude(Longitude.MAX);
        encodedLongitude = longitude.encodeUPER();
        assertTrue("11010110100100111010010000000000".equals(encodedLongitude));

        int randomNumber = 124568744;
        longitude = new Longitude(randomNumber);
        encodedLongitude = longitude.encodeUPER();
        assertTrue("01110010101101101001011010100111".equals(encodedLongitude));
    }

    @Test
    public void testDecodeUPER() {

        Longitude longitude = new Longitude();

        // test min
        String remainingBits = longitude.decodeUPER("00000000000000000000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Longitude.MIN == longitude.getValue());

        // test max
        remainingBits = longitude.decodeUPER("11010110100100111010010000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Longitude.MAX == longitude.getValue());

        int expectedNumber = 89755;
        remainingBits = longitude.decodeUPER("01101011010010110011000010011010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == longitude.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Longitude longitude = new Longitude();
        thrown.expect(IllegalArgumentException.class);
        longitude.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Longitude longitude = new Longitude();
        String remainingBits = longitude.decodeUPER("011010110100100111010001111111110");
        assertTrue("0".equals(remainingBits));
        assertTrue(longitude.getValue() == 0);
    }

    @Test
    public void testHashCode() {

        Longitude longitude = new Longitude(54851);
        Longitude longitude2 = new Longitude(874871);

        assertFalse(longitude.hashCode() == longitude2.hashCode());
        assertTrue(longitude.hashCode() == longitude.hashCode());
        assertTrue(longitude2.hashCode() == longitude2.hashCode());

        Longitude longitude3 = new Longitude(longitude.getValue());

        assertTrue(longitude.hashCode() == longitude3.hashCode());
        assertFalse(longitude2.hashCode() == longitude3.hashCode());
    }

    @Test
    public void testEquals() {

        Longitude longitude = new Longitude(-123456789);

        assertFalse(longitude.equals(null));

        assertTrue(longitude.equals(longitude));

        Longitude longitude2 = new Longitude(987654321);

        assertFalse(longitude.equals(new String()));
        assertFalse(longitude.equals(longitude2));

        longitude2.setValue(longitude.getValue());
        assertTrue(longitude.equals(longitude2));
    }
}
