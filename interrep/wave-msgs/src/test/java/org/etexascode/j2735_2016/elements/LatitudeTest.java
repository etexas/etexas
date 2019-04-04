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
 * Unit tests for the latitude element.
 * 
 * @author ttevendale
 */
public class LatitudeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Latitude latitude = new Latitude(Latitude.MIN);
        assertTrue(Latitude.MIN == latitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        latitude = new Latitude(Latitude.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Latitude latitude = new Latitude(Latitude.MAX);
        assertTrue(Latitude.MAX == latitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        latitude = new Latitude(Latitude.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 180106;
        Latitude latitude = new Latitude(randomNum);
        assertTrue(randomNum == latitude.getValue());
    }

    @Test
    public void testSetValueMin() {

        Latitude latitude = new Latitude();
        latitude.setValue(Latitude.MIN);
        assertTrue(Latitude.MIN == latitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        latitude.setValue(Latitude.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Latitude latitude = new Latitude();
        latitude.setValue(Latitude.MAX);
        assertTrue(Latitude.MAX == latitude.getValue());

        thrown.expect(IllegalArgumentException.class);
        latitude.setValue(Latitude.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Latitude latitude = new Latitude();
        int randomNum = 184;
        latitude.setValue(randomNum);
        assertTrue(randomNum == latitude.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Latitude latitude = new Latitude(Latitude.MIN);
        String encodedLatitude = latitude.encodeUPER();
        assertTrue("0000000000000000000000000000000".equals(encodedLatitude));

        // test max
        latitude = new Latitude(Latitude.MAX);
        encodedLatitude = latitude.encodeUPER();
        assertTrue("1101011010010011101001000000001".equals(encodedLatitude));

        int randomNumber = 1568744;
        latitude = new Latitude(randomNumber);
        encodedLatitude = latitude.encodeUPER();
        assertTrue("0110101101111001101100011101000".equals(encodedLatitude));
    }

    @Test
    public void testDecodeUPER() {

        Latitude latitude = new Latitude();

        // test min
        String remainingBits = latitude.decodeUPER("0000000000000000000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Latitude.MIN == latitude.getValue());

        // test max
        remainingBits = latitude.decodeUPER("1101011010010011101001000000001");
        assertTrue("".equals(remainingBits));
        assertTrue(Latitude.MAX == latitude.getValue());

        int expectedNumber = 89755;
        remainingBits = latitude.decodeUPER("0110101101001100100011110011011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == latitude.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Latitude latitude = new Latitude();
        thrown.expect(IllegalArgumentException.class);
        latitude.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Latitude latitude = new Latitude();
        String remainingBits = latitude.decodeUPER("00000000000000000000000000010001");
        assertTrue("1".equals(remainingBits));
        assertTrue(latitude.getValue() == -899999992);
    }

    @Test
    public void testHashCode() {

        Latitude latitude = new Latitude(-89451487);
        Latitude latitude2 = new Latitude(25745418);

        assertFalse(latitude.hashCode() == latitude2.hashCode());
        assertTrue(latitude.hashCode() == latitude.hashCode());
        assertTrue(latitude2.hashCode() == latitude2.hashCode());

        Latitude latitude3 = new Latitude(latitude.getValue());

        assertTrue(latitude.hashCode() == latitude3.hashCode());
        assertFalse(latitude2.hashCode() == latitude3.hashCode());
    }

    @Test
    public void testEquals() {

        Latitude latitude = new Latitude(-10000);

        assertFalse(latitude.equals(null));

        assertTrue(latitude.equals(latitude));

        Latitude latitude2 = new Latitude(123456);

        assertFalse(latitude.equals(new String()));
        assertFalse(latitude.equals(latitude2));

        latitude2.setValue(latitude.getValue());
        assertTrue(latitude.equals(latitude2));
    }
}
