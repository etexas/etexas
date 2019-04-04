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
 * Unit tests for the elevation element.
 * 
 * @author ttevendale
 */
public class ElevationTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Elevation elevation = new Elevation(Elevation.MIN);
        assertTrue(Elevation.MIN == elevation.getValue());

        thrown.expect(IllegalArgumentException.class);
        elevation = new Elevation(Elevation.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Elevation elevation = new Elevation(Elevation.MAX);
        assertTrue(Elevation.MAX == elevation.getValue());

        thrown.expect(IllegalArgumentException.class);
        elevation = new Elevation(Elevation.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 18756;
        Elevation elevation = new Elevation(randomNum);
        assertTrue(randomNum == elevation.getValue());
    }

    @Test
    public void testSetValueMin() {

        Elevation elevation = new Elevation();
        elevation.setValue(Elevation.MIN);
        assertTrue(Elevation.MIN == elevation.getValue());

        thrown.expect(IllegalArgumentException.class);
        elevation.setValue(Elevation.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Elevation elevation = new Elevation();
        elevation.setValue(Elevation.MAX);
        assertTrue(Elevation.MAX == elevation.getValue());

        thrown.expect(IllegalArgumentException.class);
        elevation.setValue(Elevation.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Elevation elevation = new Elevation();
        int randomNum = 184;
        elevation.setValue(randomNum);
        assertTrue(randomNum == elevation.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Elevation elevation = new Elevation(Elevation.MIN);
        String encodedElevation = elevation.encodeUPER();
        assertTrue("0000000000000000".equals(encodedElevation));

        // test max
        elevation = new Elevation(Elevation.MAX);
        encodedElevation = elevation.encodeUPER();
        assertTrue("1111111111111111".equals(encodedElevation));

        int randomNumber = 0;
        elevation = new Elevation(randomNumber);
        encodedElevation = elevation.encodeUPER();
        assertTrue("0001000000000000".equals(encodedElevation));
    }

    @Test
    public void testDecodeUPER() {

        Elevation elevation = new Elevation();

        // test min
        String remainingBits = elevation.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Elevation.MIN == elevation.getValue());

        // test max
        remainingBits = elevation.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(Elevation.MAX == elevation.getValue());

        int expectedNumber = 515;
        remainingBits = elevation.decodeUPER("0001001000000011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == elevation.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Elevation elevation = new Elevation();
        thrown.expect(IllegalArgumentException.class);
        elevation.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Elevation elevation = new Elevation();
        String remainingBits = elevation.decodeUPER("00000000000011000");
        assertTrue("0".equals(remainingBits));
        assertTrue(elevation.getValue() == -4084);
    }

    @Test
    public void testHashCode() {

        Elevation elevation = new Elevation(1891);
        Elevation elevation2 = new Elevation(-4095);

        assertFalse(elevation.hashCode() == elevation2.hashCode());
        assertTrue(elevation.hashCode() == elevation.hashCode());
        assertTrue(elevation2.hashCode() == elevation2.hashCode());

        Elevation elevation3 = new Elevation(elevation.getValue());

        assertTrue(elevation.hashCode() == elevation3.hashCode());
        assertFalse(elevation2.hashCode() == elevation3.hashCode());
    }

    @Test
    public void testEquals() {

        Elevation elevation = new Elevation(-404);

        assertFalse(elevation.equals(null));

        assertTrue(elevation.equals(elevation));

        Elevation elevation2 = new Elevation(8000);

        assertFalse(elevation.equals(new String()));
        assertFalse(elevation.equals(elevation2));

        elevation2.setValue(elevation.getValue());
        assertTrue(elevation.equals(elevation2));
    }
}
