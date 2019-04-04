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
 * Unit tests for the semi major axis orientation element.
 * 
 * @author ttevendale
 */
public class SemiMajorAxisOrientationTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MIN);
        assertTrue(SemiMajorAxisOrientation.MIN == majorOrientation.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MAX);
        assertTrue(SemiMajorAxisOrientation.MAX == majorOrientation.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 18756;
        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(randomNum);
        assertTrue(randomNum == majorOrientation.getValue());
    }

    @Test
    public void testSetValueMin() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();
        majorOrientation.setValue(SemiMajorAxisOrientation.MIN);
        assertTrue(SemiMajorAxisOrientation.MIN == majorOrientation.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorOrientation.setValue(SemiMajorAxisOrientation.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();
        majorOrientation.setValue(SemiMajorAxisOrientation.MAX);
        assertTrue(SemiMajorAxisOrientation.MAX == majorOrientation.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorOrientation.setValue(SemiMajorAxisOrientation.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();
        int randomNum = 184;
        majorOrientation.setValue(randomNum);
        assertTrue(randomNum == majorOrientation.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MIN);
        String encodedMajorOrientation = majorOrientation.encodeUPER();
        assertTrue("0000000000000000".equals(encodedMajorOrientation));

        // test max
        majorOrientation = new SemiMajorAxisOrientation(SemiMajorAxisOrientation.MAX);
        encodedMajorOrientation = majorOrientation.encodeUPER();
        assertTrue("1111111111111111".equals(encodedMajorOrientation));

        int randomNumber = 47894;
        majorOrientation = new SemiMajorAxisOrientation(randomNumber);
        encodedMajorOrientation = majorOrientation.encodeUPER();
        assertTrue("1011101100010110".equals(encodedMajorOrientation));
    }

    @Test
    public void testDecodeUPER() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();

        // test min
        String remainingBits = majorOrientation.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMajorAxisOrientation.MIN == majorOrientation.getValue());

        // test max
        remainingBits = majorOrientation.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMajorAxisOrientation.MAX == majorOrientation.getValue());

        int expectedNumber = 5789;
        remainingBits = majorOrientation.decodeUPER("0001011010011101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == majorOrientation.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();
        thrown.expect(IllegalArgumentException.class);
        majorOrientation.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation();
        String remainingBits = majorOrientation.decodeUPER("00000000000100001");
        assertTrue("1".equals(remainingBits));
        assertTrue(majorOrientation.getValue() == 16);
    }

    @Test
    public void testHashCode() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(65502);
        SemiMajorAxisOrientation majorOrientation2 = new SemiMajorAxisOrientation(20);

        assertFalse(majorOrientation.hashCode() == majorOrientation2.hashCode());
        assertTrue(majorOrientation.hashCode() == majorOrientation.hashCode());
        assertTrue(majorOrientation2.hashCode() == majorOrientation2.hashCode());

        SemiMajorAxisOrientation majorOrientation3 = new SemiMajorAxisOrientation(majorOrientation.getValue());

        assertTrue(majorOrientation.hashCode() == majorOrientation3.hashCode());
        assertFalse(majorOrientation2.hashCode() == majorOrientation3.hashCode());
    }

    @Test
    public void testEquals() {

        SemiMajorAxisOrientation majorOrientation = new SemiMajorAxisOrientation(1);

        assertFalse(majorOrientation.equals(null));

        assertTrue(majorOrientation.equals(majorOrientation));

        SemiMajorAxisOrientation majorOrientation2 = new SemiMajorAxisOrientation(2);

        assertFalse(majorOrientation.equals(new String()));
        assertFalse(majorOrientation.equals(majorOrientation2));

        majorOrientation2.setValue(majorOrientation.getValue());
        assertTrue(majorOrientation.equals(majorOrientation2));
    }
}
