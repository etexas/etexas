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
 * Unit tests for the semi minor axis accuracy element.
 * 
 * @author ttevendale
 */
public class SemiMinorAxisAccuracyTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MIN);
        assertTrue(SemiMinorAxisAccuracy.MIN == minorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MAX);
        assertTrue(SemiMinorAxisAccuracy.MAX == minorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 51;
        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy(randomNum);
        assertTrue(randomNum == minorAccuracy.getValue());
    }

    @Test
    public void testSetValueMin() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();
        minorAccuracy.setValue(SemiMinorAxisAccuracy.MIN);
        assertTrue(SemiMinorAxisAccuracy.MIN == minorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        minorAccuracy.setValue(SemiMinorAxisAccuracy.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();
        minorAccuracy.setValue(SemiMinorAxisAccuracy.MAX);
        assertTrue(SemiMinorAxisAccuracy.MAX == minorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        minorAccuracy.setValue(SemiMinorAxisAccuracy.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();
        int randomNum = 184;
        minorAccuracy.setValue(randomNum);
        assertTrue(randomNum == minorAccuracy.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MIN);
        String encodedMinorAccuracy = minorAccuracy.encodeUPER();
        assertTrue("00000000".equals(encodedMinorAccuracy));

        // test max
        minorAccuracy = new SemiMinorAxisAccuracy(SemiMinorAxisAccuracy.MAX);
        encodedMinorAccuracy = minorAccuracy.encodeUPER();
        assertTrue("11111111".equals(encodedMinorAccuracy));

        int randomNumber = 151;
        minorAccuracy = new SemiMinorAxisAccuracy(randomNumber);
        encodedMinorAccuracy = minorAccuracy.encodeUPER();
        assertTrue("10010111".equals(encodedMinorAccuracy));
    }

    @Test
    public void testDecodeUPER() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();

        // test min
        String remainingBits = minorAccuracy.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMinorAxisAccuracy.MIN == minorAccuracy.getValue());

        // test max
        remainingBits = minorAccuracy.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMinorAxisAccuracy.MAX == minorAccuracy.getValue());

        int expectedNumber = 200;
        remainingBits = minorAccuracy.decodeUPER("11001000");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == minorAccuracy.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();
        thrown.expect(IllegalArgumentException.class);
        minorAccuracy.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy();
        String remainingBits = minorAccuracy.decodeUPER("000010001");
        assertTrue("1".equals(remainingBits));
        assertTrue(minorAccuracy.getValue() == 8);
    }

    @Test
    public void testHashCode() {

        SemiMinorAxisAccuracy var = new SemiMinorAxisAccuracy(141);
        SemiMinorAxisAccuracy var2 = new SemiMinorAxisAccuracy(14);

        assertFalse(var.hashCode() == var2.hashCode());
        assertTrue(var.hashCode() == var.hashCode());
        assertTrue(var2.hashCode() == var2.hashCode());

        SemiMinorAxisAccuracy var3 = new SemiMinorAxisAccuracy(var.getValue());

        assertTrue(var.hashCode() == var3.hashCode());
        assertFalse(var2.hashCode() == var3.hashCode());
    }

    @Test
    public void testEquals() {

        SemiMinorAxisAccuracy minorAccuracy = new SemiMinorAxisAccuracy(12);

        assertFalse(minorAccuracy.equals(null));

        assertTrue(minorAccuracy.equals(minorAccuracy));

        SemiMinorAxisAccuracy minorAccuracy2 = new SemiMinorAxisAccuracy(255);

        assertFalse(minorAccuracy.equals(new String()));
        assertFalse(minorAccuracy.equals(minorAccuracy2));

        minorAccuracy2.setValue(minorAccuracy.getValue());
        assertTrue(minorAccuracy.equals(minorAccuracy2));
    }
}
