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
 * Unit tests for the semi major axis accuracy element.
 * 
 * @author ttevendale
 */
public class SemiMajorAxisAccuracyTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MIN);
        assertTrue(SemiMajorAxisAccuracy.MIN == majorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MAX);
        assertTrue(SemiMajorAxisAccuracy.MAX == majorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 84;
        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(randomNum);
        assertTrue(randomNum == majorAccuracy.getValue());
    }

    @Test
    public void testSetValueMin() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();
        majorAccuracy.setValue(SemiMajorAxisAccuracy.MIN);
        assertTrue(SemiMajorAxisAccuracy.MIN == majorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorAccuracy.setValue(SemiMajorAxisAccuracy.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();
        majorAccuracy.setValue(SemiMajorAxisAccuracy.MAX);
        assertTrue(SemiMajorAxisAccuracy.MAX == majorAccuracy.getValue());

        thrown.expect(IllegalArgumentException.class);
        majorAccuracy.setValue(SemiMajorAxisAccuracy.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();
        int randomNum = 184;
        majorAccuracy.setValue(randomNum);
        assertTrue(randomNum == majorAccuracy.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MIN);
        String encodedMajorAccuracy = majorAccuracy.encodeUPER();
        assertTrue("00000000".equals(encodedMajorAccuracy));

        // test max
        majorAccuracy = new SemiMajorAxisAccuracy(SemiMajorAxisAccuracy.MAX);
        encodedMajorAccuracy = majorAccuracy.encodeUPER();
        assertTrue("11111111".equals(encodedMajorAccuracy));

        int randomNumber = 200;
        majorAccuracy = new SemiMajorAxisAccuracy(randomNumber);
        encodedMajorAccuracy = majorAccuracy.encodeUPER();
        assertTrue("11001000".equals(encodedMajorAccuracy));
    }

    @Test
    public void testDecodeUPER() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();

        // test min
        String remainingBits = majorAccuracy.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMajorAxisAccuracy.MIN == majorAccuracy.getValue());

        // test max
        remainingBits = majorAccuracy.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(SemiMajorAxisAccuracy.MAX == majorAccuracy.getValue());

        int expectedNumber = 5;
        remainingBits = majorAccuracy.decodeUPER("00000101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == majorAccuracy.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();
        thrown.expect(IllegalArgumentException.class);
        majorAccuracy.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy();
        String remainingBits = majorAccuracy.decodeUPER("100000001");
        assertTrue("1".equals(remainingBits));
        assertTrue(majorAccuracy.getValue() == 128);
    }

    @Test
    public void testHashCode() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(255);
        SemiMajorAxisAccuracy majorAccuracy2 = new SemiMajorAxisAccuracy(0);

        assertFalse(majorAccuracy.hashCode() == majorAccuracy2.hashCode());
        assertTrue(majorAccuracy.hashCode() == majorAccuracy.hashCode());
        assertTrue(majorAccuracy2.hashCode() == majorAccuracy2.hashCode());

        SemiMajorAxisAccuracy majorAccuracy3 = new SemiMajorAxisAccuracy(majorAccuracy.getValue());

        assertTrue(majorAccuracy.hashCode() == majorAccuracy3.hashCode());
        assertFalse(majorAccuracy2.hashCode() == majorAccuracy3.hashCode());
    }

    @Test
    public void testEquals() {

        SemiMajorAxisAccuracy majorAccuracy = new SemiMajorAxisAccuracy(156);

        assertFalse(majorAccuracy.equals(null));

        assertTrue(majorAccuracy.equals(majorAccuracy));

        SemiMajorAxisAccuracy majorAccuracy2 = new SemiMajorAxisAccuracy(2);

        assertFalse(majorAccuracy.equals(new String()));
        assertFalse(majorAccuracy.equals(majorAccuracy2));

        majorAccuracy2.setValue(majorAccuracy.getValue());
        assertTrue(majorAccuracy.equals(majorAccuracy2));
    }
}
