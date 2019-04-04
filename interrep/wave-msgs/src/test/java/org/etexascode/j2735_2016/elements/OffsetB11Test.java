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
 * Unit tests for the offset b11 element.
 * 
 * @author ttevendale
 */
public class OffsetB11Test {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        OffsetB11 offset = new OffsetB11(OffsetB11.MIN);
        assertTrue(OffsetB11.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new OffsetB11(OffsetB11.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        OffsetB11 offset = new OffsetB11(OffsetB11.MAX);
        assertTrue(OffsetB11.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new OffsetB11(OffsetB11.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -789;
        OffsetB11 offset = new OffsetB11(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testSetValueMin() {

        OffsetB11 offset = new OffsetB11();
        offset.setValue(OffsetB11.MIN);
        assertTrue(OffsetB11.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(OffsetB11.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        OffsetB11 offset = new OffsetB11();
        offset.setValue(OffsetB11.MAX);
        assertTrue(OffsetB11.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(OffsetB11.MAX + 1);
    }

    @Test
    public void testSetValue() {

        OffsetB11 offset = new OffsetB11();
        int randomNum = 599;
        offset.setValue(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        OffsetB11 offset = new OffsetB11(OffsetB11.MIN);
        String encodedOffset = offset.encodeUPER();
        assertTrue("00000000000".equals(encodedOffset));

        // test max
        offset = new OffsetB11(OffsetB11.MAX);
        encodedOffset = offset.encodeUPER();
        assertTrue("11111111111".equals(encodedOffset));

        int randomNumber = 0;
        offset = new OffsetB11(randomNumber);
        encodedOffset = offset.encodeUPER();
        assertTrue("10000000000".equals(encodedOffset));
    }

    @Test
    public void testDecodeUPER() {

        OffsetB11 offset = new OffsetB11();

        // test min
        String remainingBits = offset.decodeUPER("00000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(OffsetB11.MIN == offset.getValue());

        // test max
        remainingBits = offset.decodeUPER("11111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(OffsetB11.MAX == offset.getValue());

        int expectedNumber = 351;
        remainingBits = offset.decodeUPER("10101011111");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == offset.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        OffsetB11 offset = new OffsetB11();
        thrown.expect(IllegalArgumentException.class);
        offset.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        OffsetB11 offset = new OffsetB11();
        String remainingBits = offset.decodeUPER("001010001010");
        assertTrue("0".equals(remainingBits));
        assertTrue(offset.getValue() == -699);
    }

    @Test
    public void testHashCode() {

        OffsetB11 offset = new OffsetB11(5);
        OffsetB11 offset2 = new OffsetB11(10);

        assertFalse(offset.hashCode() == offset2.hashCode());
        assertTrue(offset.hashCode() == offset.hashCode());
        assertTrue(offset2.hashCode() == offset2.hashCode());

        OffsetB11 offset3 = new OffsetB11(offset.getValue());

        assertTrue(offset.hashCode() == offset3.hashCode());
        assertFalse(offset2.hashCode() == offset3.hashCode());
    }

    @Test
    public void testEquals() {

        OffsetB11 offset = new OffsetB11(5);

        assertFalse(offset.equals(null));

        assertTrue(offset.equals(offset));

        OffsetB11 offset2 = new OffsetB11(offset.getValue() + 5);

        assertFalse(offset.equals(new String()));
        assertFalse(offset.equals(offset2));

        offset2.setValue(offset.getValue());
        assertTrue(offset.equals(offset2));
    }
}
