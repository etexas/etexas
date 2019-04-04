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
 * Unit tests for the driven line offset large element.
 * 
 * @author ttevendale
 */
public class DrivenLineOffsetLgTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MIN);
        assertTrue(DrivenLineOffsetLg.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MAX);
        assertTrue(DrivenLineOffsetLg.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 31201;
        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testSetValueMin() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();
        offset.setValue(DrivenLineOffsetLg.MIN);
        assertTrue(DrivenLineOffsetLg.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(DrivenLineOffsetLg.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();
        offset.setValue(DrivenLineOffsetLg.MAX);
        assertTrue(DrivenLineOffsetLg.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(DrivenLineOffsetLg.MAX + 1);
    }

    @Test
    public void testSetValue() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();
        int randomNum = -15454;
        offset.setValue(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MIN);
        String encodedOffset = offset.encodeUPER();
        assertTrue("0000000000000000".equals(encodedOffset));

        // test max
        offset = new DrivenLineOffsetLg(DrivenLineOffsetLg.MAX);
        encodedOffset = offset.encodeUPER();
        assertTrue("1111111111111110".equals(encodedOffset));

        int randomNumber = 0;
        offset = new DrivenLineOffsetLg(randomNumber);
        encodedOffset = offset.encodeUPER();
        assertTrue("0111111111111111".equals(encodedOffset));
    }

    @Test
    public void testDecodeUPER() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();

        // test min
        String remainingBits = offset.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(DrivenLineOffsetLg.MIN == offset.getValue());

        // test max
        remainingBits = offset.decodeUPER("1111111111111110");
        assertTrue("".equals(remainingBits));
        assertTrue(DrivenLineOffsetLg.MAX == offset.getValue());

        int expectedNumber = -31828;
        remainingBits = offset.decodeUPER("0000001110101011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == offset.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();
        thrown.expect(IllegalArgumentException.class);
        offset.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg();
        String remainingBits = offset.decodeUPER("110000111001010101");
        assertTrue("01".equals(remainingBits));
        assertTrue(offset.getValue() == 17302);
    }

    @Test
    public void testHashCode() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(5);
        DrivenLineOffsetLg offset2 = new DrivenLineOffsetLg(10);

        assertFalse(offset.hashCode() == offset2.hashCode());
        assertTrue(offset.hashCode() == offset.hashCode());
        assertTrue(offset2.hashCode() == offset2.hashCode());

        DrivenLineOffsetLg offset3 = new DrivenLineOffsetLg(offset.getValue());

        assertTrue(offset.hashCode() == offset3.hashCode());
        assertFalse(offset2.hashCode() == offset3.hashCode());
    }

    @Test
    public void testEquals() {

        DrivenLineOffsetLg offset = new DrivenLineOffsetLg(5);

        assertFalse(offset.equals(null));

        assertTrue(offset.equals(offset));

        DrivenLineOffsetLg offset2 = new DrivenLineOffsetLg(offset.getValue() + 5);

        assertFalse(offset.equals(new String()));
        assertFalse(offset.equals(offset2));

        offset2.setValue(offset.getValue());
        assertTrue(offset.equals(offset2));
    }
}
