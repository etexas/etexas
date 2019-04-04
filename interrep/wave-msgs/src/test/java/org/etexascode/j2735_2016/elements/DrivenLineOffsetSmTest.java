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
 * Unit tests for the driven line offset small element.
 * 
 * @author ttevendale
 */
public class DrivenLineOffsetSmTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MIN);
        assertTrue(DrivenLineOffsetSm.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MAX);
        assertTrue(DrivenLineOffsetSm.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -222;
        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testSetValueMin() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();
        offset.setValue(DrivenLineOffsetSm.MIN);
        assertTrue(DrivenLineOffsetSm.MIN == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(DrivenLineOffsetSm.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();
        offset.setValue(DrivenLineOffsetSm.MAX);
        assertTrue(DrivenLineOffsetSm.MAX == offset.getValue());

        thrown.expect(IllegalArgumentException.class);
        offset.setValue(DrivenLineOffsetSm.MAX + 1);
    }

    @Test
    public void testSetValue() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();
        int randomNum = 789;
        offset.setValue(randomNum);
        assertTrue(randomNum == offset.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MIN);
        String encodedOffset = offset.encodeUPER();
        assertTrue("000000000000".equals(encodedOffset));

        // test max
        offset = new DrivenLineOffsetSm(DrivenLineOffsetSm.MAX);
        encodedOffset = offset.encodeUPER();
        assertTrue("111111111110".equals(encodedOffset));

        int randomNumber = 0;
        offset = new DrivenLineOffsetSm(randomNumber);
        encodedOffset = offset.encodeUPER();
        assertTrue("011111111111".equals(encodedOffset));
    }

    @Test
    public void testDecodeUPER() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();

        // test min
        String remainingBits = offset.decodeUPER("000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(DrivenLineOffsetSm.MIN == offset.getValue());

        // test max
        remainingBits = offset.decodeUPER("111111111110");
        assertTrue("".equals(remainingBits));
        assertTrue(DrivenLineOffsetSm.MAX == offset.getValue());

        int expectedNumber = -1108;
        remainingBits = offset.decodeUPER("001110101011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == offset.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();
        thrown.expect(IllegalArgumentException.class);
        offset.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm();
        String remainingBits = offset.decodeUPER("1000101010000111110");
        assertTrue("0111110".equals(remainingBits));
        assertTrue(offset.getValue() == 169);
    }

    @Test
    public void testHashCode() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(5);
        DrivenLineOffsetSm offset2 = new DrivenLineOffsetSm(10);

        assertFalse(offset.hashCode() == offset2.hashCode());
        assertTrue(offset.hashCode() == offset.hashCode());
        assertTrue(offset2.hashCode() == offset2.hashCode());

        DrivenLineOffsetSm offset3 = new DrivenLineOffsetSm(offset.getValue());

        assertTrue(offset.hashCode() == offset3.hashCode());
        assertFalse(offset2.hashCode() == offset3.hashCode());
    }

    @Test
    public void testEquals() {

        DrivenLineOffsetSm offset = new DrivenLineOffsetSm(5);

        assertFalse(offset.equals(null));

        assertTrue(offset.equals(offset));

        DrivenLineOffsetSm offset2 = new DrivenLineOffsetSm(offset.getValue() + 5);

        assertFalse(offset.equals(new String()));
        assertFalse(offset.equals(offset2));

        offset2.setValue(offset.getValue());
        assertTrue(offset.equals(offset2));
    }
}
