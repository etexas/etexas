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
 * Unit tests for the message count element.
 * 
 * @author ttevendale
 */
public class MsgCountTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        MsgCount messageCount = new MsgCount(MsgCount.MIN);
        assertTrue(MsgCount.MIN == messageCount.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageCount = new MsgCount(MsgCount.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        MsgCount messageCount = new MsgCount(MsgCount.MAX);
        assertTrue(MsgCount.MAX == messageCount.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageCount = new MsgCount(MsgCount.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 120;
        MsgCount messageCount = new MsgCount(randomNum);
        assertTrue(randomNum == messageCount.getValue());
    }

    @Test
    public void testSetValueMin() {

        MsgCount messageCount = new MsgCount();
        messageCount.setValue(MsgCount.MIN);
        assertTrue(MsgCount.MIN == messageCount.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageCount.setValue(MsgCount.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        MsgCount messageCount = new MsgCount();
        messageCount.setValue(MsgCount.MAX);
        assertTrue(MsgCount.MAX == messageCount.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageCount.setValue(MsgCount.MAX + 1);
    }

    @Test
    public void testSetValue() {

        MsgCount messageCount = new MsgCount();
        int randomNum = 50;
        messageCount.setValue(randomNum);
        assertTrue(randomNum == messageCount.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        MsgCount messageCount = new MsgCount(MsgCount.MIN);
        String encodedMessageCount = messageCount.encodeUPER();
        assertTrue("0000000".equals(encodedMessageCount));

        // test max
        messageCount = new MsgCount(MsgCount.MAX);
        encodedMessageCount = messageCount.encodeUPER();
        assertTrue("1111111".equals(encodedMessageCount));

        int randomNumber = 23;
        messageCount = new MsgCount(randomNumber);
        encodedMessageCount = messageCount.encodeUPER();
        assertTrue("0010111".equals(encodedMessageCount));
    }

    @Test
    public void testDecodeUPER() {

        MsgCount messageCount = new MsgCount();

        // test min
        String remainingBits = messageCount.decodeUPER("0000000");
        assertTrue("".equals(remainingBits));
        assertTrue(MsgCount.MIN == messageCount.getValue());

        // test max
        remainingBits = messageCount.decodeUPER("1111111");
        assertTrue("".equals(remainingBits));
        assertTrue(MsgCount.MAX == messageCount.getValue());

        int expectedNumber = 80;
        remainingBits = messageCount.decodeUPER("1010000");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == messageCount.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        MsgCount messageCount = new MsgCount();
        thrown.expect(IllegalArgumentException.class);
        messageCount.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        MsgCount messageCount = new MsgCount();
        String remainingBits = messageCount.decodeUPER("01010101");
        assertTrue("1".equals(remainingBits));
        assertTrue(messageCount.getValue() == 42);
    }

    @Test
    public void testHashCode() {

        MsgCount messageCount = new MsgCount(1);
        MsgCount messageCount2 = new MsgCount(0);

        assertFalse(messageCount.hashCode() == messageCount2.hashCode());
        assertTrue(messageCount.hashCode() == messageCount.hashCode());
        assertTrue(messageCount2.hashCode() == messageCount2.hashCode());

        MsgCount messageCount3 = new MsgCount(messageCount.getValue());

        assertTrue(messageCount.hashCode() == messageCount3.hashCode());
        assertFalse(messageCount2.hashCode() == messageCount3.hashCode());
    }

    @Test
    public void testEquals() {

        MsgCount messageCount = new MsgCount(126);

        assertFalse(messageCount.equals(null));

        assertTrue(messageCount.equals(messageCount));

        MsgCount messageCount2 = new MsgCount(5);

        assertFalse(messageCount.equals(new String()));
        assertFalse(messageCount.equals(messageCount2));

        messageCount2.setValue(messageCount.getValue());
        assertTrue(messageCount.equals(messageCount2));
    }
}
