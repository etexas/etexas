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
 * Unit tests for the DSRC message ID element.
 * 
 * @author ttevendale
 */
public class DSRCmsgIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        DSRCmsgID messageId = new DSRCmsgID(DSRCmsgID.MIN);
        assertTrue(DSRCmsgID.MIN == messageId.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageId = new DSRCmsgID(DSRCmsgID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        DSRCmsgID messageId = new DSRCmsgID(DSRCmsgID.MAX);
        assertTrue(DSRCmsgID.MAX == messageId.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageId = new DSRCmsgID(DSRCmsgID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 120;
        DSRCmsgID messageId = new DSRCmsgID(randomNum);
        assertTrue(randomNum == messageId.getValue());
    }

    @Test
    public void testSetValueMin() {

        DSRCmsgID messageId = new DSRCmsgID();
        messageId.setValue(DSRCmsgID.MIN);
        assertTrue(DSRCmsgID.MIN == messageId.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageId.setValue(DSRCmsgID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        DSRCmsgID messageId = new DSRCmsgID();
        messageId.setValue(DSRCmsgID.MAX);
        assertTrue(DSRCmsgID.MAX == messageId.getValue());

        thrown.expect(IllegalArgumentException.class);
        messageId.setValue(DSRCmsgID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        DSRCmsgID messageId = new DSRCmsgID();
        int randomNum = 50;
        messageId.setValue(randomNum);
        assertTrue(randomNum == messageId.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        DSRCmsgID messageId = new DSRCmsgID(DSRCmsgID.MIN);
        String encodedMessageId = messageId.encodeUPER();
        assertTrue("000000000000000".equals(encodedMessageId));

        // test max
        messageId = new DSRCmsgID(DSRCmsgID.MAX);
        encodedMessageId = messageId.encodeUPER();
        assertTrue("111111111111111".equals(encodedMessageId));

        int randomNumber = 8080;
        messageId = new DSRCmsgID(randomNumber);
        encodedMessageId = messageId.encodeUPER();
        assertTrue("001111110010000".equals(encodedMessageId));
    }

    @Test
    public void testDecodeUPER() {

        DSRCmsgID messageId = new DSRCmsgID();

        // test min
        String remainingBits = messageId.decodeUPER("000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(DSRCmsgID.MIN == messageId.getValue());

        // test max
        remainingBits = messageId.decodeUPER("111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(DSRCmsgID.MAX == messageId.getValue());

        int expectedNumber = 801;
        remainingBits = messageId.decodeUPER("000001100100001");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == messageId.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        DSRCmsgID messageId = new DSRCmsgID();
        thrown.expect(IllegalArgumentException.class);
        messageId.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        DSRCmsgID messageId = new DSRCmsgID();
        String remainingBits = messageId.decodeUPER("0000000000000110");
        assertTrue("0".equals(remainingBits));
        assertTrue(messageId.getValue() == 3);
    }

    @Test
    public void testHashCode() {

        DSRCmsgID messageId = new DSRCmsgID(987);
        DSRCmsgID messageId2 = new DSRCmsgID(789);

        assertFalse(messageId.hashCode() == messageId2.hashCode());
        assertTrue(messageId.hashCode() == messageId.hashCode());
        assertTrue(messageId2.hashCode() == messageId2.hashCode());

        DSRCmsgID messageId3 = new DSRCmsgID(messageId.getValue());

        assertTrue(messageId.hashCode() == messageId3.hashCode());
        assertFalse(messageId2.hashCode() == messageId3.hashCode());
    }

    @Test
    public void testEquals() {

        DSRCmsgID messageId = new DSRCmsgID(20);

        assertFalse(messageId.equals(null));

        assertTrue(messageId.equals(messageId));

        DSRCmsgID messageId2 = new DSRCmsgID(22);

        assertFalse(messageId.equals(new String()));
        assertFalse(messageId.equals(messageId2));

        messageId2.setValue(messageId.getValue());
        assertTrue(messageId.equals(messageId2));
    }
}
