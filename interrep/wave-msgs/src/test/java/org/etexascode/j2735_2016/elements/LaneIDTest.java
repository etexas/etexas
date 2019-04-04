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
 * Unit tests for the lane ID element.
 * 
 * @author ttevendale
 */
public class LaneIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        LaneID id = new LaneID(LaneID.MIN);
        assertTrue(LaneID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LaneID(LaneID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        LaneID id = new LaneID(LaneID.MAX);
        assertTrue(LaneID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LaneID(LaneID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 254;
        LaneID id = new LaneID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        LaneID id = new LaneID();
        id.setValue(LaneID.MIN);
        assertTrue(LaneID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LaneID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        LaneID id = new LaneID();
        id.setValue(LaneID.MAX);
        assertTrue(LaneID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LaneID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        LaneID id = new LaneID();
        int randomNum = 28;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        LaneID id = new LaneID(LaneID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("00000000".equals(encodedId));

        // test max
        id = new LaneID(LaneID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("11111111".equals(encodedId));

        int randomNumber = 106;
        id = new LaneID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("01101010".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        LaneID id = new LaneID();

        // test min
        String remainingBits = id.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneID.MAX == id.getValue());

        int expectedNumber = 106;
        remainingBits = id.decodeUPER("01101010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneID id = new LaneID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneID id = new LaneID();
        String remainingBits = id.decodeUPER("001110111010");
        assertTrue("1010".equals(remainingBits));
        assertTrue(id.getValue() == 59);
    }

    @Test
    public void testHashCode() {

        LaneID id = new LaneID(5);
        LaneID id2 = new LaneID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        LaneID id3 = new LaneID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneID id = new LaneID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        LaneID id2 = new LaneID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
