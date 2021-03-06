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
 * Unit tests for the lane connection ID element.
 * 
 * @author ttevendale
 */
public class LaneConnectionIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        LaneConnectionID id = new LaneConnectionID(LaneConnectionID.MIN);
        assertTrue(LaneConnectionID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LaneConnectionID(LaneConnectionID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        LaneConnectionID id = new LaneConnectionID(LaneConnectionID.MAX);
        assertTrue(LaneConnectionID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LaneConnectionID(LaneConnectionID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 254;
        LaneConnectionID id = new LaneConnectionID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        LaneConnectionID id = new LaneConnectionID();
        id.setValue(LaneConnectionID.MIN);
        assertTrue(LaneConnectionID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LaneConnectionID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        LaneConnectionID id = new LaneConnectionID();
        id.setValue(LaneConnectionID.MAX);
        assertTrue(LaneConnectionID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LaneConnectionID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        LaneConnectionID id = new LaneConnectionID();
        int randomNum = 14;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        LaneConnectionID id = new LaneConnectionID(LaneConnectionID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("00000000".equals(encodedId));

        // test max
        id = new LaneConnectionID(LaneConnectionID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("11111111".equals(encodedId));

        int randomNumber = 86;
        id = new LaneConnectionID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("01010110".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        LaneConnectionID id = new LaneConnectionID();

        // test min
        String remainingBits = id.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneConnectionID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(LaneConnectionID.MAX == id.getValue());

        int expectedNumber = 86;
        remainingBits = id.decodeUPER("01010110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        LaneConnectionID id = new LaneConnectionID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LaneConnectionID id = new LaneConnectionID();
        String remainingBits = id.decodeUPER("001110111010");
        assertTrue("1010".equals(remainingBits));
        assertTrue(id.getValue() == 59);
    }

    @Test
    public void testHashCode() {

        LaneConnectionID id = new LaneConnectionID(5);
        LaneConnectionID id2 = new LaneConnectionID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        LaneConnectionID id3 = new LaneConnectionID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        LaneConnectionID id = new LaneConnectionID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        LaneConnectionID id2 = new LaneConnectionID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
