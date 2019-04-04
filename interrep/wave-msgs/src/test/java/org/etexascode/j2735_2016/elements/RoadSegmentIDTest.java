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
 * Unit tests for the road segment ID element.
 * 
 * @author ttevendale
 */
public class RoadSegmentIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RoadSegmentID id = new RoadSegmentID(RoadSegmentID.MIN);
        assertTrue(RoadSegmentID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new RoadSegmentID(RoadSegmentID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        RoadSegmentID id = new RoadSegmentID(RoadSegmentID.MAX);
        assertTrue(RoadSegmentID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new RoadSegmentID(RoadSegmentID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 5151;
        RoadSegmentID id = new RoadSegmentID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        RoadSegmentID id = new RoadSegmentID();
        id.setValue(RoadSegmentID.MIN);
        assertTrue(RoadSegmentID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(RoadSegmentID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        RoadSegmentID id = new RoadSegmentID();
        id.setValue(RoadSegmentID.MAX);
        assertTrue(RoadSegmentID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(RoadSegmentID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        RoadSegmentID id = new RoadSegmentID();
        int randomNum = 50000;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        RoadSegmentID id = new RoadSegmentID(RoadSegmentID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("0000000000000000".equals(encodedId));

        // test max
        id = new RoadSegmentID(RoadSegmentID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("1111111111111111".equals(encodedId));

        int randomNumber = 5699;
        id = new RoadSegmentID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("0001011001000011".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        RoadSegmentID id = new RoadSegmentID();

        // test min
        String remainingBits = id.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadSegmentID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadSegmentID.MAX == id.getValue());

        int expectedNumber = 50516;
        remainingBits = id.decodeUPER("1100010101010100");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        RoadSegmentID id = new RoadSegmentID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        RoadSegmentID id = new RoadSegmentID();
        String remainingBits = id.decodeUPER("10101011010101101");
        assertTrue("1".equals(remainingBits));
        assertTrue(id.getValue() == 43862);
    }

    @Test
    public void testHashCode() {

        RoadSegmentID id = new RoadSegmentID(5);
        RoadSegmentID id2 = new RoadSegmentID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        RoadSegmentID id3 = new RoadSegmentID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        RoadSegmentID id = new RoadSegmentID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        RoadSegmentID id2 = new RoadSegmentID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
