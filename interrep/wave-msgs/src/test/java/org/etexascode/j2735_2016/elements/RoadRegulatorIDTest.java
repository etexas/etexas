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
 * Unit tests for the road regulator ID element.
 * 
 * @author ttevendale
 */
public class RoadRegulatorIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RoadRegulatorID id = new RoadRegulatorID(RoadRegulatorID.MIN);
        assertTrue(RoadRegulatorID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new RoadRegulatorID(RoadRegulatorID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        RoadRegulatorID id = new RoadRegulatorID(RoadRegulatorID.MAX);
        assertTrue(RoadRegulatorID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new RoadRegulatorID(RoadRegulatorID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 21201;
        RoadRegulatorID id = new RoadRegulatorID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        RoadRegulatorID id = new RoadRegulatorID();
        id.setValue(RoadRegulatorID.MIN);
        assertTrue(RoadRegulatorID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(RoadRegulatorID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        RoadRegulatorID id = new RoadRegulatorID();
        id.setValue(RoadRegulatorID.MAX);
        assertTrue(RoadRegulatorID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(RoadRegulatorID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        RoadRegulatorID id = new RoadRegulatorID();
        int randomNum = 1154;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        RoadRegulatorID id = new RoadRegulatorID(RoadRegulatorID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("0000000000000000".equals(encodedId));

        // test max
        id = new RoadRegulatorID(RoadRegulatorID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("1111111111111111".equals(encodedId));

        int randomNumber = 10102;
        id = new RoadRegulatorID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("0010011101110110".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        RoadRegulatorID id = new RoadRegulatorID();

        // test min
        String remainingBits = id.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadRegulatorID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(RoadRegulatorID.MAX == id.getValue());

        int expectedNumber = 10102;
        remainingBits = id.decodeUPER("0010011101110110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        RoadRegulatorID id = new RoadRegulatorID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        RoadRegulatorID id = new RoadRegulatorID();
        String remainingBits = id.decodeUPER("0100111011110111111");
        assertTrue("111".equals(remainingBits));
        assertTrue(id.getValue() == 20215);
    }

    @Test
    public void testHashCode() {

        RoadRegulatorID id = new RoadRegulatorID(5);
        RoadRegulatorID id2 = new RoadRegulatorID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        RoadRegulatorID id3 = new RoadRegulatorID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        RoadRegulatorID id = new RoadRegulatorID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        RoadRegulatorID id2 = new RoadRegulatorID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
