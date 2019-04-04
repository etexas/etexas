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
 * Unit tests for the signal group ID element.
 * 
 * @author ttevendale
 */
public class SignalGroupIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SignalGroupID id = new SignalGroupID(SignalGroupID.MIN);
        assertTrue(SignalGroupID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new SignalGroupID(SignalGroupID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SignalGroupID id = new SignalGroupID(SignalGroupID.MAX);
        assertTrue(SignalGroupID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new SignalGroupID(SignalGroupID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 154;
        SignalGroupID id = new SignalGroupID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        SignalGroupID id = new SignalGroupID();
        id.setValue(SignalGroupID.MIN);
        assertTrue(SignalGroupID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(SignalGroupID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SignalGroupID id = new SignalGroupID();
        id.setValue(SignalGroupID.MAX);
        assertTrue(SignalGroupID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(SignalGroupID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SignalGroupID id = new SignalGroupID();
        int randomNum = 208;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SignalGroupID id = new SignalGroupID(SignalGroupID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("00000000".equals(encodedId));

        // test max
        id = new SignalGroupID(SignalGroupID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("11111111".equals(encodedId));

        int randomNumber = 200;
        id = new SignalGroupID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("11001000".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        SignalGroupID id = new SignalGroupID();

        // test min
        String remainingBits = id.decodeUPER("00000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SignalGroupID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("11111111");
        assertTrue("".equals(remainingBits));
        assertTrue(SignalGroupID.MAX == id.getValue());

        int expectedNumber = 200;
        remainingBits = id.decodeUPER("11001000");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SignalGroupID id = new SignalGroupID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SignalGroupID id = new SignalGroupID();
        String remainingBits = id.decodeUPER("01010001000");
        assertTrue("000".equals(remainingBits));
        assertTrue(id.getValue() == 81);
    }

    @Test
    public void testHashCode() {

        SignalGroupID id = new SignalGroupID(5);
        SignalGroupID id2 = new SignalGroupID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        SignalGroupID id3 = new SignalGroupID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        SignalGroupID id = new SignalGroupID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        SignalGroupID id2 = new SignalGroupID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
