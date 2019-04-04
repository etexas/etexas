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
 * Unit tests for the layer ID element.
 * 
 * @author ttevendale
 */
public class LayerIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        LayerID id = new LayerID(LayerID.MIN);
        assertTrue(LayerID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LayerID(LayerID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        LayerID id = new LayerID(LayerID.MAX);
        assertTrue(LayerID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new LayerID(LayerID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 12;
        LayerID id = new LayerID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        LayerID id = new LayerID();
        id.setValue(LayerID.MIN);
        assertTrue(LayerID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LayerID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        LayerID id = new LayerID();
        id.setValue(LayerID.MAX);
        assertTrue(LayerID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(LayerID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        LayerID id = new LayerID();
        int randomNum = 50;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        LayerID id = new LayerID(LayerID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("0000000".equals(encodedId));

        // test max
        id = new LayerID(LayerID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("1100100".equals(encodedId));

        int randomNumber = 13;
        id = new LayerID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("0001101".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        LayerID id = new LayerID();

        // test min
        String remainingBits = id.decodeUPER("0000000");
        assertTrue("".equals(remainingBits));
        assertTrue(LayerID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("1100100");
        assertTrue("".equals(remainingBits));
        assertTrue(LayerID.MAX == id.getValue());

        int expectedNumber = 69;
        remainingBits = id.decodeUPER("1000101");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        LayerID id = new LayerID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LayerID id = new LayerID();
        String remainingBits = id.decodeUPER("1011011001010");
        assertTrue("001010".equals(remainingBits));
        assertTrue(id.getValue() == 91);
    }

    @Test
    public void testHashCode() {

        LayerID id = new LayerID(5);
        LayerID id2 = new LayerID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        LayerID id3 = new LayerID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        LayerID id = new LayerID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        LayerID id2 = new LayerID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
