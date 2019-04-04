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
 * Unit tests for the intersection ID element.
 * 
 * @author ttevendale
 */
public class IntersectionIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        IntersectionID id = new IntersectionID(IntersectionID.MIN);
        assertTrue(IntersectionID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new IntersectionID(IntersectionID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        IntersectionID id = new IntersectionID(IntersectionID.MAX);
        assertTrue(IntersectionID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id = new IntersectionID(IntersectionID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 2151;
        IntersectionID id = new IntersectionID(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testSetValueMin() {

        IntersectionID id = new IntersectionID();
        id.setValue(IntersectionID.MIN);
        assertTrue(IntersectionID.MIN == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(IntersectionID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        IntersectionID id = new IntersectionID();
        id.setValue(IntersectionID.MAX);
        assertTrue(IntersectionID.MAX == id.getValue());

        thrown.expect(IllegalArgumentException.class);
        id.setValue(IntersectionID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        IntersectionID id = new IntersectionID();
        int randomNum = 5187;
        id.setValue(randomNum);
        assertTrue(randomNum == id.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        IntersectionID id = new IntersectionID(IntersectionID.MIN);
        String encodedId = id.encodeUPER();
        assertTrue("0000000000000000".equals(encodedId));

        // test max
        id = new IntersectionID(IntersectionID.MAX);
        encodedId = id.encodeUPER();
        assertTrue("1111111111111111".equals(encodedId));

        int randomNumber = 43614;
        id = new IntersectionID(randomNumber);
        encodedId = id.encodeUPER();
        assertTrue("1010101001011110".equals(encodedId));
    }

    @Test
    public void testDecodeUPER() {

        IntersectionID id = new IntersectionID();

        // test min
        String remainingBits = id.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(IntersectionID.MIN == id.getValue());

        // test max
        remainingBits = id.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(IntersectionID.MAX == id.getValue());

        int expectedNumber = 43614;
        remainingBits = id.decodeUPER("1010101001011110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == id.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        IntersectionID id = new IntersectionID();
        thrown.expect(IllegalArgumentException.class);
        id.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        IntersectionID id = new IntersectionID();
        String remainingBits = id.decodeUPER("000000111101010100010");
        assertTrue("00010".equals(remainingBits));
        assertTrue(id.getValue() == 981);
    }

    @Test
    public void testHashCode() {

        IntersectionID id = new IntersectionID(5);
        IntersectionID id2 = new IntersectionID(10);

        assertFalse(id.hashCode() == id2.hashCode());
        assertTrue(id.hashCode() == id.hashCode());
        assertTrue(id2.hashCode() == id2.hashCode());

        IntersectionID id3 = new IntersectionID(id.getValue());

        assertTrue(id.hashCode() == id3.hashCode());
        assertFalse(id2.hashCode() == id3.hashCode());
    }

    @Test
    public void testEquals() {

        IntersectionID id = new IntersectionID(5);

        assertFalse(id.equals(null));

        assertTrue(id.equals(id));

        IntersectionID id2 = new IntersectionID(id.getValue() + 5);

        assertFalse(id.equals(new String()));
        assertFalse(id.equals(id2));

        id2.setValue(id.getValue());
        assertTrue(id.equals(id2));
    }
}
