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
 * Unit tests for the heading element.
 * 
 * @author ttevendale
 */
public class HeadingTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        Heading heading = new Heading(Heading.MIN);
        assertTrue(Heading.MIN == heading.getValue());

        thrown.expect(IllegalArgumentException.class);
        heading = new Heading(Heading.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        Heading heading = new Heading(Heading.MAX);
        assertTrue(Heading.MAX == heading.getValue());

        thrown.expect(IllegalArgumentException.class);
        heading = new Heading(Heading.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 518;
        Heading heading = new Heading(randomNum);
        assertTrue(randomNum == heading.getValue());
    }

    @Test
    public void testSetValueMin() {

        Heading heading = new Heading();
        heading.setValue(Heading.MIN);
        assertTrue(Heading.MIN == heading.getValue());

        thrown.expect(IllegalArgumentException.class);
        heading.setValue(Heading.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        Heading heading = new Heading();
        heading.setValue(Heading.MAX);
        assertTrue(Heading.MAX == heading.getValue());

        thrown.expect(IllegalArgumentException.class);
        heading.setValue(Heading.MAX + 1);
    }

    @Test
    public void testSetValue() {

        Heading heading = new Heading();
        int randomNum = 1804;
        heading.setValue(randomNum);
        assertTrue(randomNum == heading.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        Heading heading = new Heading(Heading.MIN);
        String encodedHeading = heading.encodeUPER();
        assertTrue("000000000000000".equals(encodedHeading));

        // test max
        heading = new Heading(Heading.MAX);
        encodedHeading = heading.encodeUPER();
        assertTrue("111000010000000".equals(encodedHeading));

        int randomNumber = 8784;
        heading = new Heading(randomNumber);
        encodedHeading = heading.encodeUPER();
        assertTrue("010001001010000".equals(encodedHeading));
    }

    @Test
    public void testDecodeUPER() {

        Heading heading = new Heading();

        // test min
        String remainingBits = heading.decodeUPER("000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Heading.MIN == heading.getValue());

        // test max
        remainingBits = heading.decodeUPER("111000010000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Heading.MAX == heading.getValue());

        int expectedNumber = 3857;
        remainingBits = heading.decodeUPER("000111100010001");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == heading.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        Heading heading = new Heading();
        thrown.expect(IllegalArgumentException.class);
        heading.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        Heading heading = new Heading();
        String remainingBits = heading.decodeUPER("0000000000001001");
        assertTrue("1".equals(remainingBits));
        assertTrue(heading.getValue() == 4);
    }

    @Test
    public void testHashCode() {

        Heading heading = new Heading(141);
        Heading heading2 = new Heading(20000);

        assertFalse(heading.hashCode() == heading2.hashCode());
        assertTrue(heading.hashCode() == heading.hashCode());
        assertTrue(heading2.hashCode() == heading2.hashCode());

        Heading heading3 = new Heading(heading.getValue());

        assertTrue(heading.hashCode() == heading3.hashCode());
        assertFalse(heading2.hashCode() == heading3.hashCode());
    }

    @Test
    public void testEquals() {

        Heading heading = new Heading(800);

        assertFalse(heading.equals(null));

        assertTrue(heading.equals(heading));

        Heading heading2 = new Heading(126);

        assertFalse(heading.equals(new String()));
        assertFalse(heading.equals(heading2));

        heading2.setValue(heading.getValue());
        assertTrue(heading.equals(heading2));
    }
}
