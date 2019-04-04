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
 * Unit tests for the time mark element.
 * 
 * @author ttevendale
 */
public class TimeMarkTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        TimeMark time = new TimeMark(TimeMark.MIN);
        assertTrue(TimeMark.MIN == time.getValue());

        thrown.expect(IllegalArgumentException.class);
        time = new TimeMark(TimeMark.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        TimeMark time = new TimeMark(TimeMark.MAX);
        assertTrue(TimeMark.MAX == time.getValue());

        thrown.expect(IllegalArgumentException.class);
        time = new TimeMark(TimeMark.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 12123;
        TimeMark time = new TimeMark(randomNum);
        assertTrue(randomNum == time.getValue());
    }

    @Test
    public void testSetValueMin() {

        TimeMark time = new TimeMark();
        time.setValue(TimeMark.MIN);
        assertTrue(TimeMark.MIN == time.getValue());

        thrown.expect(IllegalArgumentException.class);
        time.setValue(TimeMark.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        TimeMark time = new TimeMark();
        time.setValue(TimeMark.MAX);
        assertTrue(TimeMark.MAX == time.getValue());

        thrown.expect(IllegalArgumentException.class);
        time.setValue(TimeMark.MAX + 1);
    }

    @Test
    public void testSetValue() {

        TimeMark time = new TimeMark();
        int randomNum = 5789;
        time.setValue(randomNum);
        assertTrue(randomNum == time.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        TimeMark time = new TimeMark(TimeMark.MIN);
        String encodedTime = time.encodeUPER();
        assertTrue("0000000000000000".equals(encodedTime));

        // test max
        time = new TimeMark(TimeMark.MAX);
        encodedTime = time.encodeUPER();
        assertTrue("1000110010100001".equals(encodedTime));

        int randomNumber = 20154;
        time = new TimeMark(randomNumber);
        encodedTime = time.encodeUPER();
        assertTrue("0100111010111010".equals(encodedTime));
    }

    @Test
    public void testDecodeUPER() {

        TimeMark time = new TimeMark();

        // test min
        String remainingBits = time.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(TimeMark.MIN == time.getValue());

        // test max
        remainingBits = time.decodeUPER("1000110010100001");
        assertTrue("".equals(remainingBits));
        assertTrue(TimeMark.MAX == time.getValue());

        int expectedNumber = 20154;
        remainingBits = time.decodeUPER("0100111010111010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == time.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        TimeMark time = new TimeMark();
        thrown.expect(IllegalArgumentException.class);
        time.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        TimeMark time = new TimeMark();
        String remainingBits = time.decodeUPER("01100010010011001100");
        assertTrue("1100".equals(remainingBits));
        assertTrue(time.getValue() == 25164);
    }

    @Test
    public void testHashCode() {

        TimeMark time = new TimeMark(5);
        TimeMark time2 = new TimeMark(10);

        assertFalse(time.hashCode() == time2.hashCode());
        assertTrue(time.hashCode() == time.hashCode());
        assertTrue(time2.hashCode() == time2.hashCode());

        TimeMark time3 = new TimeMark(time.getValue());

        assertTrue(time.hashCode() == time3.hashCode());
        assertFalse(time2.hashCode() == time3.hashCode());
    }

    @Test
    public void testEquals() {

        TimeMark time = new TimeMark(5);

        assertFalse(time.equals(null));

        assertTrue(time.equals(time));

        TimeMark time2 = new TimeMark(time.getValue() + 5);

        assertFalse(time.equals(new String()));
        assertFalse(time.equals(time2));

        time2.setValue(time.getValue());
        assertTrue(time.equals(time2));
    }
}
