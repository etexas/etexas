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
 * Unit tests for the minute of the year element.
 * 
 * @author ttevendale
 */
public class MinuteOfTheYearTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        MinuteOfTheYear moy = new MinuteOfTheYear(MinuteOfTheYear.MIN);
        assertTrue(MinuteOfTheYear.MIN == moy.getValue());

        thrown.expect(IllegalArgumentException.class);
        moy = new MinuteOfTheYear(MinuteOfTheYear.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        MinuteOfTheYear moy = new MinuteOfTheYear(MinuteOfTheYear.MAX);
        assertTrue(MinuteOfTheYear.MAX == moy.getValue());

        thrown.expect(IllegalArgumentException.class);
        moy = new MinuteOfTheYear(MinuteOfTheYear.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 23215;
        MinuteOfTheYear moy = new MinuteOfTheYear(randomNum);
        assertTrue(randomNum == moy.getValue());
    }

    @Test
    public void testSetValueMin() {

        MinuteOfTheYear moy = new MinuteOfTheYear();
        moy.setValue(MinuteOfTheYear.MIN);
        assertTrue(MinuteOfTheYear.MIN == moy.getValue());

        thrown.expect(IllegalArgumentException.class);
        moy.setValue(MinuteOfTheYear.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        MinuteOfTheYear moy = new MinuteOfTheYear();
        moy.setValue(MinuteOfTheYear.MAX);
        assertTrue(MinuteOfTheYear.MAX == moy.getValue());

        thrown.expect(IllegalArgumentException.class);
        moy.setValue(MinuteOfTheYear.MAX + 1);
    }

    @Test
    public void testSetValue() {

        MinuteOfTheYear moy = new MinuteOfTheYear();
        int randomNum = 15115;
        moy.setValue(randomNum);
        assertTrue(randomNum == moy.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        MinuteOfTheYear moy = new MinuteOfTheYear(MinuteOfTheYear.MIN);
        String encodedMoy = moy.encodeUPER();
        assertTrue("00000000000000000000".equals(encodedMoy));

        // test max
        moy = new MinuteOfTheYear(MinuteOfTheYear.MAX);
        encodedMoy = moy.encodeUPER();
        assertTrue("10000000101011000000".equals(encodedMoy));

        int randomNumber = 5156;
        moy = new MinuteOfTheYear(randomNumber);
        encodedMoy = moy.encodeUPER();
        assertTrue("00000001010000100100".equals(encodedMoy));
    }

    @Test
    public void testDecodeUPER() {

        MinuteOfTheYear moy = new MinuteOfTheYear();

        // test min
        String remainingBits = moy.decodeUPER("00000000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(MinuteOfTheYear.MIN == moy.getValue());

        // test max
        remainingBits = moy.decodeUPER("10000000101011000000");
        assertTrue("".equals(remainingBits));
        assertTrue(MinuteOfTheYear.MAX == moy.getValue());

        int expectedNumber = 15148;
        remainingBits = moy.decodeUPER("00000011101100101100");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == moy.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        MinuteOfTheYear moy = new MinuteOfTheYear();
        thrown.expect(IllegalArgumentException.class);
        moy.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        MinuteOfTheYear moy = new MinuteOfTheYear();
        String remainingBits = moy.decodeUPER("000000000000000000011");
        assertTrue("1".equals(remainingBits));
        assertTrue(moy.getValue() == 1);
    }

    @Test
    public void testHashCode() {

        MinuteOfTheYear moy = new MinuteOfTheYear(0);
        MinuteOfTheYear moy2 = new MinuteOfTheYear(126);

        assertFalse(moy.hashCode() == moy2.hashCode());
        assertTrue(moy.hashCode() == moy.hashCode());
        assertTrue(moy2.hashCode() == moy2.hashCode());

        MinuteOfTheYear moy3 = new MinuteOfTheYear(moy.getValue());

        assertTrue(moy.hashCode() == moy3.hashCode());
        assertFalse(moy2.hashCode() == moy3.hashCode());
    }

    @Test
    public void testEquals() {

        MinuteOfTheYear moy = new MinuteOfTheYear(578);

        assertFalse(moy.equals(null));

        assertTrue(moy.equals(moy));

        MinuteOfTheYear moy2 = new MinuteOfTheYear(60999);

        assertFalse(moy.equals(new String()));
        assertFalse(moy.equals(moy2));

        moy2.setValue(moy.getValue());
        assertTrue(moy.equals(moy2));
    }
}
