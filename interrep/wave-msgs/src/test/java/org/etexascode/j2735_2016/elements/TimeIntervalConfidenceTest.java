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
 * Unit tests for the time interval confidence element.
 * 
 * @author ttevendale
 */
public class TimeIntervalConfidenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MIN);
        assertTrue(TimeIntervalConfidence.MIN == confidence.getValue());

        thrown.expect(IllegalArgumentException.class);
        confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MAX);
        assertTrue(TimeIntervalConfidence.MAX == confidence.getValue());

        thrown.expect(IllegalArgumentException.class);
        confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 10;
        TimeIntervalConfidence confidence = new TimeIntervalConfidence(randomNum);
        assertTrue(randomNum == confidence.getValue());
    }

    @Test
    public void testSetValueMin() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();
        confidence.setValue(TimeIntervalConfidence.MIN);
        assertTrue(TimeIntervalConfidence.MIN == confidence.getValue());

        thrown.expect(IllegalArgumentException.class);
        confidence.setValue(TimeIntervalConfidence.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();
        confidence.setValue(TimeIntervalConfidence.MAX);
        assertTrue(TimeIntervalConfidence.MAX == confidence.getValue());

        thrown.expect(IllegalArgumentException.class);
        confidence.setValue(TimeIntervalConfidence.MAX + 1);
    }

    @Test
    public void testSetValue() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();
        int randomNum = 5;
        confidence.setValue(randomNum);
        assertTrue(randomNum == confidence.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        TimeIntervalConfidence confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MIN);
        String encodedConfidence = confidence.encodeUPER();
        assertTrue("0000".equals(encodedConfidence));

        // test max
        confidence = new TimeIntervalConfidence(TimeIntervalConfidence.MAX);
        encodedConfidence = confidence.encodeUPER();
        assertTrue("1111".equals(encodedConfidence));

        int randomNumber = 9;
        confidence = new TimeIntervalConfidence(randomNumber);
        encodedConfidence = confidence.encodeUPER();
        assertTrue("1001".equals(encodedConfidence));
    }

    @Test
    public void testDecodeUPER() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();

        // test min
        String remainingBits = confidence.decodeUPER("0000");
        assertTrue("".equals(remainingBits));
        assertTrue(TimeIntervalConfidence.MIN == confidence.getValue());

        // test max
        remainingBits = confidence.decodeUPER("1111");
        assertTrue("".equals(remainingBits));
        assertTrue(TimeIntervalConfidence.MAX == confidence.getValue());

        int expectedNumber = 9;
        remainingBits = confidence.decodeUPER("1001");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == confidence.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();
        thrown.expect(IllegalArgumentException.class);
        confidence.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence();
        String remainingBits = confidence.decodeUPER("0000001010101");
        assertTrue("001010101".equals(remainingBits));
        assertTrue(confidence.getValue() == 0);
    }

    @Test
    public void testHashCode() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence(5);
        TimeIntervalConfidence confidence2 = new TimeIntervalConfidence(10);

        assertFalse(confidence.hashCode() == confidence2.hashCode());
        assertTrue(confidence.hashCode() == confidence.hashCode());
        assertTrue(confidence2.hashCode() == confidence2.hashCode());

        TimeIntervalConfidence confidence3 = new TimeIntervalConfidence(confidence.getValue());

        assertTrue(confidence.hashCode() == confidence3.hashCode());
        assertFalse(confidence2.hashCode() == confidence3.hashCode());
    }

    @Test
    public void testEquals() {

        TimeIntervalConfidence confidence = new TimeIntervalConfidence(5);

        assertFalse(confidence.equals(null));

        assertTrue(confidence.equals(confidence));

        TimeIntervalConfidence confidence2 = new TimeIntervalConfidence(confidence.getValue() + 5);

        assertFalse(confidence.equals(new String()));
        assertFalse(confidence.equals(confidence2));

        confidence2.setValue(confidence.getValue());
        assertTrue(confidence.equals(confidence2));
    }
}
