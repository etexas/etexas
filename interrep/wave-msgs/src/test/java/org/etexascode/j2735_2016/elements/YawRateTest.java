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
 * Unit tests for the yaw rate element.
 * 
 * @author ttevendale
 */
public class YawRateTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        YawRate yawRate = new YawRate(YawRate.MIN);
        assertTrue(YawRate.MIN == yawRate.getValue());

        thrown.expect(IllegalArgumentException.class);
        yawRate = new YawRate(YawRate.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        YawRate yawRate = new YawRate(YawRate.MAX);
        assertTrue(YawRate.MAX == yawRate.getValue());

        thrown.expect(IllegalArgumentException.class);
        yawRate = new YawRate(YawRate.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 15151;
        YawRate yawRate = new YawRate(randomNum);
        assertTrue(randomNum == yawRate.getValue());
    }

    @Test
    public void testSetValueMin() {

        YawRate yawRate = new YawRate();
        yawRate.setValue(YawRate.MIN);
        assertTrue(YawRate.MIN == yawRate.getValue());

        thrown.expect(IllegalArgumentException.class);
        yawRate.setValue(YawRate.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        YawRate yawRate = new YawRate();
        yawRate.setValue(YawRate.MAX);
        assertTrue(YawRate.MAX == yawRate.getValue());

        thrown.expect(IllegalArgumentException.class);
        yawRate.setValue(YawRate.MAX + 1);
    }

    @Test
    public void testSetValue() {

        YawRate yawRate = new YawRate();
        int randomNum = -27841;
        yawRate.setValue(randomNum);
        assertTrue(randomNum == yawRate.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        YawRate yawRate = new YawRate(YawRate.MIN);
        String encodedYawRate = yawRate.encodeUPER();
        assertTrue("0000000000000000".equals(encodedYawRate));

        // test max
        yawRate = new YawRate(YawRate.MAX);
        encodedYawRate = yawRate.encodeUPER();
        assertTrue("1111111111111110".equals(encodedYawRate));

        int randomNumber = 0;
        yawRate = new YawRate(randomNumber);
        encodedYawRate = yawRate.encodeUPER();
        assertTrue("0111111111111111".equals(encodedYawRate));
    }

    @Test
    public void testDecodeUPER() {

        YawRate yawRate = new YawRate();

        // test min
        String remainingBits = yawRate.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(YawRate.MIN == yawRate.getValue());

        // test max
        remainingBits = yawRate.decodeUPER("1111111111111110");
        assertTrue("".equals(remainingBits));
        assertTrue(YawRate.MAX == yawRate.getValue());

        int expectedNumber = -30000;
        remainingBits = yawRate.decodeUPER("0000101011001111");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == yawRate.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        YawRate yawRate = new YawRate();
        thrown.expect(IllegalArgumentException.class);
        yawRate.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        YawRate yawRate = new YawRate();
        String remainingBits = yawRate.decodeUPER("1000000001100011001");
        assertTrue("001".equals(remainingBits));
        assertTrue(yawRate.getValue() == 100);
    }

    @Test
    public void testHashCode() {

        YawRate yawRate = new YawRate(-30000);
        YawRate yawRate2 = new YawRate(-30001);

        assertFalse(yawRate.hashCode() == yawRate2.hashCode());
        assertTrue(yawRate.hashCode() == yawRate.hashCode());
        assertTrue(yawRate2.hashCode() == yawRate2.hashCode());

        YawRate yawRate3 = new YawRate(yawRate.getValue());

        assertTrue(yawRate.hashCode() == yawRate3.hashCode());
        assertFalse(yawRate2.hashCode() == yawRate3.hashCode());
    }

    @Test
    public void testEquals() {

        YawRate yawRate = new YawRate(100);

        assertFalse(yawRate.equals(null));

        assertTrue(yawRate.equals(yawRate));

        YawRate yawRate2 = new YawRate(99);

        assertFalse(yawRate.equals(new String()));
        assertFalse(yawRate.equals(yawRate2));

        yawRate2.setValue(yawRate.getValue());
        assertTrue(yawRate.equals(yawRate2));
    }
}
