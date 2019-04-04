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
 * Unit tests for the speed advice element.
 * 
 * @author ttevendale
 */
public class SpeedAdviceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SpeedAdvice speedAdvice = new SpeedAdvice(SpeedAdvice.MIN);
        assertTrue(SpeedAdvice.MIN == speedAdvice.getValue());

        thrown.expect(IllegalArgumentException.class);
        speedAdvice = new SpeedAdvice(SpeedAdvice.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        SpeedAdvice speedAdvice = new SpeedAdvice(SpeedAdvice.MAX);
        assertTrue(SpeedAdvice.MAX == speedAdvice.getValue());

        thrown.expect(IllegalArgumentException.class);
        speedAdvice = new SpeedAdvice(SpeedAdvice.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 405;
        SpeedAdvice speedAdvice = new SpeedAdvice(randomNum);
        assertTrue(randomNum == speedAdvice.getValue());
    }

    @Test
    public void testSetValueMin() {

        SpeedAdvice speedAdvice = new SpeedAdvice();
        speedAdvice.setValue(SpeedAdvice.MIN);
        assertTrue(SpeedAdvice.MIN == speedAdvice.getValue());

        thrown.expect(IllegalArgumentException.class);
        speedAdvice.setValue(SpeedAdvice.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        SpeedAdvice speedAdvice = new SpeedAdvice();
        speedAdvice.setValue(SpeedAdvice.MAX);
        assertTrue(SpeedAdvice.MAX == speedAdvice.getValue());

        thrown.expect(IllegalArgumentException.class);
        speedAdvice.setValue(SpeedAdvice.MAX + 1);
    }

    @Test
    public void testSetValue() {

        SpeedAdvice speedAdvice = new SpeedAdvice();
        int randomNum = 313;
        speedAdvice.setValue(randomNum);
        assertTrue(randomNum == speedAdvice.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SpeedAdvice speedAdvice = new SpeedAdvice(SpeedAdvice.MIN);
        String encodedSpeedAdvice = speedAdvice.encodeUPER();
        assertTrue("000000000".equals(encodedSpeedAdvice));

        // test max
        speedAdvice = new SpeedAdvice(SpeedAdvice.MAX);
        encodedSpeedAdvice = speedAdvice.encodeUPER();
        assertTrue("111110100".equals(encodedSpeedAdvice));

        int randomNumber = 250;
        speedAdvice = new SpeedAdvice(randomNumber);
        encodedSpeedAdvice = speedAdvice.encodeUPER();
        assertTrue("011111010".equals(encodedSpeedAdvice));
    }

    @Test
    public void testDecodeUPER() {

        SpeedAdvice speedAdvice = new SpeedAdvice();

        // test min
        String remainingBits = speedAdvice.decodeUPER("000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedAdvice.MIN == speedAdvice.getValue());

        // test max
        remainingBits = speedAdvice.decodeUPER("111110100");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedAdvice.MAX == speedAdvice.getValue());

        int expectedNumber = 250;
        remainingBits = speedAdvice.decodeUPER("011111010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == speedAdvice.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        SpeedAdvice speedAdvice = new SpeedAdvice();
        thrown.expect(IllegalArgumentException.class);
        speedAdvice.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SpeedAdvice speedAdvice = new SpeedAdvice();
        String remainingBits = speedAdvice.decodeUPER("001101111010");
        assertTrue("010".equals(remainingBits));
        assertTrue(speedAdvice.getValue() == 111);
    }

    @Test
    public void testHashCode() {

        SpeedAdvice speedAdvice = new SpeedAdvice(5);
        SpeedAdvice speedAdvice2 = new SpeedAdvice(10);

        assertFalse(speedAdvice.hashCode() == speedAdvice2.hashCode());
        assertTrue(speedAdvice.hashCode() == speedAdvice.hashCode());
        assertTrue(speedAdvice2.hashCode() == speedAdvice2.hashCode());

        SpeedAdvice speedAdvice3 = new SpeedAdvice(speedAdvice.getValue());

        assertTrue(speedAdvice.hashCode() == speedAdvice3.hashCode());
        assertFalse(speedAdvice2.hashCode() == speedAdvice3.hashCode());
    }

    @Test
    public void testEquals() {

        SpeedAdvice speedAdvice = new SpeedAdvice(5);

        assertFalse(speedAdvice.equals(null));

        assertTrue(speedAdvice.equals(speedAdvice));

        SpeedAdvice speedAdvice2 = new SpeedAdvice(speedAdvice.getValue() + 5);

        assertFalse(speedAdvice.equals(new String()));
        assertFalse(speedAdvice.equals(speedAdvice2));

        speedAdvice2.setValue(speedAdvice.getValue());
        assertTrue(speedAdvice.equals(speedAdvice2));
    }
}
