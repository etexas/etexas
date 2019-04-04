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
 * Unit tests for the second element.
 * 
 * @author ttevendale
 */
public class DSecondTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        DSecond second = new DSecond(DSecond.MIN);
        assertTrue(DSecond.MIN == second.getValue());

        thrown.expect(IllegalArgumentException.class);
        second = new DSecond(DSecond.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        DSecond second = new DSecond(DSecond.MAX);
        assertTrue(DSecond.MAX == second.getValue());

        thrown.expect(IllegalArgumentException.class);
        second = new DSecond(DSecond.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 18756;
        DSecond second = new DSecond(randomNum);
        assertTrue(randomNum == second.getValue());
    }

    @Test
    public void testSetValueMin() {

        DSecond second = new DSecond();
        second.setValue(DSecond.MIN);
        assertTrue(DSecond.MIN == second.getValue());

        thrown.expect(IllegalArgumentException.class);
        second.setValue(DSecond.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        DSecond second = new DSecond();
        second.setValue(DSecond.MAX);
        assertTrue(DSecond.MAX == second.getValue());

        thrown.expect(IllegalArgumentException.class);
        second.setValue(DSecond.MAX + 1);
    }

    @Test
    public void testSetValue() {

        DSecond second = new DSecond();
        int randomNum = 184;
        second.setValue(randomNum);
        assertTrue(randomNum == second.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        DSecond second = new DSecond(DSecond.MIN);
        String encodedSecond = second.encodeUPER();
        assertTrue("0000000000000000".equals(encodedSecond));

        // test max
        second = new DSecond(DSecond.MAX);
        encodedSecond = second.encodeUPER();
        assertTrue("1111111111111111".equals(encodedSecond));

        int randomNumber = 15644;
        second = new DSecond(randomNumber);
        encodedSecond = second.encodeUPER();
        assertTrue("0011110100011100".equals(encodedSecond));
    }

    @Test
    public void testDecodeUPER() {

        DSecond second = new DSecond();

        // test min
        String remainingBits = second.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(DSecond.MIN == second.getValue());

        // test max
        remainingBits = second.decodeUPER("1111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(DSecond.MAX == second.getValue());

        int expectedNumber = 54;
        remainingBits = second.decodeUPER("0000000000110110");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == second.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        DSecond second = new DSecond();
        thrown.expect(IllegalArgumentException.class);
        second.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        DSecond second = new DSecond();
        String remainingBits = second.decodeUPER("00000000000011000");
        assertTrue("0".equals(remainingBits));
        assertTrue(second.getValue() == 12);
    }

    @Test
    public void testHashCode() {

        DSecond second = new DSecond(841);
        DSecond second2 = new DSecond(500);

        assertFalse(second.hashCode() == second2.hashCode());
        assertTrue(second.hashCode() == second.hashCode());
        assertTrue(second2.hashCode() == second2.hashCode());

        DSecond second3 = new DSecond(second.getValue());

        assertTrue(second.hashCode() == second3.hashCode());
        assertFalse(second2.hashCode() == second3.hashCode());
    }

    @Test
    public void testEquals() {

        DSecond second = new DSecond(10);

        assertFalse(second.equals(null));

        assertTrue(second.equals(second));

        DSecond second2 = new DSecond(20);

        assertFalse(second.equals(new String()));
        assertFalse(second.equals(second2));

        second2.setValue(second.getValue());
        assertTrue(second.equals(second2));
    }
}
