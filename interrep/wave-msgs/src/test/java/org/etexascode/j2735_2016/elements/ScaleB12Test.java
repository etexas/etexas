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
 * Unit tests for the scale b12 element.
 * 
 * @author ttevendale
 */
public class ScaleB12Test {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        ScaleB12 scale = new ScaleB12(ScaleB12.MIN);
        assertTrue(ScaleB12.MIN == scale.getValue());

        thrown.expect(IllegalArgumentException.class);
        scale = new ScaleB12(ScaleB12.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        ScaleB12 scale = new ScaleB12(ScaleB12.MAX);
        assertTrue(ScaleB12.MAX == scale.getValue());

        thrown.expect(IllegalArgumentException.class);
        scale = new ScaleB12(ScaleB12.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = -1555;
        ScaleB12 scale = new ScaleB12(randomNum);
        assertTrue(randomNum == scale.getValue());
    }

    @Test
    public void testSetValueMin() {

        ScaleB12 scale = new ScaleB12();
        scale.setValue(ScaleB12.MIN);
        assertTrue(ScaleB12.MIN == scale.getValue());

        thrown.expect(IllegalArgumentException.class);
        scale.setValue(ScaleB12.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        ScaleB12 scale = new ScaleB12();
        scale.setValue(ScaleB12.MAX);
        assertTrue(ScaleB12.MAX == scale.getValue());

        thrown.expect(IllegalArgumentException.class);
        scale.setValue(ScaleB12.MAX + 1);
    }

    @Test
    public void testSetValue() {

        ScaleB12 scale = new ScaleB12();
        int randomNum = 2001;
        scale.setValue(randomNum);
        assertTrue(randomNum == scale.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        ScaleB12 scale = new ScaleB12(ScaleB12.MIN);
        String encodedScale = scale.encodeUPER();
        assertTrue("000000000000".equals(encodedScale));

        // test max
        scale = new ScaleB12(ScaleB12.MAX);
        encodedScale = scale.encodeUPER();
        assertTrue("111111111111".equals(encodedScale));

        int randomNumber = 0;
        scale = new ScaleB12(randomNumber);
        encodedScale = scale.encodeUPER();
        assertTrue("100000000000".equals(encodedScale));
    }

    @Test
    public void testDecodeUPER() {

        ScaleB12 scale = new ScaleB12();

        // test min
        String remainingBits = scale.decodeUPER("000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue(ScaleB12.MIN == scale.getValue());

        // test max
        remainingBits = scale.decodeUPER("111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(ScaleB12.MAX == scale.getValue());

        int expectedNumber = -1061;
        remainingBits = scale.decodeUPER("001111011011");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == scale.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        ScaleB12 scale = new ScaleB12();
        thrown.expect(IllegalArgumentException.class);
        scale.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        ScaleB12 scale = new ScaleB12();
        String remainingBits = scale.decodeUPER("00011101110101");
        assertTrue("01".equals(remainingBits));
        assertTrue(scale.getValue() == -1571);
    }

    @Test
    public void testHashCode() {

        ScaleB12 scale = new ScaleB12(5);
        ScaleB12 scale2 = new ScaleB12(10);

        assertFalse(scale.hashCode() == scale2.hashCode());
        assertTrue(scale.hashCode() == scale.hashCode());
        assertTrue(scale2.hashCode() == scale2.hashCode());

        ScaleB12 scale3 = new ScaleB12(scale.getValue());

        assertTrue(scale.hashCode() == scale3.hashCode());
        assertFalse(scale2.hashCode() == scale3.hashCode());
    }

    @Test
    public void testEquals() {

        ScaleB12 scale = new ScaleB12(5);

        assertFalse(scale.equals(null));

        assertTrue(scale.equals(scale));

        ScaleB12 scale2 = new ScaleB12(scale.getValue() + 5);

        assertFalse(scale.equals(new String()));
        assertFalse(scale.equals(scale2));

        scale2.setValue(scale.getValue());
        assertTrue(scale.equals(scale2));
    }
}
