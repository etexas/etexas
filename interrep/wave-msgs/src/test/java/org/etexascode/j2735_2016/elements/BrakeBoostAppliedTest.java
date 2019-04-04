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

import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the brake boost applied element.
 * 
 * @author ttevendale
 */
public class BrakeBoostAppliedTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.UNAVAILABLE);
        assertTrue(BrakeBoost.UNAVAILABLE.equals(brakeBoost.getEnumeration()));

        // max value
        brakeBoost = new BrakeBoostApplied(BrakeBoost.ON);
        assertTrue(BrakeBoost.ON.equals(brakeBoost.getEnumeration()));

        brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        assertTrue(BrakeBoost.OFF.equals(brakeBoost.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.UNAVAILABLE);
        String encodedBrakeBoost = brakeBoost.encodeUPER();
        assertTrue("00".equals(encodedBrakeBoost));

        // test max
        brakeBoost = new BrakeBoostApplied(BrakeBoost.ON);
        encodedBrakeBoost = brakeBoost.encodeUPER();
        assertTrue("10".equals(encodedBrakeBoost));

        brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        encodedBrakeBoost = brakeBoost.encodeUPER();
        assertTrue("01".equals(encodedBrakeBoost));
    }

    @Test
    public void testDecodeUPER() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied();

        // test min
        String remainingBits = brakeBoost.decodeUPER("00");
        assertTrue("".equals(remainingBits));
        assertTrue(BrakeBoost.UNAVAILABLE.equals(brakeBoost.getEnumeration()));

        // test max
        remainingBits = brakeBoost.decodeUPER("10");
        assertTrue("".equals(remainingBits));
        assertTrue(BrakeBoost.ON.equals(brakeBoost.getEnumeration()));

        remainingBits = brakeBoost.decodeUPER("01");
        assertTrue("".equals(remainingBits));
        assertTrue(BrakeBoost.OFF.equals(brakeBoost.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        brakeBoost.decodeUPER("11");
    }

    @Test
    public void testDecodeUPERLessBits() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied();
        thrown.expect(IllegalArgumentException.class);
        brakeBoost.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied();
        String remainingBits = brakeBoost.decodeUPER("10001010"); // 7
        assertTrue("001010".equals(remainingBits));
        assertTrue(BrakeBoost.ON.equals(brakeBoost.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        BrakeBoostApplied brakeBoost2 = new BrakeBoostApplied(BrakeBoost.ON);

        assertFalse(brakeBoost.hashCode() == brakeBoost2.hashCode());
        assertTrue(brakeBoost.hashCode() == brakeBoost.hashCode());
        assertTrue(brakeBoost2.hashCode() == brakeBoost2.hashCode());

        BrakeBoostApplied brakeBoost3 = new BrakeBoostApplied(brakeBoost.getEnumeration());

        assertTrue(brakeBoost.hashCode() == brakeBoost3.hashCode());
        assertFalse(brakeBoost2.hashCode() == brakeBoost3.hashCode());
    }

    @Test
    public void testEquals() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);

        assertFalse(brakeBoost.equals(null));

        assertTrue(brakeBoost.equals(brakeBoost));

        BrakeBoostApplied brakeBoost2 = new BrakeBoostApplied(BrakeBoost.ON);

        assertFalse(brakeBoost.equals(new String()));
        assertFalse(brakeBoost.equals(brakeBoost2));

        brakeBoost2.setEnumeration(brakeBoost.getEnumeration());
        assertTrue(brakeBoost.equals(brakeBoost2));
    }
}
