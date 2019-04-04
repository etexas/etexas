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
 * Unit tests for the brake applied status element.
 * 
 * @author ttevendale
 */
public class BrakeAppliedStatusTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();

        assertTrue("10000".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("00001".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(true);
        assertTrue("00010".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("00011".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(false);
        status.setLeftRear(true);
        assertTrue("00100".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("00101".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(true);
        assertTrue("00110".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("00111".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(false);
        status.setLeftRear(false);
        status.setLeftFront(true);
        assertTrue("01000".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("01001".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(true);
        assertTrue("01010".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("01011".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(false);
        status.setLeftRear(true);
        assertTrue("01100".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("01101".equals(status.encodeUPER()));

        status.setRightRear(false);
        status.setRightFront(true);
        assertTrue("01110".equals(status.encodeUPER()));

        status.setRightRear(true);
        assertTrue("01111".equals(status.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();

        String remainingBits = status.decodeUPER("10000");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("00001");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("00010");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("00011");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("00100");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("00101");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("00110");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("00111");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("01000");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("01001");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("01010");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("01011");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("01100");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("01101");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertFalse(status.isRightFront());
        assertTrue(status.isRightRear());

        remainingBits = status.decodeUPER("01110");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertFalse(status.isRightRear());

        remainingBits = status.decodeUPER("01111");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertTrue(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertTrue(status.isRightRear());
    }

    @Test
    public void testDecodeUPERUnavailableWithBitsActive() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("11111");
    }

    @Test
    public void testDecodeUPERAvailableWithNoBitsActive() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("00000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        String remainingBits = status.decodeUPER("010101");
        assertTrue("1".equals(remainingBits));
        assertTrue(status.isLeftFront());
        assertFalse(status.isLeftRear());
        assertTrue(status.isRightFront());
        assertFalse(status.isRightRear());
    }

    @Test
    public void testHashCode() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        status.setLeftFront(true);

        BrakeAppliedStatus status2 = new BrakeAppliedStatus();
        status2.setLeftFront(false);

        assertFalse(status.hashCode() == status2.hashCode());
        assertTrue(status.hashCode() == status.hashCode());
        assertTrue(status2.hashCode() == status2.hashCode());

        BrakeAppliedStatus status3 = new BrakeAppliedStatus();
        status3.setLeftFront(status.isLeftFront());
        status3.setLeftRear(status.isLeftRear());
        status3.setRightFront(status.isRightFront());
        status3.setRightRear(status.isRightRear());

        assertTrue(status.hashCode() == status3.hashCode());
        assertFalse(status2.hashCode() == status3.hashCode());
    }

    @Test
    public void testEquals() {

        BrakeAppliedStatus status = new BrakeAppliedStatus();
        status.setLeftFront(true);
        status.setLeftRear(true);
        status.setRightFront(true);
        status.setRightRear(true);

        assertFalse(status.equals(null));

        assertTrue(status.equals(status));

        BrakeAppliedStatus status2 = new BrakeAppliedStatus();
        status2.setLeftFront(false);
        status2.setLeftRear(false);
        status2.setRightFront(false);
        status2.setRightRear(false);

        assertFalse(status.equals(new String()));
        assertFalse(status.equals(status2));

        status2.setLeftFront(status.isLeftFront());
        assertFalse(status.equals(status2));

        status2.setLeftRear(status.isLeftRear());
        assertFalse(status.equals(status2));

        status2.setRightFront(status.isRightFront());
        assertFalse(status.equals(status2));

        status2.setRightRear(status.isRightRear());
        assertTrue(status.equals(status2));
    }
}
