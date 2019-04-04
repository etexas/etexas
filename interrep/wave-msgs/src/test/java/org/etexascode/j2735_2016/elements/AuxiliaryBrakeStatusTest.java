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

import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the auxiliary brake status element.
 * 
 * @author ttevendale
 */
public class AuxiliaryBrakeStatusTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus(AuxiliaryBrake.UNAVAILABLE);
        assertTrue(AuxiliaryBrake.UNAVAILABLE.equals(status.getEnumeration()));

        // max value
        status = new AuxiliaryBrakeStatus(AuxiliaryBrake.RESERVED);
        assertTrue(AuxiliaryBrake.RESERVED.equals(status.getEnumeration()));

        status = new AuxiliaryBrakeStatus(AuxiliaryBrake.OFF);
        assertTrue(AuxiliaryBrake.OFF.equals(status.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus(AuxiliaryBrake.UNAVAILABLE);
        String encodedStatus = status.encodeUPER();
        assertTrue("00".equals(encodedStatus));

        // test max
        status = new AuxiliaryBrakeStatus(AuxiliaryBrake.RESERVED);
        encodedStatus = status.encodeUPER();
        assertTrue("11".equals(encodedStatus));

        status = new AuxiliaryBrakeStatus(AuxiliaryBrake.ON);
        encodedStatus = status.encodeUPER();
        assertTrue("10".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus();

        // test min
        String remainingBits = status.decodeUPER("00");
        assertTrue("".equals(remainingBits));
        assertTrue(AuxiliaryBrake.UNAVAILABLE.equals(status.getEnumeration()));

        // test max
        remainingBits = status.decodeUPER("11");
        assertTrue("".equals(remainingBits));
        assertTrue(AuxiliaryBrake.RESERVED.equals(status.getEnumeration()));

        remainingBits = status.decodeUPER("01");
        assertTrue("".equals(remainingBits));
        assertTrue(AuxiliaryBrake.OFF.equals(status.getEnumeration()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus();
        String remainingBits = status.decodeUPER("0001010");
        assertTrue("01010".equals(remainingBits));
        assertTrue(AuxiliaryBrake.UNAVAILABLE.equals(status.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus(AuxiliaryBrake.OFF);
        AuxiliaryBrakeStatus status2 = new AuxiliaryBrakeStatus(AuxiliaryBrake.ON);

        assertFalse(status.hashCode() == status2.hashCode());
        assertTrue(status.hashCode() == status.hashCode());
        assertTrue(status2.hashCode() == status2.hashCode());

        AuxiliaryBrakeStatus status3 = new AuxiliaryBrakeStatus(status.getEnumeration());

        assertTrue(status.hashCode() == status3.hashCode());
        assertFalse(status2.hashCode() == status3.hashCode());
    }

    @Test
    public void testEquals() {

        AuxiliaryBrakeStatus status = new AuxiliaryBrakeStatus(AuxiliaryBrake.OFF);

        assertFalse(status.equals(null));

        assertTrue(status.equals(status));

        AuxiliaryBrakeStatus status2 = new AuxiliaryBrakeStatus(AuxiliaryBrake.ON);

        assertFalse(status.equals(new String()));
        assertFalse(status.equals(status2));

        status2.setEnumeration(status.getEnumeration());
        assertTrue(status.equals(status2));
    }
}
