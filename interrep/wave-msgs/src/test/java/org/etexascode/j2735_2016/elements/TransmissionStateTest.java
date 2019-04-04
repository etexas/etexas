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

import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the transmission state element.
 * 
 * @author ttevendale
 */
public class TransmissionStateTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        TransmissionState transmission = new TransmissionState(Transmission.NEUTRAL);
        assertTrue(Transmission.NEUTRAL.equals(transmission.getEnumeration()));

        // max value
        transmission = new TransmissionState(Transmission.UNAVAILABLE);
        assertTrue(Transmission.UNAVAILABLE.equals(transmission.getEnumeration()));

        transmission = new TransmissionState(Transmission.REVERSE_GEARS);
        assertTrue(Transmission.REVERSE_GEARS.equals(transmission.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        TransmissionState transmission = new TransmissionState(Transmission.NEUTRAL);
        String encodedTransmission = transmission.encodeUPER();
        assertTrue("000".equals(encodedTransmission));

        // test max
        transmission = new TransmissionState(Transmission.UNAVAILABLE);
        encodedTransmission = transmission.encodeUPER();
        assertTrue("111".equals(encodedTransmission));

        transmission = new TransmissionState(Transmission.RESERVED1);
        encodedTransmission = transmission.encodeUPER();
        assertTrue("100".equals(encodedTransmission));
    }

    @Test
    public void testDecodeUPER() {

        TransmissionState transmission = new TransmissionState();

        // test min
        String remainingBits = transmission.decodeUPER("000");
        assertTrue("".equals(remainingBits));
        assertTrue(Transmission.NEUTRAL.equals(transmission.getEnumeration()));

        // test max
        remainingBits = transmission.decodeUPER("111");
        assertTrue("".equals(remainingBits));
        assertTrue(Transmission.UNAVAILABLE.equals(transmission.getEnumeration()));

        remainingBits = transmission.decodeUPER("101");
        assertTrue("".equals(remainingBits));
        assertTrue(Transmission.RESERVED2.equals(transmission.getEnumeration()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        TransmissionState transmission = new TransmissionState();
        thrown.expect(IllegalArgumentException.class);
        transmission.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        TransmissionState transmission = new TransmissionState();
        String remainingBits = transmission.decodeUPER("00101");
        assertTrue("01".equals(remainingBits));
        assertTrue(Transmission.PARK.equals(transmission.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        TransmissionState transmission = new TransmissionState(Transmission.FORWARD_GEARS);
        TransmissionState transmission2 = new TransmissionState(Transmission.NEUTRAL);

        assertFalse(transmission.hashCode() == transmission2.hashCode());
        assertTrue(transmission.hashCode() == transmission.hashCode());
        assertTrue(transmission2.hashCode() == transmission2.hashCode());

        TransmissionState transmission3 = new TransmissionState(transmission.getEnumeration());

        assertTrue(transmission.hashCode() == transmission3.hashCode());
        assertFalse(transmission2.hashCode() == transmission3.hashCode());
    }

    @Test
    public void testEquals() {

        TransmissionState transmission = new TransmissionState(Transmission.FORWARD_GEARS);

        assertFalse(transmission.equals(null));

        assertTrue(transmission.equals(transmission));

        TransmissionState transmission2 = new TransmissionState(Transmission.REVERSE_GEARS);

        assertFalse(transmission.equals(new String()));
        assertFalse(transmission.equals(transmission2));

        transmission2.setEnumeration(transmission.getEnumeration());
        assertTrue(transmission.equals(transmission2));
    }
}
