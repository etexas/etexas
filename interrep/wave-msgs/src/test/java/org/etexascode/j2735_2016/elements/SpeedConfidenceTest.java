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

import org.etexascode.j2735_2016.elements.SpeedConfidence.SpeedPrecision;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the speed confidence element.
 * 
 * @author ttevendale
 */
public class SpeedConfidenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.UNAVAILABLE);
        assertTrue(SpeedPrecision.UNAVAILABLE.equals(confidence.getEnumeration()));

        // max value
        confidence = new SpeedConfidence(SpeedPrecision.PREC0_01MS);
        assertTrue(SpeedPrecision.PREC0_01MS.equals(confidence.getEnumeration()));

        confidence = new SpeedConfidence(SpeedPrecision.PREC0_05MS);
        assertTrue(SpeedPrecision.PREC0_05MS.equals(confidence.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.UNAVAILABLE);
        String encodedConfidence = confidence.encodeUPER();
        assertTrue("000".equals(encodedConfidence));

        // test max
        confidence = new SpeedConfidence(SpeedPrecision.PREC0_01MS);
        encodedConfidence = confidence.encodeUPER();
        assertTrue("111".equals(encodedConfidence));

        confidence = new SpeedConfidence(SpeedPrecision.PREC0_1MS);
        encodedConfidence = confidence.encodeUPER();
        assertTrue("101".equals(encodedConfidence));
    }

    @Test
    public void testDecodeUPER() {

        SpeedConfidence confidence = new SpeedConfidence();

        // test min
        String remainingBits = confidence.decodeUPER("000");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedPrecision.UNAVAILABLE.equals(confidence.getEnumeration()));

        // test max
        remainingBits = confidence.decodeUPER("111");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedPrecision.PREC0_01MS.equals(confidence.getEnumeration()));

        remainingBits = confidence.decodeUPER("001");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedPrecision.PREC100MS.equals(confidence.getEnumeration()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        SpeedConfidence confidence = new SpeedConfidence();
        thrown.expect(IllegalArgumentException.class);
        confidence.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SpeedConfidence confidence = new SpeedConfidence();
        String remainingBits = confidence.decodeUPER("010110");
        assertTrue("110".equals(remainingBits));
        assertTrue(SpeedPrecision.PREC10MS.equals(confidence.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.PREC1MS);
        SpeedConfidence confidence2 = new SpeedConfidence(SpeedPrecision.PREC5MS);

        assertFalse(confidence.hashCode() == confidence2.hashCode());
        assertTrue(confidence.hashCode() == confidence.hashCode());
        assertTrue(confidence2.hashCode() == confidence2.hashCode());

        SpeedConfidence confidence3 = new SpeedConfidence(confidence.getEnumeration());

        assertTrue(confidence.hashCode() == confidence3.hashCode());
        assertFalse(confidence2.hashCode() == confidence3.hashCode());
    }

    @Test
    public void testEquals() {

        SpeedConfidence confidence = new SpeedConfidence(SpeedPrecision.PREC100MS);

        assertFalse(confidence.equals(null));

        assertTrue(confidence.equals(confidence));

        SpeedConfidence confidence2 = new SpeedConfidence(SpeedPrecision.PREC0_01MS);

        assertFalse(confidence.equals(new String()));
        assertFalse(confidence.equals(confidence2));

        confidence2.setEnumeration(confidence.getEnumeration());
        assertTrue(confidence.equals(confidence2));
    }
}
