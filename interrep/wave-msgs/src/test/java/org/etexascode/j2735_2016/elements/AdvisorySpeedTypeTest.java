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

import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the advisory speed type element.
 * 
 * @author ttevendale
 */
public class AdvisorySpeedTypeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType(Advisory.NONE);
        assertTrue(Advisory.NONE.equals(advisorySpeedType.getEnumeration()));

        // max value
        advisorySpeedType = new AdvisorySpeedType(Advisory.TRANSIT);
        assertTrue(Advisory.TRANSIT.equals(advisorySpeedType.getEnumeration()));

        advisorySpeedType = new AdvisorySpeedType(Advisory.ECODRIVE);
        assertTrue(Advisory.ECODRIVE.equals(advisorySpeedType.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType(Advisory.NONE);
        String encodedAdvisorySpeedType = advisorySpeedType.encodeUPER();
        assertTrue("000".equals(encodedAdvisorySpeedType));

        // test max
        advisorySpeedType = new AdvisorySpeedType(Advisory.TRANSIT);
        encodedAdvisorySpeedType = advisorySpeedType.encodeUPER();
        assertTrue("011".equals(encodedAdvisorySpeedType));

        advisorySpeedType = new AdvisorySpeedType(Advisory.GREENWAVE);
        encodedAdvisorySpeedType = advisorySpeedType.encodeUPER();
        assertTrue("001".equals(encodedAdvisorySpeedType));
    }

    @Test
    public void testDecodeUPER() {

        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType();

        // test min
        String remainingBits = advisorySpeedType.decodeUPER("000");
        assertTrue("".equals(remainingBits));
        assertTrue(Advisory.NONE.equals(advisorySpeedType.getEnumeration()));

        // test max
        remainingBits = advisorySpeedType.decodeUPER("011");
        assertTrue("".equals(remainingBits));
        assertTrue(Advisory.TRANSIT.equals(advisorySpeedType.getEnumeration()));

        remainingBits = advisorySpeedType.decodeUPER("010");
        assertTrue("".equals(remainingBits));
        assertTrue(Advisory.ECODRIVE.equals(advisorySpeedType.getEnumeration()));
    }

    @Test
    public void testDecodeUPERExtension() {

        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeedType.decodeUPER("100");
    }

    @Test
    public void testDecodeUPERLessBits() {

        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeedType.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        AdvisorySpeedType advisorySpeedType = new AdvisorySpeedType();
        String remainingBits = advisorySpeedType.decodeUPER("00001010");
        assertTrue("01010".equals(remainingBits));
        assertTrue(Advisory.NONE.equals(advisorySpeedType.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        AdvisorySpeedType AdvisorySpeedType = new AdvisorySpeedType(Advisory.TRANSIT);
        AdvisorySpeedType AdvisorySpeedType2 = new AdvisorySpeedType(Advisory.ECODRIVE);

        assertFalse(AdvisorySpeedType.hashCode() == AdvisorySpeedType2.hashCode());
        assertTrue(AdvisorySpeedType.hashCode() == AdvisorySpeedType.hashCode());
        assertTrue(AdvisorySpeedType2.hashCode() == AdvisorySpeedType2.hashCode());

        AdvisorySpeedType AdvisorySpeedType3 = new AdvisorySpeedType(AdvisorySpeedType.getEnumeration());

        assertTrue(AdvisorySpeedType.hashCode() == AdvisorySpeedType3.hashCode());
        assertFalse(AdvisorySpeedType2.hashCode() == AdvisorySpeedType3.hashCode());
    }

    @Test
    public void testEquals() {

        AdvisorySpeedType AdvisorySpeedType = new AdvisorySpeedType(Advisory.NONE);

        assertFalse(AdvisorySpeedType.equals(null));

        assertTrue(AdvisorySpeedType.equals(AdvisorySpeedType));

        AdvisorySpeedType AdvisorySpeedType2 = new AdvisorySpeedType(Advisory.TRANSIT);

        assertFalse(AdvisorySpeedType.equals(new String()));
        assertFalse(AdvisorySpeedType.equals(AdvisorySpeedType2));

        AdvisorySpeedType2.setEnumeration(AdvisorySpeedType.getEnumeration());
        assertTrue(AdvisorySpeedType.equals(AdvisorySpeedType2));
    }
}
