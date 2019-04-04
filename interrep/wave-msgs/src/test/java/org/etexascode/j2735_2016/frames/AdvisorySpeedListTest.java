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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.AdvisorySpeedType.Advisory;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the advisory speed list frame.
 * 
 * @author ttevendale
 */
public class AdvisorySpeedListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MIN_LIST_SIZE);
        assertTrue(advisorySpeeds.getAdvisorySpeedArray().length == AdvisorySpeedList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MAX_LIST_SIZE);
        assertTrue(advisorySpeeds.getAdvisorySpeedArray().length == AdvisorySpeedList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numAdvisorySpeeds = 12;
        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(numAdvisorySpeeds);
        assertTrue(advisorySpeeds.getAdvisorySpeedArray().length == numAdvisorySpeeds);
    }

    @Test
    public void testEncodeUPERMin() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MIN_LIST_SIZE);
        AdvisorySpeed advisorySpeed = new AdvisorySpeed(Advisory.ECODRIVE);
        AdvisorySpeed[] advisorySpeedArray = advisorySpeeds.getAdvisorySpeedArray();
        advisorySpeedArray[0] = advisorySpeed;

        String listSize = "0000";
        String remainingBits = advisorySpeed.encodeUPER();

        assertTrue((listSize + remainingBits).equals(advisorySpeeds.encodeUPER()));

        advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        advisorySpeeds.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MAX_LIST_SIZE);

        AdvisorySpeed[] advisorySpeedArray = advisorySpeeds.getAdvisorySpeedArray();
        for (int i = 0; i < advisorySpeedArray.length; i++) {

            AdvisorySpeed advisorySpeed = new AdvisorySpeed(Advisory.GREENWAVE);
            advisorySpeedArray[i] = advisorySpeed;
            remainingBits += advisorySpeed.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(advisorySpeeds.encodeUPER()));

        advisorySpeeds = new AdvisorySpeedList(AdvisorySpeedList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        advisorySpeeds.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList();
        thrown.expect(IllegalStateException.class);
        advisorySpeeds.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(Advisory.TRANSIT);
        advisorySpeed.setSpeed(52);
        String listSize = "0000";
        String remainingBits = advisorySpeed.encodeUPER();

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList();
        advisorySpeeds.decodeUPER(listSize + remainingBits);
        AdvisorySpeed[] advisorySpeedArray = advisorySpeeds.getAdvisorySpeedArray();
        assertTrue(AdvisorySpeedList.MIN_LIST_SIZE == advisorySpeedArray.length);
        assertTrue(advisorySpeed.equals(advisorySpeedArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        AdvisorySpeed advisorySpeed = new AdvisorySpeed(Advisory.TRANSIT);
        AdvisorySpeed advisorySpeed2 = new AdvisorySpeed(Advisory.ECODRIVE);

        String listSize = "1111";
        String remainingBits = advisorySpeed.encodeUPER();

        for (int i = 0; i < AdvisorySpeedList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += advisorySpeed2.encodeUPER();
        }

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList();
        advisorySpeeds.decodeUPER(listSize + remainingBits);

        AdvisorySpeed[] advisorySpeedArray = advisorySpeeds.getAdvisorySpeedArray();
        assertTrue(AdvisorySpeedList.MAX_LIST_SIZE == advisorySpeedArray.length);
        assertTrue(advisorySpeed.equals(advisorySpeedArray[0]));
        for (int i = 1; i < AdvisorySpeedList.MAX_LIST_SIZE; i++) {

            assertTrue(advisorySpeed2.equals(advisorySpeedArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList();
        thrown.expect(IllegalArgumentException.class);
        advisorySpeeds.decodeUPER("010");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList();
        thrown.expect(IllegalArgumentException.class);
        // 1011 = 12 objects, but there's none
        advisorySpeeds.decodeUPER("10111010100");
    }

    @Test
    public void testHashCode() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(1);
        advisorySpeeds.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.NONE);

        assertTrue(advisorySpeeds.hashCode() == advisorySpeeds.hashCode());

        AdvisorySpeedList advisorySpeeds2 = new AdvisorySpeedList(2);

        assertFalse(advisorySpeeds.hashCode() == advisorySpeeds2.hashCode());

        advisorySpeeds2 = new AdvisorySpeedList(1);
        advisorySpeeds2.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.ECODRIVE);

        assertFalse(advisorySpeeds.hashCode() == advisorySpeeds2.hashCode());

        advisorySpeeds2.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.NONE);

        assertTrue(advisorySpeeds.hashCode() == advisorySpeeds2.hashCode());
    }

    @Test
    public void testEquals() {

        AdvisorySpeedList advisorySpeeds = new AdvisorySpeedList(1);
        advisorySpeeds.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.NONE);

        assertTrue(advisorySpeeds.equals(advisorySpeeds));
        assertFalse(advisorySpeeds.equals(null));
        assertFalse(advisorySpeeds.equals(new String()));

        AdvisorySpeedList advisorySpeeds2 = new AdvisorySpeedList(2);

        assertFalse(advisorySpeeds.equals(advisorySpeeds2));

        advisorySpeeds2 = new AdvisorySpeedList(1);
        advisorySpeeds2.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.ECODRIVE);

        assertFalse(advisorySpeeds.equals(advisorySpeeds2));

        advisorySpeeds2.getAdvisorySpeedArray()[0] = new AdvisorySpeed(Advisory.NONE);

        assertTrue(advisorySpeeds.equals(advisorySpeeds2));
    }
}
