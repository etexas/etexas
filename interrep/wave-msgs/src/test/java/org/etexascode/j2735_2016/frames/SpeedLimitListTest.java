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

import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the speed limit list frame.
 * 
 * @author ttevendale
 */
public class SpeedLimitListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        SpeedLimitList speedLimits = new SpeedLimitList(SpeedLimitList.MIN_LIST_SIZE);
        assertTrue(speedLimits.getSpeedLimitArray().length == SpeedLimitList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        speedLimits = new SpeedLimitList(SpeedLimitList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        SpeedLimitList speedLimits = new SpeedLimitList(SpeedLimitList.MAX_LIST_SIZE);
        assertTrue(speedLimits.getSpeedLimitArray().length == SpeedLimitList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        speedLimits = new SpeedLimitList(SpeedLimitList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numSpeedLimits = 7;
        SpeedLimitList speedLimits = new SpeedLimitList(numSpeedLimits);
        assertTrue(speedLimits.getSpeedLimitArray().length == numSpeedLimits);
    }

    @Test
    public void testEncodeUPERMin() {

        SpeedLimitList speedLimits = new SpeedLimitList(SpeedLimitList.MIN_LIST_SIZE);
        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(SpeedLimit.TRUCK_MAX_SPEED, 383);
        RegulatorySpeedLimit[] speedLimitArray = speedLimits.getSpeedLimitArray();
        speedLimitArray[0] = speedLimit;

        String listSize = "0000";
        String remainingBits = speedLimit.encodeUPER();

        assertTrue((listSize + remainingBits).equals(speedLimits.encodeUPER()));

        speedLimits = new SpeedLimitList(SpeedLimitList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        speedLimits.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1000";
        String remainingBits = "";

        SpeedLimitList speedLimits = new SpeedLimitList(SpeedLimitList.MAX_LIST_SIZE);

        RegulatorySpeedLimit[] speedLimitArray = speedLimits.getSpeedLimitArray();
        for (int i = 0; i < speedLimitArray.length; i++) {

            RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, i);
            speedLimitArray[i] = speedLimit;
            remainingBits += speedLimit.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(speedLimits.encodeUPER()));

        speedLimits = new SpeedLimitList(SpeedLimitList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        speedLimits.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        SpeedLimitList speedLimits = new SpeedLimitList();
        thrown.expect(IllegalStateException.class);
        speedLimits.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(SpeedLimit.VEHICLE_MAX_SPEED, 3846);
        String listSize = "0000";
        String remainingBits = speedLimit.encodeUPER();

        SpeedLimitList speedLimits = new SpeedLimitList();
        speedLimits.decodeUPER(listSize + remainingBits);
        RegulatorySpeedLimit[] speedLimitArray = speedLimits.getSpeedLimitArray();
        assertTrue(SpeedLimitList.MIN_LIST_SIZE == speedLimitArray.length);
        assertTrue(speedLimit.equals(speedLimitArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(SpeedLimit.MAX_SPEED_IN_CONSTRUCTION_ZONE, 3737);
        RegulatorySpeedLimit speedLimit2 = new RegulatorySpeedLimit(SpeedLimit.TRUCK_MAX_SPEED, 4848);

        String listSize = "1000";
        String remainingBits = speedLimit.encodeUPER();

        for (int i = 0; i < SpeedLimitList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += speedLimit2.encodeUPER();
        }

        SpeedLimitList speedLimits = new SpeedLimitList();
        speedLimits.decodeUPER(listSize + remainingBits);

        RegulatorySpeedLimit[] speedLimitArray = speedLimits.getSpeedLimitArray();
        assertTrue(SpeedLimitList.MAX_LIST_SIZE == speedLimitArray.length);
        assertTrue(speedLimit.equals(speedLimitArray[0]));
        for (int i = 1; i < SpeedLimitList.MAX_LIST_SIZE; i++) {

            assertTrue(speedLimit2.equals(speedLimitArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        SpeedLimitList speedLimits = new SpeedLimitList();
        thrown.expect(IllegalArgumentException.class);
        speedLimits.decodeUPER("1001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        SpeedLimitList speedLimits = new SpeedLimitList();
        thrown.expect(IllegalArgumentException.class);
        speedLimits.decodeUPER("101");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        SpeedLimitList speedLimits = new SpeedLimitList();
        thrown.expect(IllegalArgumentException.class);
        // 111 = 8 objects, but there's none
        speedLimits.decodeUPER("1110011010100");
    }

    @Test
    public void testHashCode() {

        SpeedLimitList speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 1);

        assertTrue(speedLimits.hashCode() == speedLimits.hashCode());

        SpeedLimitList speedLimits2 = new SpeedLimitList(2);

        assertFalse(speedLimits.hashCode() == speedLimits2.hashCode());

        speedLimits2 = new SpeedLimitList(1);
        speedLimits2.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLES_WITH_TRAILERS_MAX_SPEED, 484);

        assertFalse(speedLimits.hashCode() == speedLimits2.hashCode());

        speedLimits2.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 1);

        assertTrue(speedLimits.hashCode() == speedLimits2.hashCode());
    }

    @Test
    public void testEquals() {

        SpeedLimitList speedLimits = new SpeedLimitList(1);
        speedLimits.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 1);

        assertTrue(speedLimits.equals(speedLimits));
        assertFalse(speedLimits.equals(null));
        assertFalse(speedLimits.equals(new String()));

        SpeedLimitList speedLimits2 = new SpeedLimitList(2);

        assertFalse(speedLimits.equals(speedLimits2));

        speedLimits2 = new SpeedLimitList(1);
        speedLimits2.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.VEHICLES_WITH_TRAILERS_MAX_SPEED, 484);

        assertFalse(speedLimits.equals(speedLimits2));

        speedLimits2.getSpeedLimitArray()[0] = new RegulatorySpeedLimit(SpeedLimit.UNKNOWN, 1);

        assertTrue(speedLimits.equals(speedLimits2));
    }
}
