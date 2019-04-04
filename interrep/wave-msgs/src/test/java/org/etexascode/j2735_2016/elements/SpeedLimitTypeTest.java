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

import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the speed limit type element.
 * 
 * @author ttevendale
 */
public class SpeedLimitTypeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        SpeedLimitType type = new SpeedLimitType(SpeedLimit.UNKNOWN);
        assertTrue(SpeedLimit.UNKNOWN.equals(type.getEnumeration()));

        // max value
        type = new SpeedLimitType(SpeedLimit.VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED);
        assertTrue(SpeedLimit.VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED.equals(type.getEnumeration()));

        type = new SpeedLimitType(SpeedLimit.MAX_SPEED_IN_SCHOOL_ZONE);
        assertTrue(SpeedLimit.MAX_SPEED_IN_SCHOOL_ZONE.equals(type.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SpeedLimitType type = new SpeedLimitType(SpeedLimit.UNKNOWN);
        String encodedStatus = type.encodeUPER();
        assertTrue("00000".equals(encodedStatus));

        // test max
        type = new SpeedLimitType(SpeedLimit.VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED);
        encodedStatus = type.encodeUPER();
        assertTrue("01100".equals(encodedStatus));

        type = new SpeedLimitType(SpeedLimit.VEHICLES_WITH_TRAILERS_MAX_SPEED);
        encodedStatus = type.encodeUPER();
        assertTrue("01011".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        SpeedLimitType type = new SpeedLimitType();

        // test min
        String remainingBits = type.decodeUPER("00000");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedLimit.UNKNOWN.equals(type.getEnumeration()));

        // test max
        remainingBits = type.decodeUPER("01100");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedLimit.VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED.equals(type.getEnumeration()));

        remainingBits = type.decodeUPER("01010");
        assertTrue("".equals(remainingBits));
        assertTrue(SpeedLimit.VEHICLES_WITH_TRAILERS_MIN_SPEED.equals(type.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        type.decodeUPER("01101");
    }

    @Test
    public void testDecodeUPERExtension() {

        SpeedLimitType type = new SpeedLimitType();
        thrown.expect(IllegalArgumentException.class);
        type.decodeUPER("10000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        SpeedLimitType type = new SpeedLimitType();
        thrown.expect(IllegalArgumentException.class);
        type.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SpeedLimitType type = new SpeedLimitType();
        String remainingBits = type.decodeUPER("01001000");
        assertTrue("000".equals(remainingBits));
        assertTrue(SpeedLimit.TRUCK_NIGHT_MAX_SPEED.equals(type.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        SpeedLimitType type = new SpeedLimitType(SpeedLimit.TRUCK_MIN_SPEED);
        SpeedLimitType type2 = new SpeedLimitType(SpeedLimit.TRUCK_MAX_SPEED);

        assertFalse(type.hashCode() == type2.hashCode());
        assertTrue(type.hashCode() == type.hashCode());
        assertTrue(type2.hashCode() == type2.hashCode());

        SpeedLimitType type3 = new SpeedLimitType(type.getEnumeration());

        assertTrue(type.hashCode() == type3.hashCode());
        assertFalse(type2.hashCode() == type3.hashCode());
    }

    @Test
    public void testEquals() {

        SpeedLimitType type = new SpeedLimitType(SpeedLimit.VEHICLE_MAX_SPEED);

        assertFalse(type.equals(null));

        assertTrue(type.equals(type));

        SpeedLimitType type2 = new SpeedLimitType(SpeedLimit.VEHICLE_NIGHT_MAX_SPEED);

        assertFalse(type.equals(new String()));
        assertFalse(type.equals(type2));

        type2.setEnumeration(type.getEnumeration());
        assertTrue(type.equals(type2));
    }
}
