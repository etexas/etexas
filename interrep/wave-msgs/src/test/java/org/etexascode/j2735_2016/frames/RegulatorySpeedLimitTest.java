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

import org.etexascode.j2735_2016.elements.SpeedLimitType;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.etexascode.j2735_2016.elements.Velocity;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the regulatory speed limit frame.
 * 
 * @author ttevendale
 */
public class RegulatorySpeedLimitTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    RegulatorySpeedLimit speedLimit;

    String encodedBits;

    @Before
    public void init() {

        SpeedLimitType type = new SpeedLimitType(SpeedLimit.TRUCK_MAX_SPEED);
        Velocity speed = new Velocity(183);

        speedLimit = new RegulatorySpeedLimit(type, speed);
        encodedBits = type.encodeUPER() + speed.encodeUPER();
    }

    @Test
    public void testConstructor() {

        SpeedLimitType type = new SpeedLimitType(SpeedLimit.MAX_SPEED_IN_CONSTRUCTION_ZONE);
        Velocity speed = new Velocity(1111);

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(type, speed);

        assertTrue(type.equals(speedLimit.getType()));
        assertTrue(speed.equals(speedLimit.getSpeed()));
    }

    @Test
    public void testConstructorNullType() {

        thrown.expect(NullPointerException.class);
        new RegulatorySpeedLimit(null, new Velocity(239));
    }

    @Test
    public void testConstructorNullSpeed() {

        thrown.expect(NullPointerException.class);
        new RegulatorySpeedLimit(new SpeedLimitType(SpeedLimit.MAX_SPEED_IN_SCHOOL_ZONE), null);
    }

    @Test
    public void testConstructorPrimitive() {

        SpeedLimit type = SpeedLimit.MAX_SPEED_IN_SCHOOL_ZONE_WHEN_CHILDREN_ARE_PRESENT;
        int speed = 383;

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit(type, speed);

        assertTrue(type.equals(speedLimit.getType().getEnumeration()));
        assertTrue(speed == speedLimit.getSpeed().getValue());
    }

    @Test
    public void testConstructorPrimitiveNullType() {

        thrown.expect(NullPointerException.class);
        new RegulatorySpeedLimit(null, 1578);
    }

    @Test
    public void testSetType() {

        SpeedLimitType type = new SpeedLimitType(SpeedLimit.TRUCK_MAX_SPEED);

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit();
        speedLimit.setType(type);

        assertTrue(type.equals(speedLimit.getType()));

        thrown.expect(NullPointerException.class);
        speedLimit.setType((SpeedLimitType)null);
    }

    @Test
    public void testSetTypePrimitive() {

        SpeedLimit type = SpeedLimit.TRUCK_MIN_SPEED;

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit();
        speedLimit.setType(type);

        assertTrue(type.equals(speedLimit.getType().getEnumeration()));

        thrown.expect(NullPointerException.class);
        speedLimit.setType((SpeedLimit)null);
    }

    @Test
    public void testSetSpeed() {

        Velocity speed = new Velocity(222);

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit();
        speedLimit.setSpeed(speed);

        assertTrue(speed.equals(speedLimit.getSpeed()));

        thrown.expect(NullPointerException.class);
        speedLimit.setSpeed(null);
    }

    @Test
    public void testSetSpeedPrimitive() {

        int speed = 15;

        RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit();
        speedLimit.setSpeed(speed);

        assertTrue(speed == speedLimit.getSpeed().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equals(speedLimit.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        RegulatorySpeedLimit decodedSpeedLimit = new RegulatorySpeedLimit();
        decodedSpeedLimit.decodeUPER(encodedBits);
        assertTrue(speedLimit.equals(decodedSpeedLimit));
    }

    @Test
    public void testHashCode() {

        SpeedLimit type = speedLimit.getType().getEnumeration();
        int speed = speedLimit.getSpeed().getValue();

        RegulatorySpeedLimit speedLimit2 = new RegulatorySpeedLimit(SpeedLimit.VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED, speed + 1);

        assertFalse(speedLimit.hashCode() == speedLimit2.hashCode());
        assertTrue(speedLimit.hashCode() == speedLimit.hashCode());
        assertTrue(speedLimit2.hashCode() == speedLimit2.hashCode());

        RegulatorySpeedLimit speedLimit3 = new RegulatorySpeedLimit(type, speed);

        assertTrue(speedLimit.hashCode() == speedLimit3.hashCode());
        assertFalse(speedLimit2.hashCode() == speedLimit3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(speedLimit.equals(speedLimit));
        assertFalse(speedLimit.equals(null));
        assertFalse(speedLimit.equals(new String()));

        SpeedLimit type = speedLimit.getType().getEnumeration();
        int speed = speedLimit.getSpeed().getValue();

        // different
        RegulatorySpeedLimit speedLimit2 = new RegulatorySpeedLimit(SpeedLimit.VEHICLES_WITH_TRAILERS_MAX_SPEED, speed + 1);

        assertFalse(speedLimit.equals(speedLimit2));

        // different type
        speedLimit2 = new RegulatorySpeedLimit(SpeedLimit.VEHICLES_WITH_TRAILERS_MIN_SPEED, speed);

        assertFalse(speedLimit.equals(speedLimit2));

        // different speed
        speedLimit2 = new RegulatorySpeedLimit(type, speed + 1);

        assertFalse(speedLimit.equals(speedLimit2));

        // same
        speedLimit2 = new RegulatorySpeedLimit(type, speed);

        assertTrue(speedLimit.equals(speedLimit2));
    }
}
