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

import org.etexascode.j2735_2016.elements.Acceleration;
import org.etexascode.j2735_2016.elements.VerticalAcceleration;
import org.etexascode.j2735_2016.elements.YawRate;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the acceleration set four way frame.
 * 
 * @author ttevendale
 */
public class AccelerationSet4WayTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    AccelerationSet4Way accelerationSet4Way;

    String encodedBits;

    @Before
    public void init() {

        Acceleration longitude = new Acceleration(151);
        Acceleration latitude = new Acceleration(87);
        VerticalAcceleration vertical = new VerticalAcceleration(5);
        YawRate yawRate = new YawRate(100);

        accelerationSet4Way = new AccelerationSet4Way(longitude, latitude, vertical, yawRate);
        encodedBits = longitude.encodeUPER() + latitude.encodeUPER() + vertical.encodeUPER() + yawRate.encodeUPER();
    }

    @Test
    public void testConstructor() {

        Acceleration longitude = new Acceleration(151);
        Acceleration latitude = new Acceleration(87);
        VerticalAcceleration vertical = new VerticalAcceleration(5);
        YawRate yawRate = new YawRate(100);

        AccelerationSet4Way accelerationSet4Way = new AccelerationSet4Way(longitude, latitude, vertical, yawRate);

        assertTrue(longitude.equals(accelerationSet4Way.getLongitude()));
        assertTrue(latitude.equals(accelerationSet4Way.getLatitude()));
        assertTrue(vertical.equals(accelerationSet4Way.getVertical()));
        assertTrue(yawRate.equals(accelerationSet4Way.getYawRate()));
    }

    @Test
    public void testConstructorNullLongitude() {

        thrown.expect(NullPointerException.class);
        new AccelerationSet4Way(null, new Acceleration(), new VerticalAcceleration(), new YawRate());
    }

    @Test
    public void testConstructorNullLatitude() {

        thrown.expect(NullPointerException.class);
        new AccelerationSet4Way(new Acceleration(), null, new VerticalAcceleration(), new YawRate());
    }

    @Test
    public void testConstructorNullVertical() {

        thrown.expect(NullPointerException.class);
        new AccelerationSet4Way(new Acceleration(), new Acceleration(), null, new YawRate());
    }

    @Test
    public void testConstructorNullYawRate() {

        thrown.expect(NullPointerException.class);
        new AccelerationSet4Way(new Acceleration(), new Acceleration(), new VerticalAcceleration(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int longitude = 5;
        int latitude = 20;
        int vertical = -5;
        int yawRate = 50;

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way(longitude, latitude, vertical, yawRate);

        assertTrue(longitude == accelerationSet.getLongitude().getValue());
        assertTrue(latitude == accelerationSet.getLatitude().getValue());
        assertTrue(vertical == accelerationSet.getVertical().getValue());
        assertTrue(yawRate == accelerationSet.getYawRate().getValue());
    }

    @Test
    public void testSetLongitude() {

        Acceleration longitude = new Acceleration(23);

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setLongitude(longitude);

        assertTrue(longitude.equals(accelerationSet.getLongitude()));

        thrown.expect(NullPointerException.class);
        accelerationSet.setLongitude(null);
    }

    @Test
    public void testSetLongitudePrimitive() {

        int longitude = 5;

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setLongitude(longitude);

        assertTrue(longitude == accelerationSet.getLongitude().getValue());
    }

    @Test
    public void testSetLatitude() {

        Acceleration latitude = new Acceleration(151);

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setLatitude(latitude);

        assertTrue(latitude.equals(accelerationSet.getLatitude()));

        thrown.expect(NullPointerException.class);
        accelerationSet.setLatitude(null);
    }

    @Test
    public void testSetLatitudePrimitive() {

        int latitude = 15;

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setLatitude(latitude);

        assertTrue(latitude == accelerationSet.getLatitude().getValue());
    }

    @Test
    public void testSetVertical() {

        VerticalAcceleration vertical = new VerticalAcceleration(-15);

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setVertical(vertical);

        assertTrue(vertical.equals(accelerationSet.getVertical()));

        thrown.expect(NullPointerException.class);
        accelerationSet.setVertical(null);
    }

    @Test
    public void testSetVerticalPrimitive() {

        int vertical = 22;

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setVertical(vertical);

        assertTrue(vertical == accelerationSet.getVertical().getValue());
    }

    @Test
    public void testSetYawRate() {

        YawRate yawRate = new YawRate(38);

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setYawRate(yawRate);

        assertTrue(yawRate.equals(accelerationSet.getYawRate()));

        thrown.expect(NullPointerException.class);
        accelerationSet.setYawRate(null);
    }

    @Test
    public void testSetYawRatePrimitive() {

        int yawRate = -55;

        AccelerationSet4Way accelerationSet = new AccelerationSet4Way();
        accelerationSet.setYawRate(yawRate);

        assertTrue(yawRate == accelerationSet.getYawRate().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(accelerationSet4Way.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        AccelerationSet4Way decodedAccelerationSet4Way = new AccelerationSet4Way();
        decodedAccelerationSet4Way.decodeUPER(encodedBits);
        assertTrue(accelerationSet4Way.equals(decodedAccelerationSet4Way));
    }

    @Test
    public void testHashCode() {

        int longitude = accelerationSet4Way.getLongitude().getValue();
        int latitude = accelerationSet4Way.getLatitude().getValue();
        int vertical = accelerationSet4Way.getVertical().getValue();
        int yawRate = accelerationSet4Way.getYawRate().getValue();

        AccelerationSet4Way accelerationSet4Way2 = new AccelerationSet4Way(longitude + 1, latitude, vertical, yawRate);

        assertFalse(accelerationSet4Way.hashCode() == accelerationSet4Way2.hashCode());
        assertTrue(accelerationSet4Way.hashCode() == accelerationSet4Way.hashCode());
        assertTrue(accelerationSet4Way2.hashCode() == accelerationSet4Way2.hashCode());

        AccelerationSet4Way accelerationSet4Way3 = new AccelerationSet4Way(longitude, latitude, vertical, yawRate);

        assertTrue(accelerationSet4Way.hashCode() == accelerationSet4Way3.hashCode());
        assertFalse(accelerationSet4Way2.hashCode() == accelerationSet4Way3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(accelerationSet4Way.equals(accelerationSet4Way));
        assertFalse(accelerationSet4Way.equals(null));
        assertFalse(accelerationSet4Way.equals(new String()));

        int longitude = accelerationSet4Way.getLongitude().getValue();
        int latitude = accelerationSet4Way.getLatitude().getValue();
        int vertical = accelerationSet4Way.getVertical().getValue();
        int yawRate = accelerationSet4Way.getYawRate().getValue();

        // different
        AccelerationSet4Way accelerationSet4Way2 = new AccelerationSet4Way(longitude + 1, latitude + 1, vertical + 1, yawRate + 1);

        assertFalse(accelerationSet4Way.equals(accelerationSet4Way2));

        // different longitude
        accelerationSet4Way2 = new AccelerationSet4Way(longitude + 1, latitude, vertical, yawRate);

        assertFalse(accelerationSet4Way.equals(accelerationSet4Way2));

        // different latitude
        accelerationSet4Way2 = new AccelerationSet4Way(longitude, latitude + 1, vertical, yawRate);

        assertFalse(accelerationSet4Way.equals(accelerationSet4Way2));

        // different vertical
        accelerationSet4Way2 = new AccelerationSet4Way(longitude, latitude, vertical + 1, yawRate);

        assertFalse(accelerationSet4Way.equals(accelerationSet4Way2));

        // different yaw rate
        accelerationSet4Way2 = new AccelerationSet4Way(longitude, latitude, vertical, yawRate + 1);

        assertFalse(accelerationSet4Way.equals(accelerationSet4Way2));

        // same
        accelerationSet4Way2 = new AccelerationSet4Way(longitude, latitude, vertical, yawRate);

        assertTrue(accelerationSet4Way.equals(accelerationSet4Way2));
    }
}
