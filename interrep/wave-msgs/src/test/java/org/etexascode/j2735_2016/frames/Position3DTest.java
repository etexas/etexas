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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the position 3d frame.
 * 
 * @author ttevendale
 */
public class Position3DTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    Position3D position;

    @Before
    public void init() {

        Latitude latitude = new Latitude(382329);
        Longitude longitude = new Longitude(-232323);
        Elevation elevation = new Elevation(500);

        position = new Position3D(latitude, longitude);
        position.setElevation(elevation);
    }

    @Test
    public void testConstructor() {

        Latitude latitude = new Latitude(382329);
        Longitude longitude = new Longitude(-232323);

        Position3D position = new Position3D(latitude, longitude);

        assertNull(position.getElevation());
        assertTrue(latitude.equals(position.getLatitude()));
        assertTrue(longitude.equals(position.getLongitude()));
    }

    @Test
    public void testConstructorNullLatitude() {

        thrown.expect(NullPointerException.class);
        new Position3D(null, new Longitude());
    }

    @Test
    public void testConstructorNullLongitude() {

        thrown.expect(NullPointerException.class);
        new Position3D(new Latitude(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int latitude = 123456;
        long longitude = -115481;

        Position3D position = new Position3D(latitude, longitude);

        assertNull(position.getElevation());
        assertTrue(latitude == position.getLatitude().getValue());
        assertTrue(longitude == position.getLongitude().getValue());
    }

    @Test
    public void testSetLatitude() {

        Latitude latitude = new Latitude(11111);

        Position3D position = new Position3D();
        position.setLatitude(latitude);

        assertTrue(latitude.equals(position.getLatitude()));

        thrown.expect(NullPointerException.class);
        position.setLatitude(null);
    }

    @Test
    public void testSetLatitudePrimitive() {

        int latitude = 99987;

        Position3D position = new Position3D();
        position.setLatitude(latitude);

        assertTrue(latitude == position.getLatitude().getValue());
    }

    @Test
    public void testSetLongitude() {

        Longitude longitude = new Longitude(-8987789);

        Position3D position = new Position3D();
        position.setLongitude(longitude);

        assertTrue(longitude.equals(position.getLongitude()));

        thrown.expect(NullPointerException.class);
        position.setLongitude(null);
    }

    @Test
    public void testSetLongitudePrimitive() {

        int longitude = -5;

        Position3D position = new Position3D();
        position.setLongitude(longitude);

        assertTrue(longitude == position.getLongitude().getValue());
    }

    @Test
    public void testSetElevationPrimitive() {

        int elevation = 321;

        Position3D position = new Position3D();
        position.setElevation(elevation);

        assertTrue(elevation == position.getElevation().getValue());

        elevation = 123;

        position.setElevation(elevation);

        assertTrue(elevation == position.getElevation().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        Latitude latitude = new Latitude(456789);
        Longitude longitude = new Longitude(-369258);

        Position3D position = new Position3D(latitude, longitude);

        String position3dOptionals = "000";
        String remainingBits = latitude.encodeUPER() + longitude.encodeUPER();
        assertTrue((position3dOptionals + remainingBits).equals(position.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        Latitude latitude = new Latitude(147654);
        Longitude longitude = new Longitude(8795157);
        Elevation elevation = new Elevation(123);

        Position3D position = new Position3D(latitude, longitude);
        position.setElevation(elevation);

        String position3dOptionals = "010";
        String remainingBits = latitude.encodeUPER() + longitude.encodeUPER() + elevation.encodeUPER();
        assertTrue((position3dOptionals + remainingBits).equals(position.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        Latitude latitude = new Latitude(829);
        Longitude longitude = new Longitude(529219721);

        String position3dOptionals = "000";

        Position3D position = new Position3D();
        String remainingBits = position.decodeUPER(position3dOptionals + latitude.encodeUPER() + longitude.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(position.getElevation());
        assertTrue(latitude.equals(position.getLatitude()));
        assertTrue(longitude.equals(position.getLongitude()));
    }

    @Test
    public void testDecodeUPERMax() {

        Latitude latitude = new Latitude(-5184);
        Longitude longitude = new Longitude(-8818);
        Elevation elevation = new Elevation(-50);

        String position3dOptionals = "010";

        Position3D position = new Position3D();
        String remainingBits = position.decodeUPER(position3dOptionals + latitude.encodeUPER() + longitude.encodeUPER() + elevation.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(latitude.equals(position.getLatitude()));
        assertTrue(longitude.equals(position.getLongitude()));
        assertTrue(elevation.equals(position.getElevation()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String position3dOptionals = "100";

        Position3D position = new Position3D();
        thrown.expect(IllegalArgumentException.class);
        position.decodeUPER(position3dOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String position3dOptionals = "001";

        Position3D position = new Position3D();
        thrown.expect(IllegalArgumentException.class);
        position.decodeUPER(position3dOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String position3dOptionals = "00";

        Position3D position = new Position3D();
        thrown.expect(IllegalArgumentException.class);
        position.decodeUPER(position3dOptionals);
    }

    @Test
    public void testHashCode() {

        int latitude = position.getLatitude().getValue();
        long longitude = position.getLongitude().getValue();
        int elevation = position.getElevation().getValue();

        Position3D position2 = new Position3D(latitude + 1, longitude + 1);
        position2.setElevation(elevation + 1);

        assertFalse(position.hashCode() == position2.hashCode());
        assertTrue(position.hashCode() == position.hashCode());
        assertTrue(position2.hashCode() == position2.hashCode());

        Position3D position3 = new Position3D(latitude, longitude);
        position3.setElevation(elevation);

        assertTrue(position.hashCode() == position3.hashCode());
        assertFalse(position2.hashCode() == position3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(position.equals(position));
        assertFalse(position.equals(null));
        assertFalse(position.equals(new String()));

        int latitude = position.getLatitude().getValue();
        long longitude = position.getLongitude().getValue();
        int elevation = position.getElevation().getValue();

        // different
        Position3D position2 = new Position3D(latitude + 1, longitude + 1);
        position2.setElevation(elevation + 1);

        assertFalse(position.equals(position2));

        // different latitude
        position2 = new Position3D(latitude + 1, longitude);
        position2.setElevation(elevation);

        assertFalse(position.equals(position2));

        // different longitude
        position2 = new Position3D(latitude, longitude + 1);
        position2.setElevation(elevation);

        assertFalse(position.equals(position2));

        // different elevation
        position2 = new Position3D(latitude, longitude);
        position2.setElevation(elevation + 1);

        assertFalse(position.equals(position2));

        // same
        position2 = new Position3D(latitude, longitude);
        position2.setElevation(elevation);

        assertTrue(position.equals(position2));
    }
}
