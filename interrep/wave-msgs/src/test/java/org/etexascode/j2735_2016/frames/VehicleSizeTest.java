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

import org.etexascode.j2735_2016.elements.VehicleLength;
import org.etexascode.j2735_2016.elements.VehicleWidth;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the vehicle size frame.
 * 
 * @author ttevendale
 */
public class VehicleSizeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    VehicleSize vehicleSize;

    String encodedBits;

    @Before
    public void init() {

        VehicleWidth width = new VehicleWidth(548);
        VehicleLength length = new VehicleLength(84);

        vehicleSize = new VehicleSize(width, length);
        encodedBits = width.encodeUPER() + length.encodeUPER();
    }

    @Test
    public void testConstructor() {

        VehicleWidth width = new VehicleWidth(50);
        VehicleLength length = new VehicleLength(20);

        VehicleSize vehicleSize = new VehicleSize(width, length);

        assertTrue(width.equals(vehicleSize.getWidth()));
        assertTrue(length.equals(vehicleSize.getLength()));
    }

    @Test
    public void testConstructorNullWidth() {

        thrown.expect(NullPointerException.class);
        new VehicleSize(null, new VehicleLength());
    }

    @Test
    public void testConstructorNullLength() {

        thrown.expect(NullPointerException.class);
        new VehicleSize(new VehicleWidth(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int width = 500;
        int length = 200;

        VehicleSize vehicleSize = new VehicleSize(width, length);

        assertTrue(width == vehicleSize.getWidth().getValue());
        assertTrue(length == vehicleSize.getLength().getValue());
    }

    @Test
    public void testSetWidth() {

        VehicleWidth width = new VehicleWidth(283);

        VehicleSize size = new VehicleSize();
        size.setWidth(width);

        assertTrue(width.equals(size.getWidth()));

        thrown.expect(NullPointerException.class);
        size.setWidth(null);
    }

    @Test
    public void testSetWidthPrimitive() {

        int width = 12;

        VehicleSize vehicleSize = new VehicleSize();
        vehicleSize.setWidth(width);

        assertTrue(width == vehicleSize.getWidth().getValue());
    }

    @Test
    public void testSetLength() {

        VehicleLength length = new VehicleLength(445);

        VehicleSize size = new VehicleSize();
        size.setLength(length);

        assertTrue(length.equals(size.getLength()));

        thrown.expect(NullPointerException.class);
        size.setWidth(null);
    }

    @Test
    public void testSetLengthPrimitive() {

        int length = 800;

        VehicleSize vehicleSize = new VehicleSize();
        vehicleSize.setLength(length);

        assertTrue(length == vehicleSize.getLength().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equals(vehicleSize.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        VehicleSize decodedVehicleSize = new VehicleSize();
        decodedVehicleSize.decodeUPER(encodedBits);

        assertTrue(vehicleSize.equals(decodedVehicleSize));
    }

    @Test
    public void testHashCode() {

        int width = vehicleSize.getWidth().getValue();
        int length = vehicleSize.getLength().getValue();

        VehicleSize vehicleSize2 = new VehicleSize(width + 1, length + 1);

        assertFalse(vehicleSize.hashCode() == vehicleSize2.hashCode());
        assertTrue(vehicleSize.hashCode() == vehicleSize.hashCode());
        assertTrue(vehicleSize2.hashCode() == vehicleSize2.hashCode());

        VehicleSize vehicleSize3 = new VehicleSize(width, length);

        assertTrue(vehicleSize.hashCode() == vehicleSize3.hashCode());
        assertFalse(vehicleSize2.hashCode() == vehicleSize3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(vehicleSize.equals(vehicleSize));
        assertFalse(vehicleSize.equals(null));
        assertFalse(vehicleSize.equals(new String()));

        int width = vehicleSize.getWidth().getValue();
        int length = vehicleSize.getLength().getValue();

        // different
        VehicleSize vehicleSize2 = new VehicleSize(width + 1, length + 1);

        assertFalse(vehicleSize.equals(vehicleSize2));

        // different width
        vehicleSize2 = new VehicleSize(width + 1, length);

        assertFalse(vehicleSize.equals(vehicleSize2));

        // different length
        vehicleSize2 = new VehicleSize(width, length + 1);

        assertFalse(vehicleSize.equals(vehicleSize2));

        // same
        vehicleSize2 = new VehicleSize(width, length);

        assertTrue(vehicleSize.equals(vehicleSize2));
    }
}
