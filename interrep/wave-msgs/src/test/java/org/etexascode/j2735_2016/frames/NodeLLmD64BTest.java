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

import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node longitude latitude micro degree 64b frame.
 * 
 * @author ttevendale
 */
public class NodeLLmD64BTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    NodeLLmD64B node;

    String encodedBits;

    @Before
    public void init() {

        Longitude longitude = new Longitude(181748441);
        Latitude latitude = new Latitude(-50659890);

        node = new NodeLLmD64B(longitude, latitude);
        encodedBits = longitude.encodeUPER() + latitude.encodeUPER();
    }

    @Test
    public void testConstructor() {

        Longitude longitude = new Longitude(1534840);
        Latitude latitude = new Latitude(-5345310);

        NodeLLmD64B node = new NodeLLmD64B(longitude, latitude);

        assertTrue(longitude.equals(node.getLongitude()));
        assertTrue(latitude.equals(node.getLatitude()));
    }

    @Test
    public void testConstructorNullLongitude() {

        thrown.expect(NullPointerException.class);
        new NodeLLmD64B(null, new Latitude(12896753));
    }

    @Test
    public void testConstructorNullLatitude() {

        thrown.expect(NullPointerException.class);
        new NodeLLmD64B(new Longitude(369715621), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int latitude = 428111;
        long longitude = -29635245;

        NodeLLmD64B node = new NodeLLmD64B(longitude, latitude);

        assertTrue(latitude == node.getLatitude().getValue());
        assertTrue(longitude == node.getLongitude().getValue());
    }

    @Test
    public void testSetLongitude() {

        Longitude longitude = new Longitude(44548714);

        NodeLLmD64B node = new NodeLLmD64B();
        node.setLongitude(longitude);

        assertTrue(longitude.equals(node.getLongitude()));

        thrown.expect(NullPointerException.class);
        node.setLongitude(null);
    }

    @Test
    public void testSetLongitudePrimitive() {

        long longitude = -111;

        NodeLLmD64B node = new NodeLLmD64B();
        node.setLongitude(longitude);

        assertTrue(longitude == node.getLongitude().getValue());
    }

    @Test
    public void testSetLatitude() {

        Latitude latitude = new Latitude(1518411);

        NodeLLmD64B node = new NodeLLmD64B();
        node.setLatitude(latitude);

        assertTrue(latitude.equals(node.getLatitude()));

        thrown.expect(NullPointerException.class);
        node.setLatitude(null);
    }

    @Test
    public void testSetLatitudePrimitive() {

        int latitude = 18518465;

        NodeLLmD64B node = new NodeLLmD64B();
        node.setLatitude(latitude);

        assertTrue(latitude == node.getLatitude().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(node.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        NodeLLmD64B decodedNode = new NodeLLmD64B();
        decodedNode.decodeUPER(encodedBits);
        assertTrue(node.equals(decodedNode));
    }

    @Test
    public void testHashCode() {

        long longitude = node.getLongitude().getValue();
        int latitude = node.getLatitude().getValue();

        NodeLLmD64B node2 = new NodeLLmD64B(longitude + 1, latitude + 1);

        assertFalse(node.hashCode() == node2.hashCode());
        assertTrue(node.hashCode() == node.hashCode());
        assertTrue(node2.hashCode() == node2.hashCode());

        NodeLLmD64B node3 = new NodeLLmD64B(longitude, latitude);

        assertTrue(node.hashCode() == node3.hashCode());
        assertFalse(node2.hashCode() == node3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(node.equals(node));
        assertFalse(node.equals(null));
        assertFalse(node.equals(new String()));

        long longitude = node.getLongitude().getValue();
        int latitude = node.getLatitude().getValue();

        // different
        NodeLLmD64B node2 = new NodeLLmD64B(longitude + 1, latitude + 1);
        assertFalse(node.equals(node2));

        // different x
        node2 = new NodeLLmD64B(longitude + 1, latitude);
        assertFalse(node.equals(node2));

        // different y
        node2 = new NodeLLmD64B(longitude, latitude + 1);
        assertFalse(node.equals(node2));

        // same
        node2 = new NodeLLmD64B(longitude, latitude);
        assertTrue(node.equals(node2));
    }
}
