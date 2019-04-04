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

import org.etexascode.j2735_2016.elements.OffsetB14;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node xy 28b frame.
 * 
 * @author ttevendale
 */
public class NodeXY28BTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    NodeXY28B node;

    String encodedBits;

    @Before
    public void init() {

        OffsetB14 x = new OffsetB14(-8000);
        OffsetB14 y = new OffsetB14(8000);

        node = new NodeXY28B(x, y);
        encodedBits = x.encodeUPER() + y.encodeUPER();
    }

    @Test
    public void testConstructor() {

        OffsetB14 x = new OffsetB14(5114);
        OffsetB14 y = new OffsetB14(-6887);

        NodeXY28B node = new NodeXY28B(x, y);

        assertTrue(x.equals(node.getX()));
        assertTrue(y.equals(node.getY()));
    }

    @Test
    public void testConstructorNullX() {

        thrown.expect(NullPointerException.class);
        new NodeXY28B(null, new OffsetB14(4321));
    }

    @Test
    public void testConstructorNullY() {

        thrown.expect(NullPointerException.class);
        new NodeXY28B(new OffsetB14(4321), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int x = 4444;
        int y = -4111;

        NodeXY28B node = new NodeXY28B(x, y);

        assertTrue(x == node.getX().getValue());
        assertTrue(y == node.getY().getValue());
    }

    @Test
    public void testSetX() {

        OffsetB14 x = new OffsetB14(8181);

        NodeXY28B node = new NodeXY28B();
        node.setX(x);

        assertTrue(x.equals(node.getX()));

        thrown.expect(NullPointerException.class);
        node.setX(null);
    }

    @Test
    public void testSetXPrimitive() {

        int x = -7999;

        NodeXY28B node = new NodeXY28B();
        node.setX(x);

        assertTrue(x == node.getX().getValue());
    }

    @Test
    public void testSetY() {

        OffsetB14 y = new OffsetB14(6999);

        NodeXY28B node = new NodeXY28B();
        node.setY(y);

        assertTrue(y.equals(node.getY()));

        thrown.expect(NullPointerException.class);
        node.setY(null);
    }

    @Test
    public void testSetYPrimitive() {

        int y = 8001;

        NodeXY28B node = new NodeXY28B();
        node.setY(y);

        assertTrue(y == node.getY().getValue());
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(node.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        NodeXY28B decodedNode = new NodeXY28B();
        decodedNode.decodeUPER(encodedBits);
        assertTrue(node.equals(decodedNode));
    }

    @Test
    public void testHashCode() {

        int x = node.getX().getValue();
        int y = node.getY().getValue();

        NodeXY28B node2 = new NodeXY28B(x + 1, y + 1);

        assertFalse(node.hashCode() == node2.hashCode());
        assertTrue(node.hashCode() == node.hashCode());
        assertTrue(node2.hashCode() == node2.hashCode());

        NodeXY28B node3 = new NodeXY28B(x, y);

        assertTrue(node.hashCode() == node3.hashCode());
        assertFalse(node2.hashCode() == node3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(node.equals(node));
        assertFalse(node.equals(null));
        assertFalse(node.equals(new String()));

        int x = node.getX().getValue();
        int y = node.getY().getValue();

        // different
        NodeXY28B node2 = new NodeXY28B(x + 1, y + 1);
        assertFalse(node.equals(node2));

        // different x
        node2 = new NodeXY28B(x + 1, y);
        assertFalse(node.equals(node2));

        // different y
        node2 = new NodeXY28B(x, y + 1);
        assertFalse(node.equals(node2));

        // same
        node2 = new NodeXY28B(x, y);
        assertTrue(node.equals(node2));
    }
}
