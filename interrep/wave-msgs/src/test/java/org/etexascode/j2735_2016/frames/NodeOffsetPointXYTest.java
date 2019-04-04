/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node offset point xy frame (Choice).
 * 
 * @author ttevendale
 */
public class NodeOffsetPointXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorNodeXY20B() {

        NodeXY20B node = new NodeXY20B(151, 38);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY20B()));
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY20B)null);
    }

    @Test
    public void testConstructorNodeXY22B() {

        NodeXY22B node = new NodeXY22B(818, 1000);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY22B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY22B)null);
    }

    @Test
    public void testConstructorNodeXY24B() {

        NodeXY24B node = new NodeXY24B(1500, 1212);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY24B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY24B)null);
    }

    @Test
    public void testConstructorNodeXY26B() {

        NodeXY26B node = new NodeXY26B(3000, 2555);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY26B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY26B)null);
    }

    @Test
    public void testConstructorNodeXY28B() {

        NodeXY28B node = new NodeXY28B(5000, 7676);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY28B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY28B)null);
    }

    @Test
    public void testConstructorNodeXY32B() {

        NodeXY32B node = new NodeXY32B(10000, -32767);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY32B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeXY32B)null);
    }

    @Test
    public void testConstructorNodeLLmD64B() {

        NodeLLmD64B node = new NodeLLmD64B(101871000, -84874);

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        assertTrue(node.equals(nodeOffsetPointXY.getNodeLatLong()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());

        thrown.expect(NullPointerException.class);
        new NodeOffsetPointXY((NodeLLmD64B)null);
    }

    @Test
    public void testEncodeUPERNodeXY20B() {

        NodeXY20B node = new NodeXY20B(-51, 348);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "000";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeXY22B() {

        NodeXY22B node = new NodeXY22B(-600, 999);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "001";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeXY24B() {

        NodeXY24B node = new NodeXY24B(-1141, 2000);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "010";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeXY26B() {

        NodeXY26B node = new NodeXY26B(2999, -3504);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "011";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeXY28B() {

        NodeXY28B node = new NodeXY28B(6666, -7874);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "100";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeXY32B() {

        NodeXY32B node = new NodeXY32B(-21514, 21147);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "101";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERNodeLLmD64B() {

        NodeLLmD64B node = new NodeLLmD64B(-541871, 2487454);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(node);

        String nodeOffsetPointXYChoice = "110";
        String remainingBits = node.encodeUPER();

        assertTrue((nodeOffsetPointXYChoice + remainingBits).equals(nodeOffsetPointXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        thrown.expect(IllegalStateException.class);
        nodeOffsetPointXY.encodeUPER();
    }

    @Test
    public void testDecodeUPERNodeXY20B() {

        NodeXY20B node = new NodeXY20B(481, -500);

        String nodeOffsetPointXYChoice = "000";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY20B()));
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeXY22B() {

        NodeXY22B node = new NodeXY22B(999, -684);

        String nodeOffsetPointXYChoice = "001";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY22B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeXY24B() {

        NodeXY24B node = new NodeXY24B(-1570, 1487);

        String nodeOffsetPointXYChoice = "010";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY24B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeXY26B() {

        NodeXY26B node = new NodeXY26B(3131, -2051);

        String nodeOffsetPointXYChoice = "011";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY26B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeXY28B() {

        NodeXY28B node = new NodeXY28B(5999, -6999);

        String nodeOffsetPointXYChoice = "100";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY28B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeXY32B() {

        NodeXY32B node = new NodeXY32B(-12345, -29876);

        String nodeOffsetPointXYChoice = "101";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeXY32B()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeLatLong());
    }

    @Test
    public void testDecodeUPERNodeLLmD64B() {

        NodeLLmD64B node = new NodeLLmD64B(4185876, -507867960);

        String nodeOffsetPointXYChoice = "110";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        nodeOffsetPointXY.decodeUPER(nodeOffsetPointXYChoice + node.encodeUPER());

        assertTrue(node.equals(nodeOffsetPointXY.getNodeLatLong()));
        assertNull(nodeOffsetPointXY.getNodeXY20B());
        assertNull(nodeOffsetPointXY.getNodeXY22B());
        assertNull(nodeOffsetPointXY.getNodeXY24B());
        assertNull(nodeOffsetPointXY.getNodeXY26B());
        assertNull(nodeOffsetPointXY.getNodeXY28B());
        assertNull(nodeOffsetPointXY.getNodeXY32B());
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String choice = "111";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        thrown.expect(IllegalArgumentException.class);
        nodeOffsetPointXY.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String choice = "01";

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY();
        thrown.expect(IllegalArgumentException.class);
        nodeOffsetPointXY.decodeUPER(choice);
    }

    @Test
    public void testHashCode() {

        NodeXY20B nodeXY = new NodeXY20B(444, 82);
        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(nodeXY);

        NodeOffsetPointXY nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY20B(nodeXY.getX().getValue() + 1, nodeXY.getY().getValue() + 1));

        assertFalse(nodeOffsetPointXY.hashCode() == nodeOffsetPointXY2.hashCode());
        assertTrue(nodeOffsetPointXY.hashCode() == nodeOffsetPointXY.hashCode());
        assertTrue(nodeOffsetPointXY2.hashCode() == nodeOffsetPointXY2.hashCode());

        NodeOffsetPointXY nodeOffsetPointXY3 = new NodeOffsetPointXY(new NodeXY20B(nodeXY.getX().getValue(), nodeXY.getY().getValue()));

        assertTrue(nodeOffsetPointXY.hashCode() == nodeOffsetPointXY3.hashCode());
        assertFalse(nodeOffsetPointXY2.hashCode() == nodeOffsetPointXY3.hashCode());
    }

    @Test
    public void testEquals() {

        NodeOffsetPointXY nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY20B(-23, 78));

        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY));
        assertFalse(nodeOffsetPointXY.equals(null));
        assertFalse(nodeOffsetPointXY.equals(new String()));

        // different node xy 20b
        NodeOffsetPointXY nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY20B(nodeOffsetPointXY.getNodeXY20B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY20B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 20b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY20B(nodeOffsetPointXY.getNodeXY20B().getX().getValue(), nodeOffsetPointXY.getNodeXY20B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node xy 22b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY22B(547, 1000));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY22B(nodeOffsetPointXY.getNodeXY22B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY22B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 22b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY22B(nodeOffsetPointXY.getNodeXY22B().getX().getValue(), nodeOffsetPointXY.getNodeXY22B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node xy 24b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY24B(1222, -1234));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY24B(nodeOffsetPointXY.getNodeXY24B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY24B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 24b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY24B(nodeOffsetPointXY.getNodeXY24B().getX().getValue(), nodeOffsetPointXY.getNodeXY24B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node xy 26b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY26B(2555, -3555));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY26B(nodeOffsetPointXY.getNodeXY26B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY26B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 26b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY26B(nodeOffsetPointXY.getNodeXY26B().getX().getValue(), nodeOffsetPointXY.getNodeXY26B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node xy 28b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY28B(8000, -7999));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY28B(nodeOffsetPointXY.getNodeXY28B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY28B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 28b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY28B(nodeOffsetPointXY.getNodeXY28B().getX().getValue(), nodeOffsetPointXY.getNodeXY28B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node xy 32b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeXY32B(12487, 25487));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY32B(nodeOffsetPointXY.getNodeXY32B().getX().getValue() + 1, nodeOffsetPointXY.getNodeXY32B().getY().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node xy 32b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeXY32B(nodeOffsetPointXY.getNodeXY32B().getX().getValue(), nodeOffsetPointXY.getNodeXY32B().getY().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // different node longitude latitude micro degree 64b
        nodeOffsetPointXY = new NodeOffsetPointXY(new NodeLLmD64B(12546487, -25445287));
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeLLmD64B(nodeOffsetPointXY.getNodeLatLong().getLongitude().getValue() + 1, nodeOffsetPointXY.getNodeLatLong().getLatitude().getValue() + 1));
        assertFalse(nodeOffsetPointXY.equals(nodeOffsetPointXY2));

        // same node longitude latitude micro degree 64b
        nodeOffsetPointXY2 = new NodeOffsetPointXY(new NodeLLmD64B(nodeOffsetPointXY.getNodeLatLong().getLongitude().getValue(), nodeOffsetPointXY.getNodeLatLong().getLatitude().getValue()));
        assertTrue(nodeOffsetPointXY.equals(nodeOffsetPointXY2));
    }
}
