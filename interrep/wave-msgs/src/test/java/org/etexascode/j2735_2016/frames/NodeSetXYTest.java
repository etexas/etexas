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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node set xy frame.
 * 
 * @author ttevendale
 */
public class NodeSetXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        NodeSetXY nodes = new NodeSetXY(NodeSetXY.MIN_SET_SIZE);
        assertTrue(nodes.getNodeArray().length == NodeSetXY.MIN_SET_SIZE);

        thrown.expect(IllegalArgumentException.class);
        nodes = new NodeSetXY(NodeSetXY.MIN_SET_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        NodeSetXY nodes = new NodeSetXY(NodeSetXY.MAX_SET_SIZE);
        assertTrue(nodes.getNodeArray().length == NodeSetXY.MAX_SET_SIZE);

        thrown.expect(IllegalArgumentException.class);
        nodes = new NodeSetXY(NodeSetXY.MAX_SET_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numNodes = 51;
        NodeSetXY nodes = new NodeSetXY(numNodes);
        assertTrue(nodes.getNodeArray().length == numNodes);
    }

    @Test
    public void testEncodeUPERMin() {

        NodeSetXY nodes = new NodeSetXY(NodeSetXY.MIN_SET_SIZE);
        NodeXY node = new NodeXY(new NodeOffsetPointXY(new NodeXY22B(23, -600)));
        NodeXY node2 = new NodeXY(new NodeOffsetPointXY(new NodeXY26B(100, 3500)));
        NodeXY[] nodeArray = nodes.getNodeArray();
        nodeArray[0] = node;
        nodeArray[1] = node2;

        String listSize = "000000";
        String remainingBits = node.encodeUPER() + node2.encodeUPER();

        assertTrue((listSize + remainingBits).equals(nodes.encodeUPER()));

        nodes = new NodeSetXY(NodeSetXY.MIN_SET_SIZE);
        thrown.expect(IllegalStateException.class);
        nodes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "111101";
        String remainingBits = "";

        NodeSetXY nodes = new NodeSetXY(NodeSetXY.MAX_SET_SIZE);

        NodeXY[] nodeArray = nodes.getNodeArray();
        for (int i = 0; i < nodeArray.length; i++) {

            NodeXY node = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(-i, i)));
            nodeArray[i] = node;
            remainingBits += node.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(nodes.encodeUPER()));

        nodes = new NodeSetXY(NodeSetXY.MAX_SET_SIZE);
        thrown.expect(IllegalStateException.class);
        nodes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        NodeSetXY nodes = new NodeSetXY();
        thrown.expect(IllegalStateException.class);
        nodes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        NodeXY node = new NodeXY(new NodeOffsetPointXY(new NodeXY32B(31100, -650)));
        NodeXY node2 = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(100, 1)));
        String listSize = "000000";
        String remainingBits = node.encodeUPER() + node2.encodeUPER();

        NodeSetXY nodes = new NodeSetXY();
        nodes.decodeUPER(listSize + remainingBits);
        NodeXY[] nodeArray = nodes.getNodeArray();
        assertTrue(NodeSetXY.MIN_SET_SIZE == nodeArray.length);
        assertTrue(node.equals(nodeArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        NodeXY node = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(23, -60)));
        NodeXY node2 = new NodeXY(new NodeOffsetPointXY(new NodeXY32B(-21541, 154)));

        String listSize = "111101";
        String remainingBits = node.encodeUPER();

        for (int i = 0; i < NodeSetXY.MAX_SET_SIZE - 1; i++) {

            remainingBits += node2.encodeUPER();
        }

        NodeSetXY nodes = new NodeSetXY();
        nodes.decodeUPER(listSize + remainingBits);

        NodeXY[] nodeArray = nodes.getNodeArray();
        assertTrue(NodeSetXY.MAX_SET_SIZE == nodeArray.length);
        assertTrue(node.equals(nodeArray[0]));
        for (int i = 1; i < NodeSetXY.MAX_SET_SIZE; i++) {

            assertTrue(node2.equals(nodeArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        NodeSetXY nodes = new NodeSetXY();
        thrown.expect(IllegalArgumentException.class);
        nodes.decodeUPER("111111");
    }

    @Test
    public void testDecodeUPERLessBits() {

        NodeSetXY nodes = new NodeSetXY();
        thrown.expect(IllegalArgumentException.class);
        nodes.decodeUPER("0101");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        NodeSetXY nodes = new NodeSetXY();
        thrown.expect(IllegalArgumentException.class);
        // 011111 = 33 objects, but there's none
        nodes.decodeUPER("011111010100");
    }

    @Test
    public void testHashCode() {

        NodeSetXY nodes = new NodeSetXY(2);
        nodes.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(0, 0)));

        assertTrue(nodes.hashCode() == nodes.hashCode());

        NodeSetXY nodes2 = new NodeSetXY(3);

        assertFalse(nodes.hashCode() == nodes2.hashCode());

        nodes2 = new NodeSetXY(2);
        nodes2.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY26B(-1234, 4000)));

        assertFalse(nodes.hashCode() == nodes2.hashCode());

        nodes2.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(0, 0)));

        assertTrue(nodes.hashCode() == nodes2.hashCode());
    }

    @Test
    public void testEquals() {

        NodeSetXY nodes = new NodeSetXY(2);
        nodes.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(0, 0)));

        assertTrue(nodes.equals(nodes));
        assertFalse(nodes.equals(null));
        assertFalse(nodes.equals(new String()));

        NodeSetXY nodes2 = new NodeSetXY(3);

        assertFalse(nodes.equals(nodes2));

        nodes2 = new NodeSetXY(2);
        nodes2.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY26B(-1234, 4000)));

        assertFalse(nodes.equals(nodes2));

        nodes2.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(0, 0)));

        assertTrue(nodes.equals(nodes2));
    }
}
