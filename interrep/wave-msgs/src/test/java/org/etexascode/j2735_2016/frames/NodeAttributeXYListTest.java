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

import org.etexascode.j2735_2016.elements.NodeAttributeXY;
import org.etexascode.j2735_2016.elements.NodeAttributeXY.NodeAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node attribute xy list frame.
 * 
 * @author ttevendale
 */
public class NodeAttributeXYListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MIN_LIST_SIZE);
        assertTrue(nodeAttributes.getAttributeArray().length == NodeAttributeXYList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MAX_LIST_SIZE);
        assertTrue(nodeAttributes.getAttributeArray().length == NodeAttributeXYList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numAttributes = 4;
        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(numAttributes);
        assertTrue(nodeAttributes.getAttributeArray().length == numAttributes);
    }

    @Test
    public void testEncodeUPERMin() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MIN_LIST_SIZE);
        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.CLOSED_TO_TRAFFIC);
        NodeAttributeXY[] attributeArray = nodeAttributes.getAttributeArray();
        attributeArray[0] = attribute;

        String listSize = "000";
        String remainingBits = attribute.encodeUPER();

        assertTrue((listSize + remainingBits).equals(nodeAttributes.encodeUPER()));

        nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        nodeAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "111";
        String remainingBits = "";

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MAX_LIST_SIZE);

        NodeAttributeXY[] attributeArray = nodeAttributes.getAttributeArray();
        for (int i = 0; i < attributeArray.length; i++) {

            NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.CURB_PRESENT_AT_STEP_OFF);
            attributeArray[i] = attribute;
            remainingBits += attribute.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(nodeAttributes.encodeUPER()));

        nodeAttributes = new NodeAttributeXYList(NodeAttributeXYList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        nodeAttributes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList();
        thrown.expect(IllegalStateException.class);
        nodeAttributes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.DIVERGE_POINT);
        String listSize = "000";
        String remainingBits = attribute.encodeUPER();

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList();
        nodeAttributes.decodeUPER(listSize + remainingBits);
        NodeAttributeXY[] attributeArray = nodeAttributes.getAttributeArray();
        assertTrue(NodeAttributeXYList.MIN_LIST_SIZE == attributeArray.length);
        assertTrue(attribute.equals(attributeArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.DOWNSTREAM_START_NODE);
        NodeAttributeXY attribute2 = new NodeAttributeXY(NodeAttribute.DOWNSTREAM_STOP_LINE);

        String listSize = "111";
        String remainingBits = attribute.encodeUPER();

        for (int i = 0; i < NodeAttributeXYList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += attribute2.encodeUPER();
        }

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList();
        nodeAttributes.decodeUPER(listSize + remainingBits);

        NodeAttributeXY[] attributeArray = nodeAttributes.getAttributeArray();
        assertTrue(NodeAttributeXYList.MAX_LIST_SIZE == attributeArray.length);
        assertTrue(attribute.equals(attributeArray[0]));
        for (int i = 1; i < NodeAttributeXYList.MAX_LIST_SIZE; i++) {

            assertTrue(attribute2.equals(attributeArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList();
        thrown.expect(IllegalArgumentException.class);
        nodeAttributes.decodeUPER("01");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList();
        thrown.expect(IllegalArgumentException.class);
        // 111 = 8 objects, but there's none
        nodeAttributes.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(1);
        nodeAttributes.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);

        assertTrue(nodeAttributes.hashCode() == nodeAttributes.hashCode());

        NodeAttributeXYList nodeAttributes2 = new NodeAttributeXYList(2);

        assertFalse(nodeAttributes.hashCode() == nodeAttributes2.hashCode());

        nodeAttributes2 = new NodeAttributeXYList(1);
        nodeAttributes2.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.MERGE_POINT);

        assertFalse(nodeAttributes.hashCode() == nodeAttributes2.hashCode());

        nodeAttributes2.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);

        assertTrue(nodeAttributes.hashCode() == nodeAttributes2.hashCode());
    }

    @Test
    public void testEquals() {

        NodeAttributeXYList nodeAttributes = new NodeAttributeXYList(1);
        nodeAttributes.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);

        assertTrue(nodeAttributes.equals(nodeAttributes));
        assertFalse(nodeAttributes.equals(null));
        assertFalse(nodeAttributes.equals(new String()));

        NodeAttributeXYList nodeAttributes2 = new NodeAttributeXYList(2);

        assertFalse(nodeAttributes.equals(nodeAttributes2));

        nodeAttributes2 = new NodeAttributeXYList(1);
        nodeAttributes2.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.MERGE_POINT);

        assertFalse(nodeAttributes.equals(nodeAttributes2));

        nodeAttributes2.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);

        assertTrue(nodeAttributes.equals(nodeAttributes2));
    }
}
