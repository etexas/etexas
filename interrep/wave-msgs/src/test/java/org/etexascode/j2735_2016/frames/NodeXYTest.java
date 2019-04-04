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

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node xy frame.
 * 
 * @author ttevendale
 */
public class NodeXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    NodeXY node;

    @Before
    public void init() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY22B(100, 988));
        NodeAttributeSetXY attributes = new NodeAttributeSetXY();
        attributes.setDElevation(38);
        attributes.setDWidth(100);

        node = new NodeXY(delta);
        node.setAttributes(attributes);
    }

    @Test
    public void testConstructor() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY28B(8000, -88));

        NodeXY node = new NodeXY(delta);

        assertNull(node.getAttributes());
        assertTrue(delta.equals(node.getDelta()));

        thrown.expect(NullPointerException.class);
        new NodeXY(null);
    }

    @Test
    public void testSetDelta() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY20B(0, -88));

        NodeXY node = new NodeXY();
        node.setDelta(delta);

        assertTrue(delta.equals(node.getDelta()));

        thrown.expect(NullPointerException.class);
        node.setDelta(null);
    }

    @Test
    public void testEncodeUPERMin() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY22B(0, 588));

        NodeXY node = new NodeXY(delta);

        String nodeOptionals = "00";
        String remainingBits = delta.encodeUPER();
        assertTrue((nodeOptionals + remainingBits).equals(node.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY24B(1000, 1500));
        NodeAttributeSetXY attributes = new NodeAttributeSetXY();
        attributes.setDElevation(200);
        attributes.setDWidth(10);

        NodeXY node = new NodeXY(delta);
        node.setAttributes(attributes);

        String nodeOptionals = "01";
        String remainingBits = delta.encodeUPER() + attributes.encodeUPER();
        assertTrue((nodeOptionals + remainingBits).equals(node.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY24B(-1000, 2000));

        String nodeOptionals = "00";

        NodeXY node = new NodeXY();
        String remainingBits = node.decodeUPER(nodeOptionals + delta.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(node.getAttributes());
        assertTrue(delta.equals(node.getDelta()));
    }

    @Test
    public void testDecodeUPERMax() {

        NodeOffsetPointXY delta = new NodeOffsetPointXY(new NodeXY26B(1000, -3500));
        NodeAttributeSetXY attributes = new NodeAttributeSetXY();
        attributes.setDElevation(200);
        attributes.setDWidth(10);

        String nodeOptionals = "01";

        NodeXY node = new NodeXY();
        String remainingBits = node.decodeUPER(nodeOptionals + delta.encodeUPER() + attributes.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(delta.equals(node.getDelta()));
        assertTrue(attributes.equals(node.getAttributes()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String nodeOptionals = "10";

        NodeXY node = new NodeXY();
        thrown.expect(IllegalArgumentException.class);
        node.decodeUPER(nodeOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String nodeOptionals = "0";

        NodeXY node = new NodeXY();
        thrown.expect(IllegalArgumentException.class);
        node.decodeUPER(nodeOptionals);
    }

    @Test
    public void testHashCode() {

        NodeOffsetPointXY offset = node.getDelta();
        NodeAttributeSetXY attributes = node.getAttributes();

        NodeOffsetPointXY diffOffset = new NodeOffsetPointXY(new NodeXY32B(offset.getNodeXY22B().getX().getValue() + 1, offset.getNodeXY22B().getY().getValue() + 1));
        NodeAttributeSetXY diffAttributes = new NodeAttributeSetXY();
        diffAttributes.setDElevation(node.getAttributes().getDElevation().getValue() + 1);
        diffAttributes.setDWidth(node.getAttributes().getDWidth().getValue() + 1);

        NodeXY node2 = new NodeXY(diffOffset);
        node2.setAttributes(diffAttributes);

        assertFalse(node.hashCode() == node2.hashCode());
        assertTrue(node.hashCode() == node.hashCode());
        assertTrue(node2.hashCode() == node2.hashCode());

        NodeXY node3 = new NodeXY(offset);
        node3.setAttributes(attributes);

        assertTrue(node.hashCode() == node3.hashCode());
        assertFalse(node2.hashCode() == node3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(node.equals(node));
        assertFalse(node.equals(null));
        assertFalse(node.equals(new String()));

        NodeOffsetPointXY offset = node.getDelta();
        NodeAttributeSetXY attributes = node.getAttributes();

        NodeOffsetPointXY diffOffset = new NodeOffsetPointXY(new NodeXY32B(offset.getNodeXY22B().getX().getValue() + 1, offset.getNodeXY22B().getY().getValue() + 1));
        NodeAttributeSetXY diffAttributes = new NodeAttributeSetXY();
        diffAttributes.setDElevation(node.getAttributes().getDElevation().getValue() + 1);
        diffAttributes.setDWidth(node.getAttributes().getDWidth().getValue() + 1);

        // different
        NodeXY node2 = new NodeXY(diffOffset);
        node2.setAttributes(diffAttributes);

        assertFalse(node.equals(node2));

        // different offset
        node2 = new NodeXY(diffOffset);
        node2.setAttributes(attributes);

        assertFalse(node.equals(node2));

        // different attributes
        node2 = new NodeXY(offset);
        node2.setAttributes(diffAttributes);

        assertFalse(node.equals(node2));

        // same
        node2 = new NodeXY(offset);
        node2.setAttributes(attributes);

        assertTrue(node.equals(node2));
    }
}
