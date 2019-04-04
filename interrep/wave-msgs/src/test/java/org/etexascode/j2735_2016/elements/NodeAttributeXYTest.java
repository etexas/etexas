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
package org.etexascode.j2735_2016.elements;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.NodeAttributeXY.NodeAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node attribute xy element.
 * 
 * @author ttevendale
 */
public class NodeAttributeXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.RESERVED);
        assertTrue(NodeAttribute.RESERVED.equals(attribute.getEnumeration()));

        // max value
        attribute = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);
        assertTrue(NodeAttribute.HYDRANT_PRESENT.equals(attribute.getEnumeration()));

        attribute = new NodeAttributeXY(NodeAttribute.STOP_LINE);
        assertTrue(NodeAttribute.STOP_LINE.equals(attribute.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.RESERVED);
        String encodedStatus = attribute.encodeUPER();
        assertTrue("00000".equals(encodedStatus));

        // test max
        attribute = new NodeAttributeXY(NodeAttribute.HYDRANT_PRESENT);
        encodedStatus = attribute.encodeUPER();
        assertTrue("01011".equals(encodedStatus));

        attribute = new NodeAttributeXY(NodeAttribute.ROUNDED_CAP_STYLE_A);
        encodedStatus = attribute.encodeUPER();
        assertTrue("00010".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        NodeAttributeXY attribute = new NodeAttributeXY();

        // test min
        String remainingBits = attribute.decodeUPER("00000");
        assertTrue("".equals(remainingBits));
        assertTrue(NodeAttribute.RESERVED.equals(attribute.getEnumeration()));

        // test max
        remainingBits = attribute.decodeUPER("01011");
        assertTrue("".equals(remainingBits));
        assertTrue(NodeAttribute.HYDRANT_PRESENT.equals(attribute.getEnumeration()));

        remainingBits = attribute.decodeUPER("00011");
        assertTrue("".equals(remainingBits));
        assertTrue(NodeAttribute.ROUNDED_CAP_STYLE_B.equals(attribute.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("01100");
    }

    @Test
    public void testDecodeUPERExtension() {

        NodeAttributeXY attribute = new NodeAttributeXY();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("10000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        NodeAttributeXY attribute = new NodeAttributeXY();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        NodeAttributeXY attribute = new NodeAttributeXY();
        String remainingBits = attribute.decodeUPER("0010011110");
        assertTrue("11110".equals(remainingBits));
        assertTrue(NodeAttribute.MERGE_POINT.equals(attribute.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.DIVERGE_POINT);
        NodeAttributeXY attribute2 = new NodeAttributeXY(NodeAttribute.DOWNSTREAM_STOP_LINE);

        assertFalse(attribute.hashCode() == attribute2.hashCode());
        assertTrue(attribute.hashCode() == attribute.hashCode());
        assertTrue(attribute2.hashCode() == attribute2.hashCode());

        NodeAttributeXY attribute3 = new NodeAttributeXY(attribute.getEnumeration());

        assertTrue(attribute.hashCode() == attribute3.hashCode());
        assertFalse(attribute2.hashCode() == attribute3.hashCode());
    }

    @Test
    public void testEquals() {

        NodeAttributeXY attribute = new NodeAttributeXY(NodeAttribute.DOWNSTREAM_START_NODE);

        assertFalse(attribute.equals(null));

        assertTrue(attribute.equals(attribute));

        NodeAttributeXY attribute2 = new NodeAttributeXY(NodeAttribute.CLOSED_TO_TRAFFIC);

        assertFalse(attribute.equals(new String()));
        assertFalse(attribute.equals(attribute2));

        attribute2.setEnumeration(attribute.getEnumeration());
        assertTrue(attribute.equals(attribute2));
    }
}
