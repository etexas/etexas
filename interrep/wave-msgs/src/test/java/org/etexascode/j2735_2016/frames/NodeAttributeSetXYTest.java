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

import org.etexascode.j2735_2016.elements.DeltaAngle;
import org.etexascode.j2735_2016.elements.NodeAttributeXY;
import org.etexascode.j2735_2016.elements.NodeAttributeXY.NodeAttribute;
import org.etexascode.j2735_2016.elements.OffsetB10;
import org.etexascode.j2735_2016.elements.SegmentAttributeXY;
import org.etexascode.j2735_2016.elements.SegmentAttributeXY.SegmentAttribute;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node attribute set xy frame.
 * 
 * @author ttevendale
 */
public class NodeAttributeSetXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    NodeAttributeSetXY nodeAttributeSetXY;

    @Before
    public void init() {

        NodeAttributeXYList localNode = new NodeAttributeXYList(1);
        localNode.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.DIVERGE_POINT);

        SegmentAttributeXYList disabled = new SegmentAttributeXYList(1);
        disabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.BIKE_BOX_IN_FRONT);

        SegmentAttributeXYList enabled = new SegmentAttributeXYList(1);
        enabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.COST_TO_PARK);

        LaneDataAttributeList data = new LaneDataAttributeList(1);
        data.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(1));

        OffsetB10 dWidth = new OffsetB10(111);
        OffsetB10 dElevation = new OffsetB10(5);

        nodeAttributeSetXY = new NodeAttributeSetXY();
        nodeAttributeSetXY.setLocalNode(localNode);
        nodeAttributeSetXY.setDisabled(disabled);
        nodeAttributeSetXY.setEnabled(enabled);
        nodeAttributeSetXY.setData(data);
        nodeAttributeSetXY.setDWidth(dWidth);
        nodeAttributeSetXY.setDElevation(dElevation);
    }

    @Test
    public void testSetDWidthPrimitive() {

        int dWidth = 12;

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        nodeAttributeSetXY.setDWidth(dWidth);

        assertTrue(dWidth == nodeAttributeSetXY.getDWidth().getValue());

        dWidth = 44;

        nodeAttributeSetXY.setDWidth(dWidth);

        assertTrue(dWidth == nodeAttributeSetXY.getDWidth().getValue());
    }

    @Test
    public void testSetDElevationPrimitive() {

        int dElevation = 500;

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        nodeAttributeSetXY.setDElevation(dElevation);

        assertTrue(dElevation == nodeAttributeSetXY.getDElevation().getValue());

        dElevation = 100;

        nodeAttributeSetXY.setDElevation(dElevation);

        assertTrue(dElevation == nodeAttributeSetXY.getDElevation().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();

        String nodeAttributeSetXYOptionals = "00000000";
        assertTrue(nodeAttributeSetXYOptionals.equals(nodeAttributeSetXY.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        NodeAttributeXYList localNode = new NodeAttributeXYList(1);
        localNode.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.CLOSED_TO_TRAFFIC);

        SegmentAttributeXYList disabled = new SegmentAttributeXYList(1);
        disabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADAPTIVE_TIMING_PRESENT);

        SegmentAttributeXYList enabled = new SegmentAttributeXYList(1);
        enabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_RIGHT);

        LaneDataAttributeList data = new LaneDataAttributeList(1);
        data.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(10));

        OffsetB10 dWidth = new OffsetB10(5);
        OffsetB10 dElevation = new OffsetB10(121);

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        nodeAttributeSetXY.setLocalNode(localNode);
        nodeAttributeSetXY.setDisabled(disabled);
        nodeAttributeSetXY.setEnabled(enabled);
        nodeAttributeSetXY.setData(data);
        nodeAttributeSetXY.setDWidth(dWidth);
        nodeAttributeSetXY.setDElevation(dElevation);

        String nodeAttributeSetXYOptionals = "01111110";
        String remainingBits = localNode.encodeUPER() + disabled.encodeUPER() + enabled.encodeUPER() + data.encodeUPER() + dWidth.encodeUPER() + dElevation.encodeUPER();
        assertTrue((nodeAttributeSetXYOptionals + remainingBits).equals(nodeAttributeSetXY.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        String nodeAttributeSetXYOptionals = "00000000";

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        String remainingBits = nodeAttributeSetXY.decodeUPER(nodeAttributeSetXYOptionals);
        assertTrue("".equals(remainingBits));

        assertNull(nodeAttributeSetXY.getLocalNode());
        assertNull(nodeAttributeSetXY.getDisabled());
        assertNull(nodeAttributeSetXY.getEnabled());
        assertNull(nodeAttributeSetXY.getData());
        assertNull(nodeAttributeSetXY.getDWidth());
        assertNull(nodeAttributeSetXY.getDElevation());
    }

    @Test
    public void testDecodeUPERMax() {

        NodeAttributeXYList localNode = new NodeAttributeXYList(1);
        localNode.getAttributeArray()[0] = new NodeAttributeXY(NodeAttribute.CLOSED_TO_TRAFFIC);

        SegmentAttributeXYList disabled = new SegmentAttributeXYList(1);
        disabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADAPTIVE_TIMING_PRESENT);

        SegmentAttributeXYList enabled = new SegmentAttributeXYList(1);
        enabled.getAttributeArray()[0] = new SegmentAttributeXY(SegmentAttribute.ADJACENT_PARKING_ON_RIGHT);

        LaneDataAttributeList data = new LaneDataAttributeList(1);
        data.getAttributeArray()[0] = new LaneDataAttribute(new DeltaAngle(10));

        OffsetB10 dWidth = new OffsetB10(5);
        OffsetB10 dElevation = new OffsetB10(121);

        String nodeAttributeSetXYOptionals = "01111110";

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        String remainingBits = nodeAttributeSetXY
                .decodeUPER(nodeAttributeSetXYOptionals + localNode.encodeUPER() + disabled.encodeUPER() + enabled.encodeUPER() + data.encodeUPER() + dWidth.encodeUPER() + dElevation.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(localNode.equals(nodeAttributeSetXY.getLocalNode()));
        assertTrue(disabled.equals(nodeAttributeSetXY.getDisabled()));
        assertTrue(enabled.equals(nodeAttributeSetXY.getEnabled()));
        assertTrue(data.equals(nodeAttributeSetXY.getData()));
        assertTrue(dWidth.equals(nodeAttributeSetXY.getDWidth()));
        assertTrue(dElevation.equals(nodeAttributeSetXY.getDElevation()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String nodeAttributeSetXYOptionals = "10000000";

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        thrown.expect(IllegalArgumentException.class);
        nodeAttributeSetXY.decodeUPER(nodeAttributeSetXYOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String nodeAttributeSetXYOptionals = "00100111";

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        thrown.expect(IllegalArgumentException.class);
        nodeAttributeSetXY.decodeUPER(nodeAttributeSetXYOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String nodeAttributeSetXYOptionals = "0110010";

        NodeAttributeSetXY nodeAttributeSetXY = new NodeAttributeSetXY();
        thrown.expect(IllegalArgumentException.class);
        nodeAttributeSetXY.decodeUPER(nodeAttributeSetXYOptionals);
    }

    @Test
    public void testHashCode() {

        NodeAttributeXYList localNode = nodeAttributeSetXY.getLocalNode();
        SegmentAttributeXYList disabled = nodeAttributeSetXY.getDisabled();
        SegmentAttributeXYList enabled = nodeAttributeSetXY.getEnabled();
        LaneDataAttributeList data = nodeAttributeSetXY.getData();
        int dWidth = nodeAttributeSetXY.getDWidth().getValue();
        int dElevation = nodeAttributeSetXY.getDElevation().getValue();

        NodeAttributeXYList diffLocalNode = new NodeAttributeXYList(nodeAttributeSetXY.getLocalNode().getAttributeArray().length + 1);
        SegmentAttributeXYList diffDisabled = new SegmentAttributeXYList(nodeAttributeSetXY.getDisabled().getAttributeArray().length + 1);
        SegmentAttributeXYList diffEnabled = new SegmentAttributeXYList(nodeAttributeSetXY.getEnabled().getAttributeArray().length + 1);
        LaneDataAttributeList diffData = new LaneDataAttributeList(nodeAttributeSetXY.getData().getAttributeArray().length + 1);

        NodeAttributeSetXY nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(diffLocalNode);
        nodeAttributeSetXY2.setDisabled(diffDisabled);
        nodeAttributeSetXY2.setEnabled(diffEnabled);
        nodeAttributeSetXY2.setData(diffData);
        nodeAttributeSetXY2.setDWidth(dWidth + 1);
        nodeAttributeSetXY2.setDElevation(dElevation + 1);

        assertFalse(nodeAttributeSetXY.hashCode() == nodeAttributeSetXY2.hashCode());
        assertTrue(nodeAttributeSetXY.hashCode() == nodeAttributeSetXY.hashCode());
        assertTrue(nodeAttributeSetXY2.hashCode() == nodeAttributeSetXY2.hashCode());

        NodeAttributeSetXY nodeAttributeSetXY3 = new NodeAttributeSetXY();
        nodeAttributeSetXY3.setLocalNode(localNode);
        nodeAttributeSetXY3.setDisabled(disabled);
        nodeAttributeSetXY3.setEnabled(enabled);
        nodeAttributeSetXY3.setData(data);
        nodeAttributeSetXY3.setDWidth(dWidth);
        nodeAttributeSetXY3.setDElevation(dElevation);

        assertTrue(nodeAttributeSetXY.hashCode() == nodeAttributeSetXY3.hashCode());
        assertFalse(nodeAttributeSetXY2.hashCode() == nodeAttributeSetXY3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(nodeAttributeSetXY.equals(nodeAttributeSetXY));
        assertFalse(nodeAttributeSetXY.equals(null));
        assertFalse(nodeAttributeSetXY.equals(new String()));

        NodeAttributeXYList localNode = nodeAttributeSetXY.getLocalNode();
        SegmentAttributeXYList disabled = nodeAttributeSetXY.getDisabled();
        SegmentAttributeXYList enabled = nodeAttributeSetXY.getEnabled();
        LaneDataAttributeList data = nodeAttributeSetXY.getData();
        int dWidth = nodeAttributeSetXY.getDWidth().getValue();
        int dElevation = nodeAttributeSetXY.getDElevation().getValue();

        NodeAttributeXYList diffLocalNode = new NodeAttributeXYList(nodeAttributeSetXY.getLocalNode().getAttributeArray().length + 1);
        SegmentAttributeXYList diffDisabled = new SegmentAttributeXYList(nodeAttributeSetXY.getDisabled().getAttributeArray().length + 1);
        SegmentAttributeXYList diffEnabled = new SegmentAttributeXYList(nodeAttributeSetXY.getEnabled().getAttributeArray().length + 1);
        LaneDataAttributeList diffData = new LaneDataAttributeList(nodeAttributeSetXY.getData().getAttributeArray().length + 1);

        // different
        NodeAttributeSetXY nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(diffLocalNode);
        nodeAttributeSetXY2.setDisabled(diffDisabled);
        nodeAttributeSetXY2.setEnabled(diffEnabled);
        nodeAttributeSetXY2.setData(diffData);
        nodeAttributeSetXY2.setDWidth(dWidth + 1);
        nodeAttributeSetXY2.setDElevation(dElevation + 1);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different local node
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(diffLocalNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different disabled
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(diffDisabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different enabled
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(diffEnabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different data
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(diffData);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different width
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth + 1);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // different elevation
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation + 1);

        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        // same
        nodeAttributeSetXY2 = new NodeAttributeSetXY();
        nodeAttributeSetXY2.setLocalNode(localNode);
        nodeAttributeSetXY2.setDisabled(disabled);
        nodeAttributeSetXY2.setEnabled(enabled);
        nodeAttributeSetXY2.setData(data);
        nodeAttributeSetXY2.setDWidth(dWidth);
        nodeAttributeSetXY2.setDElevation(dElevation);

        assertTrue(nodeAttributeSetXY.equals(nodeAttributeSetXY2));
    }

    @Test
    public void testEqualsNull() {

        NodeAttributeXYList localNode = nodeAttributeSetXY.getLocalNode();
        SegmentAttributeXYList disabled = nodeAttributeSetXY.getDisabled();
        SegmentAttributeXYList enabled = nodeAttributeSetXY.getEnabled();
        LaneDataAttributeList data = nodeAttributeSetXY.getData();
        int dWidth = nodeAttributeSetXY.getDWidth().getValue();
        int dElevation = nodeAttributeSetXY.getDElevation().getValue();

        NodeAttributeSetXY nodeAttributeSetXY2 = new NodeAttributeSetXY();
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setLocalNode(localNode);
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setDisabled(disabled);
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setEnabled(enabled);
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setData(data);
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setDWidth(dWidth);
        assertFalse(nodeAttributeSetXY.equals(nodeAttributeSetXY2));

        nodeAttributeSetXY2.setDElevation(dElevation);
        assertTrue(nodeAttributeSetXY.equals(nodeAttributeSetXY2));
    }
}
