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

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneID;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the node list xy frame (Choice).
 * 
 * @author ttevendale
 */
public class NodeListXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorNodeSetXY() {

        NodeSetXY nodeSet = new NodeSetXY();

        NodeListXY nodeList = new NodeListXY(nodeSet);

        assertTrue(nodeSet.equals(nodeList.getNodes()));
        assertNull(nodeList.getComputed());

        thrown.expect(NullPointerException.class);
        new NodeListXY((NodeSetXY)null);
    }

    @Test
    public void testConstructorComputedLane() {

        ComputedLane computed = new ComputedLane();

        NodeListXY nodeList = new NodeListXY(computed);

        assertTrue(computed.equals(nodeList.getComputed()));
        assertNull(nodeList.getNodes());

        thrown.expect(NullPointerException.class);
        new NodeListXY((ComputedLane)null);
    }

    @Test
    public void testEncodeUPERNodeSetXY() {

        NodeSetXY nodeSet = new NodeSetXY(2);
        nodeSet.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(10, 20)));
        nodeSet.getNodeArray()[1] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(20, 10)));

        NodeListXY nodeList = new NodeListXY(nodeSet);

        String nodeListXYChoice = "00";
        String remainingBits = nodeSet.encodeUPER();

        assertTrue((nodeListXYChoice + remainingBits).equals(nodeList.encodeUPER()));
    }

    @Test
    public void testEncodeUPERComputedLane() {

        ComputedLane computed = new ComputedLane(new LaneID(3), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(2)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(1)));

        NodeListXY nodeList = new NodeListXY(computed);

        String nodeListXYChoice = "01";
        String remainingBits = computed.encodeUPER();

        assertTrue((nodeListXYChoice + remainingBits).equals(nodeList.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        NodeListXY nodeList = new NodeListXY();
        thrown.expect(IllegalStateException.class);
        nodeList.encodeUPER();
    }

    @Test
    public void testDecodeUPERNodeSetXY() {

        NodeSetXY nodeSet = new NodeSetXY(2);
        nodeSet.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(100, 20)));
        nodeSet.getNodeArray()[1] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(200, 10)));

        String nodeListXYChoice = "00";

        NodeListXY nodeList = new NodeListXY();
        nodeList.decodeUPER(nodeListXYChoice + nodeSet.encodeUPER());

        assertTrue(nodeSet.equals(nodeList.getNodes()));
        assertNull(nodeList.getComputed());
    }

    @Test
    public void testDecodeUPERComputedLane() {

        ComputedLane computed = new ComputedLane(new LaneID(1), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(1)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(2)));

        String nodeListXYChoice = "01";

        NodeListXY nodeList = new NodeListXY();
        nodeList.decodeUPER(nodeListXYChoice + computed.encodeUPER());

        assertTrue(computed.equals(nodeList.getComputed()));
        assertNull(nodeList.getNodes());
    }

    @Test
    public void testDecodeUPERExtension() {

        String nodeListXYChoice = "10";

        NodeListXY nodeList = new NodeListXY();
        thrown.expect(IllegalArgumentException.class);
        nodeList.decodeUPER(nodeListXYChoice);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String nodeListXYChoice = "";

        NodeListXY nodeList = new NodeListXY();
        thrown.expect(IllegalArgumentException.class);
        nodeList.decodeUPER(nodeListXYChoice);
    }

    @Test
    public void testHashCode() {

        ComputedLane computed = new ComputedLane(new LaneID(2), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(2)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(1)));
        ComputedLane sameComputed = new ComputedLane(new LaneID(2), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(2)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(1)));
        ComputedLane diffComputed = new ComputedLane(new LaneID(1), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(1)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(2)));

        NodeListXY nodeList = new NodeListXY(computed);

        NodeListXY nodeList2 = new NodeListXY(diffComputed);

        assertFalse(nodeList.hashCode() == nodeList2.hashCode());
        assertTrue(nodeList.hashCode() == nodeList.hashCode());
        assertTrue(nodeList2.hashCode() == nodeList2.hashCode());

        NodeListXY nodeList3 = new NodeListXY(sameComputed);

        assertTrue(nodeList.hashCode() == nodeList3.hashCode());
        assertFalse(nodeList2.hashCode() == nodeList3.hashCode());
    }

    @Test
    public void testEquals() {

        NodeSetXY nodeSet = new NodeSetXY(2);
        nodeSet.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(100, 20)));
        nodeSet.getNodeArray()[1] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(200, 10)));

        NodeSetXY sameNodeSet = new NodeSetXY(2);
        sameNodeSet.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(100, 20)));
        sameNodeSet.getNodeArray()[1] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(200, 10)));

        NodeSetXY diffNodeSet = new NodeSetXY(2);
        diffNodeSet.getNodeArray()[0] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(500, 40)));
        diffNodeSet.getNodeArray()[1] = new NodeXY(new NodeOffsetPointXY(new NodeXY20B(300, 20)));

        ComputedLane computed = new ComputedLane(new LaneID(10), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(20)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(10)));
        ComputedLane sameComputed = new ComputedLane(new LaneID(10), new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(20)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(10)));
        ComputedLane diffComputed = new ComputedLane(new LaneID(20), new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(12521)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetLg(5555)));

        NodeListXY nodeList = new NodeListXY(nodeSet);

        assertTrue(nodeList.equals(nodeList));
        assertFalse(nodeList.equals(null));
        assertFalse(nodeList.equals(new String()));

        // different node set xy
        NodeListXY nodeList2 = new NodeListXY(diffNodeSet);
        assertFalse(nodeList.equals(nodeList2));

        // same node set xy
        nodeList2 = new NodeListXY(sameNodeSet);
        assertTrue(nodeList.equals(nodeList2));

        // different computed lane
        nodeList = new NodeListXY(computed);
        nodeList2 = new NodeListXY(diffComputed);
        assertFalse(nodeList.equals(nodeList2));

        // same computed lane
        nodeList2 = new NodeListXY(sameComputed);
        assertTrue(nodeList.equals(nodeList2));
    }
}
