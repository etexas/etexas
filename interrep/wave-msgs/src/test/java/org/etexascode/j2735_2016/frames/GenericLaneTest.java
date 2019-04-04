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

import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.ApproachID;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.elements.LaneAttributesBike;
import org.etexascode.j2735_2016.elements.LaneAttributesSidewalk;
import org.etexascode.j2735_2016.elements.LaneAttributesVehicle;
import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the generic lane frame.
 * 
 * @author ttevendale
 */
public class GenericLaneTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    GenericLane lane;

    @Before
    public void init() {

        LaneID laneId = new LaneID(15);
        ApproachID ingressApproach = new ApproachID(2);
        ApproachID egressApproach = new ApproachID(3);
        LaneAttributes laneAttributes = new LaneAttributes();
        AllowedManeuvers maneuvers = new AllowedManeuvers();
        maneuvers.setCaution(true);
        NodeListXY nodeList = new NodeListXY(new ComputedLane(18, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(38)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50))));
        ConnectsToList connectsTo = new ConnectsToList(1);
        connectsTo.getConnectionArray()[0] = new Connection(new ConnectingLane(100));
        OverlayLaneList overlays = new OverlayLaneList(1);
        overlays.getLaneArray()[0] = new LaneID(5);

        lane = new GenericLane(laneId, laneAttributes, nodeList);
        lane.setIngressApproach(ingressApproach);
        lane.setEgressApproach(egressApproach);
        lane.setManeuvers(maneuvers);
        lane.setConnectsTo(connectsTo);
        lane.setOverlays(overlays);
    }

    @Test
    public void testConstructor() {

        LaneID laneId = new LaneID(10);
        LaneAttributes laneAttributes = new LaneAttributes();
        NodeListXY nodeList = new NodeListXY(new ComputedLane(2, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(100)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-25))));

        GenericLane lane = new GenericLane(laneId, laneAttributes, nodeList);

        assertNull(lane.getIngressApproach());
        assertNull(lane.getEgressApproach());
        assertNull(lane.getManeuvers());
        assertNull(lane.getConnectsTo());
        assertNull(lane.getOverlays());
        assertTrue(laneId.equals(lane.getLaneId()));
        assertTrue(laneAttributes.equals(lane.getLaneAttributes()));
        assertTrue(nodeList.equals(lane.getNodeList()));
    }

    @Test
    public void testConstructorNullLaneId() {

        thrown.expect(NullPointerException.class);
        new GenericLane(null, new LaneAttributes(), new NodeListXY());
    }

    @Test
    public void testConstructorNullLaneAttributes() {

        thrown.expect(NullPointerException.class);
        new GenericLane(new LaneID(), null, new NodeListXY());
    }

    @Test
    public void testConstructorNullNodeList() {

        thrown.expect(NullPointerException.class);
        new GenericLane(new LaneID(), new LaneAttributes(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int laneId = 38;
        LaneAttributes laneAttributes = new LaneAttributes();
        NodeListXY nodeList = new NodeListXY(new ComputedLane(2, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(100)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-25))));

        GenericLane lane = new GenericLane(laneId, laneAttributes, nodeList);

        assertNull(lane.getIngressApproach());
        assertNull(lane.getEgressApproach());
        assertNull(lane.getManeuvers());
        assertNull(lane.getConnectsTo());
        assertNull(lane.getOverlays());
        assertTrue(laneId == lane.getLaneId().getValue());
        assertTrue(laneAttributes.equals(lane.getLaneAttributes()));
        assertTrue(nodeList.equals(lane.getNodeList()));
    }

    @Test
    public void testConstructorPrimitiveNullLaneAttributes() {

        thrown.expect(NullPointerException.class);
        new GenericLane(0, null, new NodeListXY());
    }

    @Test
    public void testConstructorPrimitiveNullNodeList() {

        thrown.expect(NullPointerException.class);
        new GenericLane(0, new LaneAttributes(), null);
    }

    @Test
    public void testSetLaneId() {

        LaneID laneId = new LaneID(1);

        GenericLane lane = new GenericLane();
        lane.setLaneId(laneId);

        assertTrue(laneId.equals(lane.getLaneId()));

        thrown.expect(NullPointerException.class);
        lane.setLaneId(null);
    }

    @Test
    public void testSetLaneIdPrimitive() {

        int laneId = 2;

        GenericLane lane = new GenericLane();
        lane.setLaneId(laneId);

        assertTrue(laneId == lane.getLaneId().getValue());
    }

    @Test
    public void testSetIngressApproachPrimitive() {

        int ingressApproach = 0;

        GenericLane lane = new GenericLane();
        lane.setIngressApproach(ingressApproach);

        assertTrue(ingressApproach == lane.getIngressApproach().getValue());

        ingressApproach = 1;

        lane.setIngressApproach(ingressApproach);

        assertTrue(ingressApproach == lane.getIngressApproach().getValue());
    }

    @Test
    public void testSetEgressApproachPrimitive() {

        int egressApproach = 2;

        GenericLane lane = new GenericLane();
        lane.setEgressApproach(egressApproach);

        assertTrue(egressApproach == lane.getEgressApproach().getValue());

        egressApproach = 3;

        lane.setEgressApproach(egressApproach);

        assertTrue(egressApproach == lane.getEgressApproach().getValue());
    }

    @Test
    public void testSetLaneAttributes() {

        LaneAttributes laneAttributes = new LaneAttributes();

        GenericLane lane = new GenericLane();
        lane.setLaneAttributes(laneAttributes);

        assertTrue(laneAttributes.equals(lane.getLaneAttributes()));

        thrown.expect(NullPointerException.class);
        lane.setLaneAttributes(null);
    }

    @Test
    public void testSetNodeList() {

        NodeListXY nodeList = new NodeListXY();

        GenericLane lane = new GenericLane();
        lane.setNodeList(nodeList);

        assertTrue(nodeList.equals(lane.getNodeList()));

        thrown.expect(NullPointerException.class);
        lane.setNodeList(null);
    }

    @Test
    public void testEncodeUPERMin() {

        LaneID laneId = new LaneID(18);
        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setLaneType(new LaneTypeAttributes(new LaneAttributesVehicle()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(100, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(1)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(2))));

        GenericLane lane = new GenericLane(laneId, laneAttributes, nodeList);

        String laneOptionals = "00000000";
        String remainingBits = laneId.encodeUPER() + laneAttributes.encodeUPER() + nodeList.encodeUPER();
        assertTrue((laneOptionals + remainingBits).equals(lane.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        LaneID laneId = new LaneID(15);
        ApproachID ingressApproach = new ApproachID(2);
        ApproachID egressApproach = new ApproachID(3);
        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));
        AllowedManeuvers maneuvers = new AllowedManeuvers();
        maneuvers.setCaution(true);
        NodeListXY nodeList = new NodeListXY(new ComputedLane(18, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(38)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50))));
        ConnectsToList connectsTo = new ConnectsToList(1);
        connectsTo.getConnectionArray()[0] = new Connection(new ConnectingLane(100));
        OverlayLaneList overlays = new OverlayLaneList(1);
        overlays.getLaneArray()[0] = new LaneID(5);

        GenericLane lane = new GenericLane(laneId, laneAttributes, nodeList);
        lane.setIngressApproach(ingressApproach);
        lane.setEgressApproach(egressApproach);
        lane.setManeuvers(maneuvers);
        lane.setConnectsTo(connectsTo);
        lane.setOverlays(overlays);

        String laneOptionals = "00111110";
        String remainingBits = laneId.encodeUPER() + ingressApproach.encodeUPER() + egressApproach.encodeUPER() + laneAttributes.encodeUPER() + maneuvers.encodeUPER() + nodeList.encodeUPER()
                + connectsTo.encodeUPER() + overlays.encodeUPER();
        assertTrue((laneOptionals + remainingBits).equals(lane.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        LaneID laneId = new LaneID(81);
        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setLaneType(new LaneTypeAttributes(new LaneAttributesSidewalk()));
        NodeListXY nodeList = new NodeListXY(new ComputedLane(50, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(2)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(1))));

        String laneOptionals = "00000000";

        GenericLane lane = new GenericLane();
        String remainingBits = lane.decodeUPER(laneOptionals + laneId.encodeUPER() + laneAttributes.encodeUPER() + nodeList.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(lane.getIngressApproach());
        assertNull(lane.getEgressApproach());
        assertNull(lane.getManeuvers());
        assertNull(lane.getConnectsTo());
        assertNull(lane.getOverlays());
        assertTrue(laneId.equals(lane.getLaneId()));
        assertTrue(laneAttributes.equals(lane.getLaneAttributes()));
        assertTrue(nodeList.equals(lane.getNodeList()));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneID laneId = new LaneID(15);
        ApproachID ingressApproach = new ApproachID(2);
        ApproachID egressApproach = new ApproachID(3);
        LaneAttributes laneAttributes = new LaneAttributes();
        laneAttributes.setLaneType(new LaneTypeAttributes(new LaneAttributesBike()));
        AllowedManeuvers maneuvers = new AllowedManeuvers();
        maneuvers.setCaution(true);
        NodeListXY nodeList = new NodeListXY(new ComputedLane(18, new ComputedLaneOffsetXAxis(new DrivenLineOffsetSm(38)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-50))));
        ConnectsToList connectsTo = new ConnectsToList(1);
        connectsTo.getConnectionArray()[0] = new Connection(new ConnectingLane(100));
        OverlayLaneList overlays = new OverlayLaneList(1);
        overlays.getLaneArray()[0] = new LaneID(5);

        String laneOptionals = "00111110";

        GenericLane lane = new GenericLane();
        String remainingBits = lane
                .decodeUPER(laneOptionals + laneId.encodeUPER() + ingressApproach.encodeUPER() + egressApproach.encodeUPER() + laneAttributes.encodeUPER() + maneuvers.encodeUPER()
                        + nodeList.encodeUPER() + connectsTo.encodeUPER() + overlays.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(laneId.equals(lane.getLaneId()));
        assertTrue(ingressApproach.equals(lane.getIngressApproach()));
        assertTrue(egressApproach.equals(lane.getEgressApproach()));
        assertTrue(laneAttributes.equals(lane.getLaneAttributes()));
        assertTrue(maneuvers.equals(lane.getManeuvers()));
        assertTrue(nodeList.equals(lane.getNodeList()));
        assertTrue(connectsTo.equals(lane.getConnectsTo()));
        assertTrue(overlays.equals(lane.getOverlays()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String laneOptionals = "10000000";

        GenericLane lane = new GenericLane();
        thrown.expect(IllegalArgumentException.class);
        lane.decodeUPER(laneOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String laneOptionals = "01000000";

        GenericLane lane = new GenericLane();
        thrown.expect(IllegalArgumentException.class);
        lane.decodeUPER(laneOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String laneOptionals = "00000001";

        GenericLane lane = new GenericLane();
        thrown.expect(IllegalArgumentException.class);
        lane.decodeUPER(laneOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String laneOptionals = "0000000";

        GenericLane lane = new GenericLane();
        thrown.expect(IllegalArgumentException.class);
        lane.decodeUPER(laneOptionals);
    }

    @Test
    public void testHashCode() {

        int laneId = lane.getLaneId().getValue();
        int ingressApproach = lane.getIngressApproach().getValue();
        int egressApproach = lane.getEgressApproach().getValue();
        LaneAttributes laneAttributes = lane.getLaneAttributes();
        AllowedManeuvers maneuvers = lane.getManeuvers();
        NodeListXY nodeList = lane.getNodeList();
        ConnectsToList connectsTo = lane.getConnectsTo();
        OverlayLaneList overlays = lane.getOverlays();

        LaneAttributes diffLaneAttributes = new LaneAttributes();
        LaneDirection diffLaneDirection = new LaneDirection();
        diffLaneDirection.setIngressPath(!laneAttributes.getDirectionalUse().isIngressPath());
        diffLaneDirection.setEgressPath(!laneAttributes.getDirectionalUse().isEgressPath());
        diffLaneAttributes.setDirectionalUse(diffLaneDirection);
        AllowedManeuvers diffManeuvers = new AllowedManeuvers();
        diffManeuvers.setManeuverNoStoppingAllowed(!maneuvers.isManeuverNoStoppingAllowed());
        NodeListXY diffNodeList = new NodeListXY(new ComputedLane(1, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(14874)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-234))));
        ConnectsToList diffConnectsTo = new ConnectsToList(connectsTo.getConnectionArray().length + 1);
        OverlayLaneList diffOverlays = new OverlayLaneList(overlays.getLaneArray().length + 1);

        GenericLane lane2 = new GenericLane(laneId + 1, diffLaneAttributes, diffNodeList);
        lane2.setIngressApproach(ingressApproach + 1);
        lane2.setEgressApproach(egressApproach + 1);
        lane2.setManeuvers(diffManeuvers);
        lane2.setConnectsTo(diffConnectsTo);
        lane2.setOverlays(diffOverlays);

        assertFalse(lane.hashCode() == lane2.hashCode());
        assertTrue(lane.hashCode() == lane.hashCode());
        assertTrue(lane2.hashCode() == lane2.hashCode());

        GenericLane lane3 = new GenericLane(laneId, laneAttributes, nodeList);
        lane3.setIngressApproach(ingressApproach);
        lane3.setEgressApproach(egressApproach);
        lane3.setManeuvers(maneuvers);
        lane3.setConnectsTo(connectsTo);
        lane3.setOverlays(overlays);

        assertTrue(lane.hashCode() == lane3.hashCode());
        assertFalse(lane2.hashCode() == lane3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(lane.equals(lane));
        assertFalse(lane.equals(null));
        assertFalse(lane.equals(new String()));

        int laneId = lane.getLaneId().getValue();
        int ingressApproach = lane.getIngressApproach().getValue();
        int egressApproach = lane.getEgressApproach().getValue();
        LaneAttributes laneAttributes = lane.getLaneAttributes();
        AllowedManeuvers maneuvers = lane.getManeuvers();
        NodeListXY nodeList = lane.getNodeList();
        ConnectsToList connectsTo = lane.getConnectsTo();
        OverlayLaneList overlays = lane.getOverlays();

        LaneAttributes diffLaneAttributes = new LaneAttributes();
        LaneDirection diffLaneDirection = new LaneDirection();
        diffLaneDirection.setIngressPath(!laneAttributes.getDirectionalUse().isIngressPath());
        diffLaneDirection.setEgressPath(!laneAttributes.getDirectionalUse().isEgressPath());
        diffLaneAttributes.setDirectionalUse(diffLaneDirection);
        AllowedManeuvers diffManeuvers = new AllowedManeuvers();
        diffManeuvers.setManeuverNoStoppingAllowed(!maneuvers.isManeuverNoStoppingAllowed());
        NodeListXY diffNodeList = new NodeListXY(new ComputedLane(1, new ComputedLaneOffsetXAxis(new DrivenLineOffsetLg(14874)), new ComputedLaneOffsetYAxis(new DrivenLineOffsetSm(-234))));
        ConnectsToList diffConnectsTo = new ConnectsToList(connectsTo.getConnectionArray().length + 1);
        OverlayLaneList diffOverlays = new OverlayLaneList(overlays.getLaneArray().length + 1);

        // different
        GenericLane lane2 = new GenericLane(laneId + 1, diffLaneAttributes, diffNodeList);
        lane2.setIngressApproach(ingressApproach + 1);
        lane2.setEgressApproach(egressApproach + 1);
        lane2.setManeuvers(diffManeuvers);
        lane2.setConnectsTo(diffConnectsTo);
        lane2.setOverlays(diffOverlays);

        assertFalse(lane.equals(lane2));

        // different lane ID
        lane2 = new GenericLane(laneId + 1, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different lane attributes
        lane2 = new GenericLane(laneId, diffLaneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different node list
        lane2 = new GenericLane(laneId, laneAttributes, diffNodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different ingress approach
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach + 1);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different egress approach
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach + 1);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different maneuvers
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(diffManeuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different connects to
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(diffConnectsTo);
        lane2.setOverlays(overlays);

        assertFalse(lane.equals(lane2));

        // different overlays
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(diffOverlays);

        assertFalse(lane.equals(lane2));

        // same
        lane2 = new GenericLane(laneId, laneAttributes, nodeList);
        lane2.setIngressApproach(ingressApproach);
        lane2.setEgressApproach(egressApproach);
        lane2.setManeuvers(maneuvers);
        lane2.setConnectsTo(connectsTo);
        lane2.setOverlays(overlays);

        assertTrue(lane.equals(lane2));
    }
}
