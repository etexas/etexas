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

import java.util.Objects;

import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.ApproachID;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The generic lane frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class GenericLane implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the generic lane.
     */
    public static final int NUM_BITS = 8;

    /**
     * The lane ID element.
     */
    private LaneID laneId;

    /**
     * The ingress approach ID element. (OPTIONAL)
     */
    private ApproachID ingressApproach;

    /**
     * The egress approach ID element. (OPTIONAL)
     */
    private ApproachID egressApproach;

    /**
     * The lane attributes frame.
     */
    private LaneAttributes laneAttributes;

    /**
     * The allowed maneuvers element. (OPTIONAL)
     */
    private AllowedManeuvers maneuvers;

    /**
     * The node list xy frame.
     */
    private NodeListXY nodeList;

    /**
     * The connects to list frame. (OPTIONAL)
     */
    private ConnectsToList connectsTo;

    /**
     * The overlay lane list frame. (OPTIONAL)
     */
    private OverlayLaneList overlays;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public GenericLane() {

        laneId = new LaneID();
        laneAttributes = new LaneAttributes();
        nodeList = new NodeListXY();
    }

    /**
     * A constructor for the generic lane frame for all required fields.
     * 
     * @param laneId The lane ID element.
     * @param laneAttributes The lane attributes frame.
     * @param nodeList The node list xy frame.
     */
    public GenericLane(LaneID laneId, LaneAttributes laneAttributes, NodeListXY nodeList) {

        this.laneId = Objects.requireNonNull(laneId);
        this.laneAttributes = Objects.requireNonNull(laneAttributes);
        this.nodeList = Objects.requireNonNull(nodeList);
    }

    /**
     * A constructor for the intersection reference ID frame for all required fields (primitive).
     * 
     * @param laneId The lane ID value.
     * @param laneAttributes The lane attributes frame.
     * @param nodeList The node list xy frame.
     */
    public GenericLane(int laneId, LaneAttributes laneAttributes, NodeListXY nodeList) {

        this.laneId = new LaneID(laneId);
        this.laneAttributes = Objects.requireNonNull(laneAttributes);
        this.nodeList = Objects.requireNonNull(nodeList);
    }

    /**
     * A getter for the lane ID element.
     * 
     * @return The lane ID element.
     */
    public LaneID getLaneId() {

        return laneId;
    }

    /**
     * A setter for the lane ID element.
     * 
     * @param laneId The lane ID element to set.
     */
    public void setLaneId(LaneID laneId) {

        this.laneId = Objects.requireNonNull(laneId);
    }

    /**
     * A setter for the lane ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param laneId The lane ID value to be set in the element.
     */
    public void setLaneId(int laneId) {

        this.laneId.setValue(laneId);
    }

    /**
     * A getter for the ingress approach ID element.
     * 
     * @return The ingress approach ID element.
     */
    public ApproachID getIngressApproach() {

        return ingressApproach;
    }

    /**
     * A setter for the ingress approach ID element.
     * 
     * @param ingressApproach The ingress approach ID element to set.
     */
    public void setIngressApproach(ApproachID ingressApproach) {

        this.ingressApproach = ingressApproach;
    }

    /**
     * A setter for the ingress approach ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param ingressApproach The ingress approach ID value to be set in the element.
     */
    public void setIngressApproach(int ingressApproach) {

        if (this.ingressApproach == null) {

            this.ingressApproach = new ApproachID();
        }
        this.ingressApproach.setValue(ingressApproach);
    }

    /**
     * A getter for the egress approach ID element.
     * 
     * @return The egress approach ID element.
     */
    public ApproachID getEgressApproach() {

        return egressApproach;
    }

    /**
     * A setter for the egress approach ID element.
     * 
     * @param egressApproach The egress approach ID element to set.
     */
    public void setEgressApproach(ApproachID egressApproach) {

        this.egressApproach = egressApproach;
    }

    /**
     * A setter for the egress approach ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param egressApproach The egress approach ID value to be set in the element.
     */
    public void setEgressApproach(int egressApproach) {

        if (this.egressApproach == null) {

            this.egressApproach = new ApproachID();
        }
        this.egressApproach.setValue(egressApproach);
    }

    /**
     * A getter for the lane attributes frame.
     * 
     * @return The lane attributes frame.
     */
    public LaneAttributes getLaneAttributes() {

        return laneAttributes;
    }

    /**
     * A setter for the lane attributes frame.
     * 
     * @param laneAttributes The lane attributes frame.
     */
    public void setLaneAttributes(LaneAttributes laneAttributes) {

        this.laneAttributes = Objects.requireNonNull(laneAttributes);
    }

    /**
     * A getter for the allowed maneuvers element.
     * 
     * @return The allowed maneuvers element.
     */
    public AllowedManeuvers getManeuvers() {

        return maneuvers;
    }

    /**
     * A setter for the allowed maneuvers element.
     * 
     * @param maneuvers The allowed maneuvers element to set.
     */
    public void setManeuvers(AllowedManeuvers maneuvers) {

        this.maneuvers = maneuvers;
    }

    /**
     * A getter for the node list xy frame.
     * 
     * @return The node list xy frame.
     */
    public NodeListXY getNodeList() {

        return nodeList;
    }

    /**
     * A setter for the node list xy frame.
     * 
     * @param nodeList The node list xy frame.
     */
    public void setNodeList(NodeListXY nodeList) {

        this.nodeList = Objects.requireNonNull(nodeList);
    }

    /**
     * A getter for the connects to list frame.
     * 
     * @return The connects to list frame.
     */
    public ConnectsToList getConnectsTo() {

        return connectsTo;
    }

    /**
     * A setter for the connects to list frame.
     * 
     * @param connectsTo The connects to list frame.
     */
    public void setConnectsTo(ConnectsToList connectsTo) {

        this.connectsTo = connectsTo;
    }

    /**
     * A getter for the overlay lane list frame.
     * 
     * @return The overlay lane list frame.
     */
    public OverlayLaneList getOverlays() {

        return overlays;
    }

    /**
     * A setter for the overlay lane list frame.
     * 
     * @param overlays The overlay lane list frame.
     */
    public void setOverlays(OverlayLaneList overlays) {

        this.overlays = overlays;
    }

    @Override
    public String encodeUPER() {

        StringBuilder genericLaneBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension or descriptive name usable so setting both to
        // off.
        optionalBits.append("00");

        genericLaneBits.append(laneId.encodeUPER());

        if (ingressApproach != null) {

            optionalBits.append('1');
            genericLaneBits.append(ingressApproach.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (egressApproach != null) {

            optionalBits.append('1');
            genericLaneBits.append(egressApproach.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        genericLaneBits.append(laneAttributes.encodeUPER());

        if (maneuvers != null) {

            optionalBits.append('1');
            genericLaneBits.append(maneuvers.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        genericLaneBits.append(nodeList.encodeUPER());

        if (connectsTo != null) {

            optionalBits.append('1');
            genericLaneBits.append(connectsTo.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (overlays != null) {

            optionalBits.append('1');
            genericLaneBits.append(overlays.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        genericLaneBits.insert(0, optionalBits);

        return genericLaneBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (GenericLane.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a GenericLane frame (%d)", GenericLane.NUM_BITS));
        }

        String genericLaneOptionalBits = bits.substring(0, GenericLane.NUM_BITS);
        bits = bits.substring(GenericLane.NUM_BITS);

        if (genericLaneOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The GenericLane extension is not supported");
        }

        if (genericLaneOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The GenericLane descriptive name is not supported");
        }

        if (genericLaneOptionalBits.charAt(7) != '0') {

            throw new IllegalArgumentException("The GenericLane regional extension is not supported");
        }

        bits = laneId.decodeUPER(bits);

        if (genericLaneOptionalBits.charAt(2) == '1') {

            ingressApproach = new ApproachID();
            bits = ingressApproach.decodeUPER(bits);
        }

        if (genericLaneOptionalBits.charAt(3) == '1') {

            egressApproach = new ApproachID();
            bits = egressApproach.decodeUPER(bits);
        }

        bits = laneAttributes.decodeUPER(bits);

        if (genericLaneOptionalBits.charAt(4) == '1') {

            maneuvers = new AllowedManeuvers();
            bits = maneuvers.decodeUPER(bits);
        }

        bits = nodeList.decodeUPER(bits);

        if (genericLaneOptionalBits.charAt(5) == '1') {

            connectsTo = new ConnectsToList();
            bits = connectsTo.decodeUPER(bits);
        }

        if (genericLaneOptionalBits.charAt(6) == '1') {

            overlays = new OverlayLaneList();
            bits = overlays.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(laneId, ingressApproach, egressApproach, laneAttributes, maneuvers, nodeList, connectsTo, overlays);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof GenericLane)) {

            return false;
        }
        GenericLane frame = (GenericLane)object;
        return this.laneId.equals(frame.laneId)
                && Objects.equals(this.ingressApproach, frame.ingressApproach)
                && Objects.equals(this.egressApproach, frame.egressApproach)
                && this.laneAttributes.equals(frame.laneAttributes)
                && Objects.equals(this.maneuvers, frame.maneuvers)
                && this.nodeList.equals(frame.nodeList)
                && Objects.equals(this.connectsTo, frame.connectsTo)
                && Objects.equals(this.overlays, frame.overlays);
    }
}
