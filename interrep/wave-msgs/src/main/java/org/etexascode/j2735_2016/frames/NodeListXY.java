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

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The node list xy frame (Choice) for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeListXY implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the node list xy.
     */
    public static final int NUM_BITS = 2;

    /**
     * The node set xy frame.
     */
    private NodeSetXY nodes;

    /**
     * The computed lane frame.
     */
    private ComputedLane computed;

    /**
     * A constructor setup only for decoding purposes.
     */
    public NodeListXY() {

        nodes = null;
        computed = null;
    }

    /**
     * A constructor for the node list xy frame for the node set xy choice.
     * 
     * @param nodes The node set xy frame.
     */
    public NodeListXY(NodeSetXY nodes) {

        this.nodes = Objects.requireNonNull(nodes);
    }

    /**
     * A constructor for the node list xy frame for the computed lane choice.
     * 
     * @param computed The computed lane frame.
     */
    public NodeListXY(ComputedLane computed) {

        this.computed = Objects.requireNonNull(computed);
    }

    /**
     * A getter for the node set xy frame.
     * 
     * @return The node set xy frame.
     */
    public NodeSetXY getNodes() {

        return nodes;
    }

    /**
     * A getter for the computed lane frame.
     * 
     * @return The computed lane frame.
     */
    public ComputedLane getComputed() {

        return computed;
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeListXYBits = new StringBuilder();
        nodeListXYBits.append('0');

        if (nodes != null) {

            nodeListXYBits.append(UPERInteger.encode(0, 0, NodeListXY.NUM_BITS - 1));
            nodeListXYBits.append(nodes.encodeUPER());
        }
        else if (computed != null) {

            nodeListXYBits.append(UPERInteger.encode(1, 0, NodeListXY.NUM_BITS - 1));
            nodeListXYBits.append(computed.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return nodeListXYBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeListXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeListXY frame (%d)", NodeListXY.NUM_BITS));
        }

        String nodeListXYChoiceBits = bits.substring(0, NodeListXY.NUM_BITS);

        if (nodeListXYChoiceBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The NodeListXY extension is not supported");
        }

        bits = bits.substring(NodeListXY.NUM_BITS);

        int choice = UPERInteger.decode(nodeListXYChoiceBits.substring(1), 0);

        switch (choice) {

            case 0:
                nodes = new NodeSetXY();
                bits = nodes.decodeUPER(bits);
                break;
            case 1:
                computed = new ComputedLane();
                bits = computed.decodeUPER(bits);
                break;
            default:
                throw new IllegalArgumentException(String.format("There should not be any more possible integers that would be returned, but received %d", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(nodes, computed);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeListXY)) {

            return false;
        }
        NodeListXY frame = (NodeListXY)object;
        return Objects.equals(this.nodes, frame.nodes)
                && Objects.equals(this.computed, frame.computed);
    }
}
