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

import java.util.Arrays;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The node set xy frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeSetXY implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the node set xy size.
     */
    public static final int NUM_BITS_SET_SIZE = 6;

    /**
     * The maximum size of the node set xy.
     */
    public static final int MAX_SET_SIZE = 63;

    /**
     * The minimum size of the node set xy.
     */
    public static final int MIN_SET_SIZE = 2;

    /**
     * The node set xy.
     */
    private NodeXY[] nodes;

    /**
     * A constructor setup only for decoding purposes.
     */
    public NodeSetXY() {

        nodes = new NodeXY[0];
    }

    /**
     * A constructor for the node set xy frame, where the size will be validated.
     * 
     * @param size The node set xy size to set.
     */
    public NodeSetXY(int size) {

        this.nodes = new NodeXY[validate(size)];
    }

    /**
     * A getter for the node array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The node array.
     */
    public NodeXY[] getNodeArray() {

        return nodes;
    }

    /**
     * Validates the node set xy size.
     * 
     * @param size The size to be validated.
     * @return The node set xy size that was successfully validated.
     */
    private int validate(int size) {

        if (size < NodeSetXY.MIN_SET_SIZE || size > NodeSetXY.MAX_SET_SIZE) {

            throw new IllegalArgumentException(String.format("The size of the node set xy frame must be in the range of %d to %d", NodeSetXY.MIN_SET_SIZE, NodeSetXY.MAX_SET_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (nodes.length < NodeSetXY.MIN_SET_SIZE) {

            throw new IllegalStateException(String.format("There are not enough NodeXY objects to encode the set. minimum: %d", NodeSetXY.MIN_SET_SIZE));
        }

        StringBuilder nodeSetXYBits = new StringBuilder(UPERInteger.encode(nodes.length, NodeSetXY.MIN_SET_SIZE, NodeSetXY.NUM_BITS_SET_SIZE));

        for (int i = 0; i < nodes.length; i++) {

            if (nodes[i] == null) {

                throw new IllegalStateException(String.format("The node set xy frame was not filled up to the amount specified. specified: %d, received: %d", nodes.length, i + 1));
            }
            nodeSetXYBits.append(nodes[i].encodeUPER());
        }
        return nodeSetXYBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeSetXY.NUM_BITS_SET_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeSetXY frame (%d)", NodeSetXY.NUM_BITS_SET_SIZE));
        }
        // 000000 = 2
        int size = validate(UPERInteger.decode(bits.substring(0, NodeSetXY.NUM_BITS_SET_SIZE), NodeSetXY.MIN_SET_SIZE));

        bits = bits.substring(NodeSetXY.NUM_BITS_SET_SIZE);

        nodes = new NodeXY[size];
        for (int i = 0; i < size; i++) {

            NodeXY node = new NodeXY();
            bits = node.decodeUPER(bits);
            nodes[i] = node;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(nodes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeSetXY)) {

            return false;
        }
        NodeSetXY frame = (NodeSetXY)object;
        return Arrays.equals(this.nodes, frame.nodes);
    }
}
