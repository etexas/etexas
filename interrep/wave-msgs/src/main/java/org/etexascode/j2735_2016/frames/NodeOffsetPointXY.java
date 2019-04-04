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
 * The node offset point xy frame (Choice) for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeOffsetPointXY implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the node offset point xy.
     */
    public static final int NUM_BITS = 3;

    /**
     * The node xy 20b frame.
     */
    private NodeXY20B nodeXY20B;

    /**
     * The node xy 22b frame.
     */
    private NodeXY22B nodeXY22B;

    /**
     * The node xy 24b frame.
     */
    private NodeXY24B nodeXY24B;

    /**
     * The node xy 26b frame.
     */
    private NodeXY26B nodeXY26B;

    /**
     * The node xy 28b frame.
     */
    private NodeXY28B nodeXY28B;

    /**
     * The node xy 32b frame.
     */
    private NodeXY32B nodeXY32B;

    /**
     * The node longitude latitude micro degree 64b frame.
     */
    private NodeLLmD64B nodeLatLong;

    /**
     * A constructor setup only for decoding purposes.
     */
    public NodeOffsetPointXY() {

        nodeXY20B = null;
        nodeXY22B = null;
        nodeXY24B = null;
        nodeXY26B = null;
        nodeXY28B = null;
        nodeXY32B = null;
        nodeLatLong = null;
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 20b choice.
     * 
     * @param nodeXY20B The node xy 20b element.
     */
    public NodeOffsetPointXY(NodeXY20B nodeXY20B) {

        this.nodeXY20B = Objects.requireNonNull(nodeXY20B);
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 22b choice.
     * 
     * @param nodeXY22B The node xy 22b element.
     */
    public NodeOffsetPointXY(NodeXY22B nodeXY22B) {

        this.nodeXY22B = Objects.requireNonNull(nodeXY22B);
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 24b choice.
     * 
     * @param nodeXY24B The node xy 24b element.
     */
    public NodeOffsetPointXY(NodeXY24B nodeXY24B) {

        this.nodeXY24B = Objects.requireNonNull(nodeXY24B);
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 26b choice.
     * 
     * @param nodeXY26B The node xy 26b element.
     */
    public NodeOffsetPointXY(NodeXY26B nodeXY26B) {

        this.nodeXY26B = Objects.requireNonNull(nodeXY26B);
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 28b choice.
     * 
     * @param nodeXY28B The node xy 28b element.
     */
    public NodeOffsetPointXY(NodeXY28B nodeXY28B) {

        this.nodeXY28B = Objects.requireNonNull(nodeXY28B);
    }

    /**
     * A constructor for the node offset point xy frame for the node xy 32b choice.
     * 
     * @param nodeXY32B The node xy 32b element.
     */
    public NodeOffsetPointXY(NodeXY32B nodeXY32B) {

        this.nodeXY32B = Objects.requireNonNull(nodeXY32B);
    }

    /**
     * A constructor for the node offset point xy frame for the node longitude latitude micro degree
     * 64b choice.
     * 
     * @param nodeLatLong The node longitude latitude micro degree 64b element.
     */
    public NodeOffsetPointXY(NodeLLmD64B nodeLatLong) {

        this.nodeLatLong = Objects.requireNonNull(nodeLatLong);
    }

    /**
     * A getter for the node xy 20b element.
     * 
     * @return The node xy 20b element.
     */
    public NodeXY20B getNodeXY20B() {

        return nodeXY20B;
    }

    /**
     * A getter for the node xy 22b element.
     * 
     * @return The node xy 22b element.
     */
    public NodeXY22B getNodeXY22B() {

        return nodeXY22B;
    }

    /**
     * A getter for the node xy 24b element.
     * 
     * @return The node xy 24b element.
     */
    public NodeXY24B getNodeXY24B() {

        return nodeXY24B;
    }

    /**
     * A getter for the node xy 26b element.
     * 
     * @return The node xy 26b element.
     */
    public NodeXY26B getNodeXY26B() {

        return nodeXY26B;
    }

    /**
     * A getter for the node xy 28b element.
     * 
     * @return The node xy 28b element.
     */
    public NodeXY28B getNodeXY28B() {

        return nodeXY28B;
    }

    /**
     * A getter for the node xy 32b element.
     * 
     * @return The node xy 32b element.
     */
    public NodeXY32B getNodeXY32B() {

        return nodeXY32B;
    }

    /**
     * A getter for the node longitude latitude micro degree 64b element.
     * 
     * @return The node longitude latitude micro degree 64b element.
     */
    public NodeLLmD64B getNodeLatLong() {

        return nodeLatLong;
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeOffsetPointXYBits = new StringBuilder();

        if (nodeXY20B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(0, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY20B.encodeUPER());
        }
        else if (nodeXY22B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(1, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY22B.encodeUPER());
        }
        else if (nodeXY24B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(2, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY24B.encodeUPER());
        }
        else if (nodeXY26B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(3, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY26B.encodeUPER());
        }
        else if (nodeXY28B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(4, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY28B.encodeUPER());
        }
        else if (nodeXY32B != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(5, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeXY32B.encodeUPER());
        }
        else if (nodeLatLong != null) {

            nodeOffsetPointXYBits.append(UPERInteger.encode(6, 0, NodeOffsetPointXY.NUM_BITS));
            nodeOffsetPointXYBits.append(nodeLatLong.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return nodeOffsetPointXYBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeOffsetPointXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeOffsetPointXY frame (%d)", NodeOffsetPointXY.NUM_BITS));
        }

        String nodeOffsetPointXYChoiceBits = bits.substring(0, NodeOffsetPointXY.NUM_BITS);
        bits = bits.substring(NodeOffsetPointXY.NUM_BITS);

        int choice = UPERInteger.decode(nodeOffsetPointXYChoiceBits, 0);

        switch (choice) {

            case 0:
                nodeXY20B = new NodeXY20B();
                bits = nodeXY20B.decodeUPER(bits);
                break;
            case 1:
                nodeXY22B = new NodeXY22B();
                bits = nodeXY22B.decodeUPER(bits);
                break;
            case 2:
                nodeXY24B = new NodeXY24B();
                bits = nodeXY24B.decodeUPER(bits);
                break;
            case 3:
                nodeXY26B = new NodeXY26B();
                bits = nodeXY26B.decodeUPER(bits);
                break;
            case 4:
                nodeXY28B = new NodeXY28B();
                bits = nodeXY28B.decodeUPER(bits);
                break;
            case 5:
                nodeXY32B = new NodeXY32B();
                bits = nodeXY32B.decodeUPER(bits);
                break;
            case 6:
                nodeLatLong = new NodeLLmD64B();
                bits = nodeLatLong.decodeUPER(bits);
                break;
            case 7:
                throw new IllegalArgumentException("The NodeOffsetPointXY regional extension is not supported");
            default:
                throw new IllegalArgumentException(String.format("There should not be any more possible integers that would be returned, but received %d.", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(nodeXY20B, nodeXY22B, nodeXY24B, nodeXY26B, nodeXY28B, nodeXY32B, nodeLatLong);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeOffsetPointXY)) {

            return false;
        }
        NodeOffsetPointXY frame = (NodeOffsetPointXY)object;
        return Objects.equals(this.nodeXY20B, frame.nodeXY20B)
                && Objects.equals(this.nodeXY22B, frame.nodeXY22B)
                && Objects.equals(this.nodeXY24B, frame.nodeXY24B)
                && Objects.equals(this.nodeXY26B, frame.nodeXY26B)
                && Objects.equals(this.nodeXY28B, frame.nodeXY28B)
                && Objects.equals(this.nodeXY32B, frame.nodeXY32B)
                && Objects.equals(this.nodeLatLong, frame.nodeLatLong);
    }
}
