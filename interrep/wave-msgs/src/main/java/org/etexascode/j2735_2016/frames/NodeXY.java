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

/**
 * The node xy frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeXY implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the node xy.
     */
    public static final int NUM_BITS = 2;

    /**
     * The node offset point xy frame.
     */
    private NodeOffsetPointXY delta;

    /**
     * The node attribute set xy frame. (OPTIONAL)
     */
    private NodeAttributeSetXY attributes;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public NodeXY() {

        delta = new NodeOffsetPointXY();
    }

    /**
     * A constructor for the node xy frame for all required fields.
     * 
     * @param delta The node offset point xy frame.
     */
    public NodeXY(NodeOffsetPointXY delta) {

        this.delta = Objects.requireNonNull(delta);
    }

    /**
     * A getter for the node offset point xy frame.
     * 
     * @return The node offset point xy frame.
     */
    public NodeOffsetPointXY getDelta() {

        return delta;
    }

    /**
     * A setter for the node offset point xy frame.
     * 
     * @param delta The node offset point xy frame to set.
     */
    public void setDelta(NodeOffsetPointXY delta) {

        this.delta = Objects.requireNonNull(delta);
    }

    /**
     * A getter for the node attribute set xy frame.
     * 
     * @return The node attribute set xy frame.
     */
    public NodeAttributeSetXY getAttributes() {

        return attributes;
    }

    /**
     * A setter for the node attribute set xy frame.
     * 
     * @param attributes The node attribute set xy frame.
     */
    public void setAttributes(NodeAttributeSetXY attributes) {

        this.attributes = attributes;
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeXYBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        nodeXYBits.append(delta.encodeUPER());

        if (attributes != null) {

            optionalBits.append('1');
            nodeXYBits.append(attributes.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        nodeXYBits.insert(0, optionalBits);

        return nodeXYBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeXY frame (%d)", NodeXY.NUM_BITS));
        }

        String nodeXYOptionalBits = bits.substring(0, NodeXY.NUM_BITS);
        bits = bits.substring(NodeXY.NUM_BITS);

        if (nodeXYOptionalBits.charAt(0) == '1') {

            throw new IllegalArgumentException("The NodeXY extension is not supported");
        }

        bits = delta.decodeUPER(bits);

        if (nodeXYOptionalBits.charAt(1) == '1') {

            attributes = new NodeAttributeSetXY();
            bits = attributes.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(delta, attributes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeXY)) {

            return false;
        }
        NodeXY frame = (NodeXY)object;
        return this.delta.equals(frame.delta)
                && Objects.equals(this.attributes, frame.attributes);
    }
}
