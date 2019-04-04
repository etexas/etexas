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

import org.etexascode.j2735_2016.elements.OffsetB10;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The node attribute set xy frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeAttributeSetXY implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the node attribute set xy.
     */
    public static final int NUM_BITS = 8;

    /**
     * The node attribute xy list frame. (OPTIONAL)
     */
    private NodeAttributeXYList localNode;

    /**
     * The disabled segment attribute xy list frame. (OPTIONAL)
     */
    private SegmentAttributeXYList disabled;

    /**
     * The enabled segment attribute xy list frame. (OPTIONAL)
     */
    private SegmentAttributeXYList enabled;

    /**
     * The lane data attribute list frame. (OPTIONAL)
     */
    private LaneDataAttributeList data;

    /**
     * The lane node width offset b10 element. (OPTIONAL)
     */
    private OffsetB10 dWidth;

    /**
     * The lane node elevation offset b10 element. (OPTIONAL)
     */
    private OffsetB10 dElevation;

    /**
     * A constructor for the node attribute set xy frame.
     */
    public NodeAttributeSetXY() {
        // since all the fields are optional, nothing needs to be done here.
    }

    /**
     * A getter for the node attribute xy list frame.
     * 
     * @return The node attribute xy list frame.
     */
    public NodeAttributeXYList getLocalNode() {

        return localNode;
    }

    /**
     * A setter for the node attribute xy list frame.
     * 
     * @param localNode The node attribute xy list frame to set.
     */
    public void setLocalNode(NodeAttributeXYList localNode) {

        this.localNode = localNode;
    }

    /**
     * A getter for the disabled segment attribute xy list frame.
     * 
     * @return The disabled segment attribute xy list frame.
     */
    public SegmentAttributeXYList getDisabled() {

        return disabled;
    }

    /**
     * A setter for the disabled segment attribute xy list frame.
     * 
     * @param disabled The disabled segment attribute xy list to set.
     */
    public void setDisabled(SegmentAttributeXYList disabled) {

        this.disabled = disabled;
    }

    /**
     * A getter for the enabled segment attribute xy list frame.
     * 
     * @return The enabled segment attribute xy list frame.
     */
    public SegmentAttributeXYList getEnabled() {

        return enabled;
    }

    /**
     * A setter for the enabled segment attribute xy list frame.
     * 
     * @param enabled The enabled segment attribute xy list frame to set.
     */
    public void setEnabled(SegmentAttributeXYList enabled) {

        this.enabled = enabled;
    }

    /**
     * A getter for the lane data attribute list frame.
     * 
     * @return The lane data attribute list frame.
     */
    public LaneDataAttributeList getData() {

        return data;
    }

    /**
     * A setter for the lane data attribute list frame.
     * 
     * @param data The lane data attribute list frame to set.
     */
    public void setData(LaneDataAttributeList data) {

        this.data = data;
    }

    /**
     * A getter for the lane node width offset b10 element.
     * 
     * @return The lane node width offset b10 element.
     */
    public OffsetB10 getDWidth() {

        return dWidth;
    }

    /**
     * A setter for the lane node width offset b10 element.
     * 
     * @param dWidth The lane node width offset b10 element to set.
     */
    public void setDWidth(OffsetB10 dWidth) {

        this.dWidth = dWidth;
    }

    /**
     * A setter for the lane node width value.
     * 
     * @param dWidth The lane node width value.
     */
    public void setDWidth(int dWidth) {

        if (this.dWidth == null) {

            this.dWidth = new OffsetB10();
        }
        this.dWidth.setValue(dWidth);
    }

    /**
     * A getter for the lane node elevation offset b10 element.
     * 
     * @return The lane node elevation offset b10 element.
     */
    public OffsetB10 getDElevation() {

        return dElevation;
    }

    /**
     * A setter for the lane node elevation offset b10 element.
     * 
     * @param dElevation The lane node elevation offset b10 element to set.
     */
    public void setDElevation(OffsetB10 dElevation) {

        this.dElevation = dElevation;
    }

    /**
     * A setter for the lane node elevation value.
     * 
     * @param dElevation The lane node elevation value to set.
     */
    public void setDElevation(int dElevation) {

        if (this.dElevation == null) {

            this.dElevation = new OffsetB10();
        }
        this.dElevation.setValue(dElevation);
    }

    @Override
    public String encodeUPER() {

        StringBuilder nodeAttributeSetXYBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        if (localNode != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(localNode.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (disabled != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(disabled.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (enabled != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(enabled.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (data != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(data.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (dWidth != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(dWidth.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (dElevation != null) {

            optionalBits.append('1');
            nodeAttributeSetXYBits.append(dElevation.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');

        nodeAttributeSetXYBits.insert(0, optionalBits);

        return nodeAttributeSetXYBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeAttributeSetXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeAttributeSetXY frame (%d)", NodeAttributeSetXY.NUM_BITS));
        }

        String nodeAttributeSetXYOptionalBits = bits.substring(0, NodeAttributeSetXY.NUM_BITS);
        bits = bits.substring(NodeAttributeSetXY.NUM_BITS);

        if (nodeAttributeSetXYOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The NodeAttributeSetXY extension is not supported");
        }

        if (nodeAttributeSetXYOptionalBits.charAt(7) != '0') {

            throw new IllegalArgumentException("The NodeAttributeSetXY regional extension is not supported");
        }

        if (nodeAttributeSetXYOptionalBits.charAt(1) == '1') {

            localNode = new NodeAttributeXYList();
            bits = localNode.decodeUPER(bits);
        }

        if (nodeAttributeSetXYOptionalBits.charAt(2) == '1') {

            disabled = new SegmentAttributeXYList();
            bits = disabled.decodeUPER(bits);
        }

        if (nodeAttributeSetXYOptionalBits.charAt(3) == '1') {

            enabled = new SegmentAttributeXYList();
            bits = enabled.decodeUPER(bits);
        }

        if (nodeAttributeSetXYOptionalBits.charAt(4) == '1') {

            data = new LaneDataAttributeList();
            bits = data.decodeUPER(bits);
        }

        if (nodeAttributeSetXYOptionalBits.charAt(5) == '1') {

            dWidth = new OffsetB10();
            bits = dWidth.decodeUPER(bits);
        }

        if (nodeAttributeSetXYOptionalBits.charAt(6) == '1') {

            dElevation = new OffsetB10();
            bits = dElevation.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(localNode, disabled, enabled, data, dWidth, dElevation);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeAttributeSetXY)) {

            return false;
        }
        NodeAttributeSetXY frame = (NodeAttributeSetXY)object;
        return Objects.equals(this.localNode, frame.localNode)
                && Objects.equals(this.disabled, frame.disabled)
                && Objects.equals(this.enabled, frame.enabled)
                && Objects.equals(this.data, frame.data)
                && Objects.equals(this.dWidth, frame.dWidth)
                && Objects.equals(this.dElevation, frame.dElevation);
    }
}
