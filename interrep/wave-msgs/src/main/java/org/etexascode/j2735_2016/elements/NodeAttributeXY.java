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
package org.etexascode.j2735_2016.elements;

import java.util.Objects;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The node attribute XY element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class NodeAttributeXY implements UnalignedPackedEncodingRules {

    /**
     * The enum of NodeAttributeXY. This is created inside of the class because NodeAttributeXY is
     * an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum NodeAttribute {
        RESERVED(0),
        STOP_LINE(1),
        ROUNDED_CAP_STYLE_A(2),
        ROUNDED_CAP_STYLE_B(3),
        MERGE_POINT(4),
        DIVERGE_POINT(5),
        DOWNSTREAM_STOP_LINE(6),
        DOWNSTREAM_START_NODE(7),
        CLOSED_TO_TRAFFIC(8),
        SAFE_ISLAND(9),
        CURB_PRESENT_AT_STEP_OFF(10),
        HYDRANT_PRESENT(11);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>NodeAttribute</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private NodeAttribute(int number) {

            this.number = number;
        }

        /**
         * Gets the integer representation of this enumeration.
         * 
         * @return The number.
         */
        public int getNumber() {

            return number;
        }
    }

    /**
     * The number of bits that cover a node attribute XY.
     */
    public static final int NUM_BITS = 5;

    /**
     * The node attribute enumeration.
     */
    private NodeAttribute enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public NodeAttributeXY() {

        enumeration = NodeAttribute.RESERVED;
    }

    /**
     * A constructor for the node attribute XY element.
     * 
     * @param enumeration The node attribute enumeration to set.
     */
    public NodeAttributeXY(NodeAttribute enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the node attribute enumeration.
     * 
     * @return The node attribute enumeration.
     */
    public NodeAttribute getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the node attribute enumeration.
     * 
     * @param enumeration The node attribute enumeration to set.
     */
    public void setEnumeration(NodeAttribute enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, NodeAttributeXY.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (NodeAttributeXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a NodeAttribute element (%d)", NodeAttributeXY.NUM_BITS));
        }

        String nodeAttributeXYBits = bits.substring(0, NodeAttributeXY.NUM_BITS);

        if (nodeAttributeXYBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The NodeAttributeXY extension is not supported");
        }

        int attributeNum = UPERInteger.decode(nodeAttributeXYBits.substring(1), 0);
        boolean found = false;
        for (NodeAttribute attribute : NodeAttribute.values()) {

            if (attributeNum == attribute.getNumber()) {

                enumeration = attribute;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known NodeAttribute enumeration");
        }
        return bits.substring(NodeAttributeXY.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hashCode(enumeration);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof NodeAttributeXY)) {

            return false;
        }
        NodeAttributeXY element = (NodeAttributeXY)object;
        return this.enumeration.equals(element.enumeration);
    }
}
