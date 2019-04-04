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
 * The layer type element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LayerType implements UnalignedPackedEncodingRules {

    /**
     * The enum of LayerType. This is created inside of the class because LayerType is an enum but I
     * needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum Layer {
        NONE(0),
        MIXED_CONTENT(1),
        GENERAL_MAP_DATA(2),
        INTERSECTION_DATA(3),
        CURVE_DATA(4),
        ROADWAY_SECTION_DATA(5),
        PARKING_AREA_DATA(6),
        SHARED_LANE_DATA(7);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>Layer</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private Layer(int number) {

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
     * The number of bits that cover a layer type.
     */
    public static final int NUM_BITS = 4;

    /**
     * The layer enumeration.
     */
    private Layer enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public LayerType() {

        enumeration = Layer.NONE;
    }

    /**
     * A constructor for the layer type element.
     * 
     * @param enumeration The layer enumeration to set.
     */
    public LayerType(Layer enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the layer enumeration.
     * 
     * @return The layer enumeration.
     */
    public Layer getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the layer enumeration.
     * 
     * @param enumeration The layer enumeration to set.
     */
    public void setEnumeration(Layer enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, LayerType.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (LayerType.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LayerType element (%d)", LayerType.NUM_BITS));
        }

        String layerTypeBits = bits.substring(0, LayerType.NUM_BITS);

        if (layerTypeBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The LayerType extension is not supported");
        }

        int layerNum = UPERInteger.decode(layerTypeBits.substring(1), 0);
        for (Layer layer : Layer.values()) {

            if (layerNum == layer.getNumber()) {

                enumeration = layer;
            }
        }
        return bits.substring(LayerType.NUM_BITS);
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
        if (!(object instanceof LayerType)) {

            return false;
        }
        LayerType element = (LayerType)object;
        return this.enumeration.equals(element.enumeration);
    }
}
