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
 * The lane data attribute list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneDataAttributeList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the lane data attribute list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 3;

    /**
     * The maximum size of the lane data attribute list.
     */
    public static final int MAX_LIST_SIZE = 8;

    /**
     * The minimum size of the lane data attribute list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The lane data attribute list.
     */
    private LaneDataAttribute[] attributes;

    /**
     * A constructor setup only for decoding purposes.
     */
    public LaneDataAttributeList() {

        attributes = new LaneDataAttribute[0];
    }

    /**
     * A constructor for the lane data attribute list frame, where the size will be validated.
     * 
     * @param size The lane data attribute list size to set.
     */
    public LaneDataAttributeList(int size) {

        this.attributes = new LaneDataAttribute[validate(size)];
    }

    /**
     * A getter for the attribute array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The attribute array.
     */
    public LaneDataAttribute[] getAttributeArray() {

        return attributes;
    }

    /**
     * Validates the lane data attribute list size.
     * 
     * @param size The size to be validated.
     * @return The lane data attribute list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < LaneDataAttributeList.MIN_LIST_SIZE || size > LaneDataAttributeList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the lane data attribute list frame must be in the range of %d to %d", LaneDataAttributeList.MIN_LIST_SIZE, LaneDataAttributeList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (attributes.length < LaneDataAttributeList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough LaneDataAttribute objects to encode the list. minimum: %d", LaneDataAttributeList.MIN_LIST_SIZE));
        }

        StringBuilder attributesListBits = new StringBuilder(UPERInteger.encode(attributes.length, LaneDataAttributeList.MIN_LIST_SIZE, LaneDataAttributeList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < attributes.length; i++) {

            if (attributes[i] == null) {

                throw new IllegalStateException(String.format("The lane data attribute list frame was not filled up to the amount specified. specified: %d, received: %d", attributes.length, i + 1));
            }
            attributesListBits.append(attributes[i].encodeUPER());
        }
        return attributesListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneDataAttributeList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneDataAttributeList frame (%d)", LaneDataAttributeList.NUM_BITS_LIST_SIZE));
        }
        // 000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, LaneDataAttributeList.NUM_BITS_LIST_SIZE), LaneDataAttributeList.MIN_LIST_SIZE));
        bits = bits.substring(LaneDataAttributeList.NUM_BITS_LIST_SIZE);

        attributes = new LaneDataAttribute[size];
        for (int i = 0; i < size; i++) {

            LaneDataAttribute attribute = new LaneDataAttribute();
            bits = attribute.decodeUPER(bits);
            attributes[i] = attribute;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(attributes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneDataAttributeList)) {

            return false;
        }
        LaneDataAttributeList frame = (LaneDataAttributeList)object;
        return Arrays.equals(this.attributes, frame.attributes);
    }
}
