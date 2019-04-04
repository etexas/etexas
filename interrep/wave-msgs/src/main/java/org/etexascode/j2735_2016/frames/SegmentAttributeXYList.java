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

import org.etexascode.j2735_2016.elements.SegmentAttributeXY;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The segment attribute xy list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SegmentAttributeXYList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the segment attribute xy list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 3;

    /**
     * The maximum size of the segment attribute xy list.
     */
    public static final int MAX_LIST_SIZE = 8;

    /**
     * The minimum size of the segment attribute xy list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The segment attribute xy list.
     */
    private SegmentAttributeXY[] attributes;

    /**
     * A constructor setup only for decoding purposes.
     */
    public SegmentAttributeXYList() {

        attributes = new SegmentAttributeXY[0];
    }

    /**
     * A constructor for the segment attribute xy list frame, where the size will be validated.
     * 
     * @param size The segment attribute xy list size to set.
     */
    public SegmentAttributeXYList(int size) {

        this.attributes = new SegmentAttributeXY[validate(size)];
    }

    /**
     * A getter for the attribute array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The attribute array.
     */
    public SegmentAttributeXY[] getAttributeArray() {

        return attributes;
    }

    /**
     * Validates the segment attribute xy list size.
     * 
     * @param size The size to be validated.
     * @return The segment attribute xy list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < SegmentAttributeXYList.MIN_LIST_SIZE || size > SegmentAttributeXYList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the segment attribute xy list frame must be in the range of %d to %d", SegmentAttributeXYList.MIN_LIST_SIZE, SegmentAttributeXYList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (attributes.length < SegmentAttributeXYList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough SegmentAttributeXY objects to encode the list. minimum: %d", SegmentAttributeXYList.MIN_LIST_SIZE));
        }

        StringBuilder attributesListBits = new StringBuilder(UPERInteger.encode(attributes.length, SegmentAttributeXYList.MIN_LIST_SIZE, SegmentAttributeXYList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < attributes.length; i++) {

            if (attributes[i] == null) {

                throw new IllegalStateException(String.format("The segment attribute xy list frame was not filled up to the amount specified. specified: %d, received: %d", attributes.length, i + 1));
            }
            attributesListBits.append(attributes[i].encodeUPER());
        }
        return attributesListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (SegmentAttributeXYList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SegmentAttributeXYList frame (%d)", SegmentAttributeXYList.NUM_BITS_LIST_SIZE));
        }
        // 000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, SegmentAttributeXYList.NUM_BITS_LIST_SIZE), SegmentAttributeXYList.MIN_LIST_SIZE));
        bits = bits.substring(SegmentAttributeXYList.NUM_BITS_LIST_SIZE);

        attributes = new SegmentAttributeXY[size];
        for (int i = 0; i < size; i++) {

            SegmentAttributeXY attribute = new SegmentAttributeXY();
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
        if (!(object instanceof SegmentAttributeXYList)) {

            return false;
        }
        SegmentAttributeXYList frame = (SegmentAttributeXYList)object;
        return Arrays.equals(this.attributes, frame.attributes);
    }
}
