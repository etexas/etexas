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
 * The intersection geometry list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionGeometryList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the intersection geometry list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 5;

    /**
     * The maximum size of the intersection geometry list.
     */
    public static final int MAX_LIST_SIZE = 32;

    /**
     * The minimum size of the intersection geometry list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The intersection geometry list.
     */
    private IntersectionGeometry[] intersections;

    /**
     * A constructor setup only for decoding purposes.
     */
    public IntersectionGeometryList() {

        intersections = new IntersectionGeometry[0];
    }

    /**
     * A constructor for the intersection geometry list frame, where the size will be validated.
     * 
     * @param size The intersection geometry list size to set.
     */
    public IntersectionGeometryList(int size) {

        this.intersections = new IntersectionGeometry[validate(size)];
    }

    /**
     * A getter for the intersection geometry array. NOTE: Changes to the array will be reflected
     * here as well.
     * 
     * @return The intersection geometry array.
     */
    public IntersectionGeometry[] getIntersectionGeometryArray() {

        return intersections;
    }

    /**
     * Validates the intersection geometry list size.
     * 
     * @param size The size to be validated.
     * @return The intersection geometry list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < IntersectionGeometryList.MIN_LIST_SIZE || size > IntersectionGeometryList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the intersection geometry list frame must be in the range of %d to %d", IntersectionGeometryList.MIN_LIST_SIZE, IntersectionGeometryList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (intersections.length < IntersectionGeometryList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough IntersectionGeometry objects to encode the list. minimum: %d", IntersectionGeometryList.MIN_LIST_SIZE));
        }

        StringBuilder intersectionGeometryListBits = new StringBuilder(UPERInteger.encode(intersections.length, IntersectionGeometryList.MIN_LIST_SIZE, IntersectionGeometryList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < intersections.length; i++) {

            if (intersections[i] == null) {

                throw new IllegalStateException(
                        String.format("The intersection geometry list frame was not filled up to the amount specified. specified: %d, received: %d", intersections.length, i + 1));
            }
            intersectionGeometryListBits.append(intersections[i].encodeUPER());
        }
        return intersectionGeometryListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionGeometryList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionGeometryList frame (%d)", IntersectionGeometryList.NUM_BITS_LIST_SIZE));
        }
        // 00000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, IntersectionGeometryList.NUM_BITS_LIST_SIZE), IntersectionGeometryList.MIN_LIST_SIZE));
        bits = bits.substring(IntersectionGeometryList.NUM_BITS_LIST_SIZE);

        intersections = new IntersectionGeometry[size];
        for (int i = 0; i < size; i++) {

            IntersectionGeometry intersection = new IntersectionGeometry();
            bits = intersection.decodeUPER(bits);
            intersections[i] = intersection;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(intersections);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof IntersectionGeometryList)) {

            return false;
        }
        IntersectionGeometryList frame = (IntersectionGeometryList)object;
        return Arrays.equals(this.intersections, frame.intersections);
    }
}
