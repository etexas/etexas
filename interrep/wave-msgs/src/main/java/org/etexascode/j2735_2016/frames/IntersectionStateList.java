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
 * The intersection state list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionStateList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the intersection state list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 5;

    /**
     * The maximum size of the intersection state list.
     */
    public static final int MAX_LIST_SIZE = 32;

    /**
     * The minimum size of the intersection state list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The intersection state list.
     */
    private IntersectionState[] intersections;

    /**
     * A constructor setup only for decoding purposes.
     */
    public IntersectionStateList() {

        intersections = new IntersectionState[0];
    }

    /**
     * A constructor for the intersection state list frame, where the size will be validated.
     * 
     * @param size The intersection state list size to set.
     */
    public IntersectionStateList(int size) {

        this.intersections = new IntersectionState[validate(size)];
    }

    /**
     * A getter for the intersection state array. NOTE: Changes to the array will be reflected here
     * as well.
     * 
     * @return The movement array.
     */
    public IntersectionState[] getIntersectionStateArray() {

        return intersections;
    }

    /**
     * Validates the intersection state list size.
     * 
     * @param size The size to be validated.
     * @return The intersection state list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < IntersectionStateList.MIN_LIST_SIZE || size > IntersectionStateList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the intersection state list frame must be in the range of %d to %d", IntersectionStateList.MIN_LIST_SIZE, IntersectionStateList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (intersections.length < IntersectionStateList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough IntersectionState objects to encode the list. minimum: %d", IntersectionStateList.MIN_LIST_SIZE));
        }

        StringBuilder intersectionStateListBits = new StringBuilder(UPERInteger.encode(intersections.length, IntersectionStateList.MIN_LIST_SIZE, IntersectionStateList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < intersections.length; i++) {

            if (intersections[i] == null) {

                throw new IllegalStateException(String.format("The intersection state list frame was not filled up to the amount specified. specified: %d, received: %d", intersections.length, i + 1));
            }
            intersectionStateListBits.append(intersections[i].encodeUPER());
        }
        return intersectionStateListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionStateList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionStateList frame (%d)", IntersectionStateList.NUM_BITS_LIST_SIZE));
        }
        // 00000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, IntersectionStateList.NUM_BITS_LIST_SIZE), IntersectionStateList.MIN_LIST_SIZE));
        bits = bits.substring(IntersectionStateList.NUM_BITS_LIST_SIZE);

        intersections = new IntersectionState[size];
        for (int i = 0; i < size; i++) {

            IntersectionState interseection = new IntersectionState();
            bits = interseection.decodeUPER(bits);
            intersections[i] = interseection;
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
        if (!(object instanceof IntersectionStateList)) {

            return false;
        }
        IntersectionStateList frame = (IntersectionStateList)object;
        return Arrays.equals(this.intersections, frame.intersections);
    }
}
