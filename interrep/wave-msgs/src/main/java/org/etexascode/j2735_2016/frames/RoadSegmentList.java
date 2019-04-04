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
 * The road segment list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RoadSegmentList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the road segment list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 5;

    /**
     * The maximum size of the road segment list.
     */
    public static final int MAX_LIST_SIZE = 32;

    /**
     * The minimum size of the road segment list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The road segment list.
     */
    private RoadSegment[] segments;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RoadSegmentList() {

        segments = new RoadSegment[0];
    }

    /**
     * A constructor for the road segment list frame, where the size will be validated.
     * 
     * @param size The road segment list size to set.
     */
    public RoadSegmentList(int size) {

        this.segments = new RoadSegment[validate(size)];
    }

    /**
     * A getter for the road segment array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The road segment array.
     */
    public RoadSegment[] getRoadSegmentArray() {

        return segments;
    }

    /**
     * Validates the road segment list size.
     * 
     * @param size The size to be validated.
     * @return The road segment list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < RoadSegmentList.MIN_LIST_SIZE || size > RoadSegmentList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(String.format("The size of the road segment list frame must be in the range of %d to %d", RoadSegmentList.MIN_LIST_SIZE, RoadSegmentList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (segments.length < RoadSegmentList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough RoadSegment objects to encode the list. minimum: %d", RoadSegmentList.MIN_LIST_SIZE));
        }

        StringBuilder roadSegmentListBits = new StringBuilder(UPERInteger.encode(segments.length, RoadSegmentList.MIN_LIST_SIZE, RoadSegmentList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < segments.length; i++) {

            if (segments[i] == null) {

                throw new IllegalStateException(String.format("The road segment list frame was not filled up to the amount specified. specified: %d, received: %d", segments.length, i + 1));
            }
            roadSegmentListBits.append(segments[i].encodeUPER());
        }
        return roadSegmentListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RoadSegmentList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RoadSegmentList frame (%d)", RoadSegmentList.NUM_BITS_LIST_SIZE));
        }
        // 00000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, RoadSegmentList.NUM_BITS_LIST_SIZE), RoadSegmentList.MIN_LIST_SIZE));
        bits = bits.substring(RoadSegmentList.NUM_BITS_LIST_SIZE);

        segments = new RoadSegment[size];
        for (int i = 0; i < size; i++) {

            RoadSegment segment = new RoadSegment();
            bits = segment.decodeUPER(bits);
            segments[i] = segment;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(segments);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RoadSegmentList)) {

            return false;
        }
        RoadSegmentList frame = (RoadSegmentList)object;
        return Arrays.equals(this.segments, frame.segments);
    }
}
