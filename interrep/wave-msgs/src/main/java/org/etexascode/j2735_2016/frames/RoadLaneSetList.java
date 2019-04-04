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
 * The road lane set list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RoadLaneSetList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the road lane set list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 8;

    /**
     * The maximum size of the road lane set list.
     */
    public static final int MAX_LIST_SIZE = 255;

    /**
     * The minimum size of the road lane set list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The road lane set list.
     */
    private GenericLane[] lanes;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RoadLaneSetList() {

        lanes = new GenericLane[0];
    }

    /**
     * A constructor for the road lane set list frame, where the size will be validated.
     * 
     * @param size The road lane set list size to set.
     */
    public RoadLaneSetList(int size) {

        this.lanes = new GenericLane[validate(size)];
    }

    /**
     * A getter for the lane array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The lane array.
     */
    public GenericLane[] getLaneArray() {

        return lanes;
    }

    /**
     * Validates the road lane set list size.
     * 
     * @param size The size to be validated.
     * @return The road lane set list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < RoadLaneSetList.MIN_LIST_SIZE || size > RoadLaneSetList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the road lane set list frame must be in the range of %d to %d", RoadLaneSetList.MIN_LIST_SIZE, RoadLaneSetList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (lanes.length < RoadLaneSetList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough GenericLane objects to encode the list. minimum: %d", RoadLaneSetList.MIN_LIST_SIZE));
        }

        StringBuilder roadLaneSetListBits = new StringBuilder(UPERInteger.encode(lanes.length, RoadLaneSetList.MIN_LIST_SIZE, RoadLaneSetList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < lanes.length; i++) {

            if (lanes[i] == null) {

                throw new IllegalStateException(String.format("The road lane set list frame was not filled up to the amount specified. specified: %d, received: %d", lanes.length, i + 1));
            }
            roadLaneSetListBits.append(lanes[i].encodeUPER());
        }
        return roadLaneSetListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RoadLaneSetList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RoadLaneSetList frame (%d)", RoadLaneSetList.NUM_BITS_LIST_SIZE));
        }
        // 00000000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, RoadLaneSetList.NUM_BITS_LIST_SIZE), RoadLaneSetList.MIN_LIST_SIZE));
        bits = bits.substring(RoadLaneSetList.NUM_BITS_LIST_SIZE);

        lanes = new GenericLane[size];
        for (int i = 0; i < size; i++) {

            GenericLane lane = new GenericLane();
            bits = lane.decodeUPER(bits);
            lanes[i] = lane;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(lanes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RoadLaneSetList)) {

            return false;
        }
        RoadLaneSetList frame = (RoadLaneSetList)object;
        return Arrays.equals(this.lanes, frame.lanes);
    }
}
