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
 * The advisory speed list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AdvisorySpeedList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the advisory speed list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the advisory speed list.
     */
    public static final int MAX_LIST_SIZE = 16;

    /**
     * The minimum size of the advisory speed list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The advisory speed list.
     */
    private AdvisorySpeed[] speeds;

    /**
     * A constructor setup only for decoding purposes.
     */
    public AdvisorySpeedList() {

        speeds = new AdvisorySpeed[0];
    }

    /**
     * A constructor for the advisory speed list frame, where the size will be validated.
     * 
     * @param size The advisory speed list size to set.
     */
    public AdvisorySpeedList(int size) {

        this.speeds = new AdvisorySpeed[validate(size)];
    }

    /**
     * A getter for the advisory speed array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The advisory speed array.
     */
    public AdvisorySpeed[] getAdvisorySpeedArray() {

        return speeds;
    }

    /**
     * Validates the advisory speed list size.
     * 
     * @param size The size to be validated.
     * @return The advisory speed list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < AdvisorySpeedList.MIN_LIST_SIZE || size > AdvisorySpeedList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the advisory speed list frame must be in the range of %d to %d", AdvisorySpeedList.MIN_LIST_SIZE, AdvisorySpeedList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (speeds.length < AdvisorySpeedList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough AdvisorySpeed objects to encode the list. minimum: %d", AdvisorySpeedList.MIN_LIST_SIZE));
        }

        StringBuilder advisorySpeedListBits = new StringBuilder(UPERInteger.encode(speeds.length, AdvisorySpeedList.MIN_LIST_SIZE, AdvisorySpeedList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < speeds.length; i++) {

            if (speeds[i] == null) {

                throw new IllegalStateException(String.format("The advisory speed list frame was not filled up to the amount specified. specified: %d, received: %d", speeds.length, i + 1));
            }
            advisorySpeedListBits.append(speeds[i].encodeUPER());
        }
        return advisorySpeedListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (AdvisorySpeedList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AdvisorySpeedList frame (%d)", AdvisorySpeedList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, AdvisorySpeedList.NUM_BITS_LIST_SIZE), AdvisorySpeedList.MIN_LIST_SIZE));
        bits = bits.substring(AdvisorySpeedList.NUM_BITS_LIST_SIZE);

        speeds = new AdvisorySpeed[size];
        for (int i = 0; i < size; i++) {

            AdvisorySpeed speed = new AdvisorySpeed();
            bits = speed.decodeUPER(bits);
            speeds[i] = speed;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(speeds);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof AdvisorySpeedList)) {

            return false;
        }
        AdvisorySpeedList frame = (AdvisorySpeedList)object;
        return Arrays.equals(this.speeds, frame.speeds);
    }
}
