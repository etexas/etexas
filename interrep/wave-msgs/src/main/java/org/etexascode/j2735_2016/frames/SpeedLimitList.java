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
 * The speed limit list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SpeedLimitList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the speed limit list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the speed limit list.
     */
    public static final int MAX_LIST_SIZE = 9;

    /**
     * The minimum size of the speed limit list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The speed limit list.
     */
    private RegulatorySpeedLimit[] speedLimits;

    /**
     * A constructor setup only for decoding purposes.
     */
    public SpeedLimitList() {

        speedLimits = new RegulatorySpeedLimit[0];
    }

    /**
     * A constructor for the speed limit list frame, where the size will be validated.
     * 
     * @param size The speed limit list size to set.
     */
    public SpeedLimitList(int size) {

        this.speedLimits = new RegulatorySpeedLimit[validate(size)];
    }

    /**
     * A getter for the speed limit array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The speed limit array.
     */
    public RegulatorySpeedLimit[] getSpeedLimitArray() {

        return speedLimits;
    }

    /**
     * Validates the speed limit list size.
     * 
     * @param size The size to be validated.
     * @return The speed limit list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < SpeedLimitList.MIN_LIST_SIZE || size > SpeedLimitList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(String.format("The size of the speed limit list frame must be in the range of %d to %d", SpeedLimitList.MIN_LIST_SIZE, SpeedLimitList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (speedLimits.length < SpeedLimitList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough RegulatorySpeedLimit objects to encode the list. minimum: %d", SpeedLimitList.MIN_LIST_SIZE));
        }

        StringBuilder speedLimitListBits = new StringBuilder(UPERInteger.encode(speedLimits.length, SpeedLimitList.MIN_LIST_SIZE, SpeedLimitList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < speedLimits.length; i++) {

            if (speedLimits[i] == null) {

                throw new IllegalStateException(String.format("The speed limit list frame was not filled up to the amount specified. specified: %d, received: %d", speedLimits.length, i + 1));
            }
            speedLimitListBits.append(speedLimits[i].encodeUPER());
        }
        return speedLimitListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (SpeedLimitList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SpeedLimitList frame (%d)", SpeedLimitList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, SpeedLimitList.NUM_BITS_LIST_SIZE), SpeedLimitList.MIN_LIST_SIZE));

        bits = bits.substring(SpeedLimitList.NUM_BITS_LIST_SIZE);

        speedLimits = new RegulatorySpeedLimit[size];
        for (int i = 0; i < size; i++) {

            RegulatorySpeedLimit speedLimit = new RegulatorySpeedLimit();
            bits = speedLimit.decodeUPER(bits);
            speedLimits[i] = speedLimit;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(speedLimits);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof SpeedLimitList)) {

            return false;
        }
        SpeedLimitList frame = (SpeedLimitList)object;
        return Arrays.equals(this.speedLimits, frame.speedLimits);
    }
}
