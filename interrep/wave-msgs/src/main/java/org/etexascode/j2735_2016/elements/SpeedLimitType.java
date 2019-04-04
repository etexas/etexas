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
 * The speed limit type element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SpeedLimitType implements UnalignedPackedEncodingRules {

    /**
     * The enum of SpeedLimitType. This is created inside of the class because SpeedLimitType is an
     * enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum SpeedLimit {
        UNKNOWN(0),
        MAX_SPEED_IN_SCHOOL_ZONE(1),
        MAX_SPEED_IN_SCHOOL_ZONE_WHEN_CHILDREN_ARE_PRESENT(2),
        MAX_SPEED_IN_CONSTRUCTION_ZONE(3),
        VEHICLE_MIN_SPEED(4),
        VEHICLE_MAX_SPEED(5),
        VEHICLE_NIGHT_MAX_SPEED(6),
        TRUCK_MIN_SPEED(7),
        TRUCK_MAX_SPEED(8),
        TRUCK_NIGHT_MAX_SPEED(9),
        VEHICLES_WITH_TRAILERS_MIN_SPEED(10),
        VEHICLES_WITH_TRAILERS_MAX_SPEED(11),
        VEHICLES_WITH_TRAILERS_NIGHT_MAX_SPEED(12);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>SpeedLimit</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private SpeedLimit(int number) {

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
     * The number of bits that cover a speed limit type.
     */
    public static final int NUM_BITS = 5;

    /**
     * The speed limit enumeration.
     */
    private SpeedLimit enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SpeedLimitType() {

        enumeration = SpeedLimit.UNKNOWN;
    }

    /**
     * A constructor for the speed limit type element.
     * 
     * @param enumeration The speed limit enumeration to set.
     */
    public SpeedLimitType(SpeedLimit enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the speed limit enumeration.
     * 
     * @return The speed limit enumeration.
     */
    public SpeedLimit getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the speed limit enumeration.
     * 
     * @param enumeration The speed limit enumeration to set.
     */
    public void setEnumeration(SpeedLimit enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, SpeedLimitType.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (SpeedLimitType.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SpeedLimitType element (%d)", SpeedLimitType.NUM_BITS));
        }

        String speedLimitTypeBits = bits.substring(0, SpeedLimitType.NUM_BITS);

        if (speedLimitTypeBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The SpeedLimitType extension is not supported");
        }

        int speedLimitNum = UPERInteger.decode(speedLimitTypeBits.substring(1), 0);
        boolean found = false;
        for (SpeedLimit speedLimit : SpeedLimit.values()) {

            if (speedLimitNum == speedLimit.getNumber()) {

                enumeration = speedLimit;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known SpeedLimit enumeration");
        }
        return bits.substring(SpeedLimitType.NUM_BITS);
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
        if (!(object instanceof SpeedLimitType)) {

            return false;
        }
        SpeedLimitType element = (SpeedLimitType)object;
        return this.enumeration.equals(element.enumeration);
    }
}
