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
 * The brake boost applied element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class BrakeBoostApplied implements UnalignedPackedEncodingRules {

    /**
     * The enum of BrakeBoostApplied. This is created inside of the class because BrakeBoostApplied
     * is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum BrakeBoost {
        UNAVAILABLE(0), // Not Equpped with brake boost or data is unavailable
        OFF(1), // Vehicle's brake boost is off
        ON(2); // Vehicle's brake boost is on (applied)

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>BrakeBoost</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private BrakeBoost(int number) {

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
     * The number of bits that cover a brake boost applied.
     */
    public static final int NUM_BITS = 2;

    /**
     * The brake boost enumeration.
     */
    private BrakeBoost enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public BrakeBoostApplied() {

        enumeration = BrakeBoost.UNAVAILABLE;
    }

    /**
     * A constructor for the brake boost applied element.
     * 
     * @param enumeration The brake boost enumeration to set.
     */
    public BrakeBoostApplied(BrakeBoost enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the brake boost enumeration.
     * 
     * @return The brake boost enumeration.
     */
    public BrakeBoost getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the brake boost enumeration.
     * 
     * @param enumeration The brake boost enumeration to set.
     */
    public void setEnumeration(BrakeBoost enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, BrakeBoostApplied.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (BrakeBoostApplied.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a BrakeBoostApplied element(%d)", BrakeBoostApplied.NUM_BITS));
        }

        int appliedNum = UPERInteger.decode(bits.substring(0, BrakeBoostApplied.NUM_BITS), 0);
        boolean found = false;
        for (BrakeBoost boostApplied : BrakeBoost.values()) {

            if (appliedNum == boostApplied.getNumber()) {

                enumeration = boostApplied;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known BrakeBoost enumeration");
        }

        return bits.substring(BrakeBoostApplied.NUM_BITS);
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
        if (!(object instanceof BrakeBoostApplied)) {

            return false;
        }
        BrakeBoostApplied element = (BrakeBoostApplied)object;
        return this.enumeration.equals(element.enumeration);
    }
}
