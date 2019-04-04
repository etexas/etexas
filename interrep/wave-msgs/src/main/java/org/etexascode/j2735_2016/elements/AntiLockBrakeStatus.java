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
 * The anti lock brake status element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AntiLockBrakeStatus implements UnalignedPackedEncodingRules {

    /**
     * The enum of AntiLockBrakeStatus. This is created inside of the class because
     * AntiLockBrakeStatus is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum AntiLockBrake {
        UNAVAILABLE(0), // B'00, Not Equpped with ABS Brakes or status is unavailable
        OFF(1), // B'01, Vehicle's ABS is off
        ON(2), // B'10, Vehicle's ABS is on (but not Engaged)
        ENGAGED(3); // B'11, Vehicle's ABS is Engaged on any wheel

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>AntiLockBrake</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private AntiLockBrake(int number) {

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
     * The number of bits that cover an anti lock brake status.
     */
    public static final int NUM_BITS = 2;

    /**
     * The anti lock brake enumeration.
     */
    private AntiLockBrake enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public AntiLockBrakeStatus() {

        enumeration = AntiLockBrake.UNAVAILABLE;
    }

    /**
     * A constructor for the anti lock brake status element.
     * 
     * @param enumeration The anti lock brake enumeration to set.
     */
    public AntiLockBrakeStatus(AntiLockBrake enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the anti lock brake enumeration.
     * 
     * @return The anti lock brake enumeration.
     */
    public AntiLockBrake getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the anti lock brake enumeration.
     * 
     * @param enumeration The anti lock brake enumeration to set.
     */
    public void setEnumeration(AntiLockBrake enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, AntiLockBrakeStatus.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (AntiLockBrakeStatus.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AntiLockBrakeStatus element (%d)", AntiLockBrakeStatus.NUM_BITS));
        }

        int statusNum = UPERInteger.decode(bits.substring(0, AntiLockBrakeStatus.NUM_BITS), 0);
        for (AntiLockBrake status : AntiLockBrake.values()) {

            if (statusNum == status.getNumber()) {

                enumeration = status;
            }
        }
        return bits.substring(AntiLockBrakeStatus.NUM_BITS);
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
        if (!(object instanceof AntiLockBrakeStatus)) {

            return false;
        }
        AntiLockBrakeStatus element = (AntiLockBrakeStatus)object;
        return this.enumeration.equals(element.enumeration);
    }
}
