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
 * The auxiliary brake status element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AuxiliaryBrakeStatus implements UnalignedPackedEncodingRules {

    /**
     * The enum of AuxiliaryBrakeStatus. This is created inside of the class because
     * AuxiliaryBrakeStatus is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum AuxiliaryBrake {
        UNAVAILABLE(0), // B'00, Not Equpped with Aux Brakes or status is unavailable
        OFF(1), // B'01, Vehicle's Aux Brakes are off
        ON(2), // B'10, Vehicle's Aux Brakes are on ( Engaged )
        RESERVED(3); // B'11

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>AuxiliaryBrake</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private AuxiliaryBrake(int number) {

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
     * The number of bits that cover an auxiliary brake status.
     */
    public static final int NUM_BITS = 2;

    /**
     * The auxiliary brake enumeration.
     */
    private AuxiliaryBrake enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public AuxiliaryBrakeStatus() {

        enumeration = AuxiliaryBrake.UNAVAILABLE;
    }

    /**
     * A constructor for the auxiliary brake status element.
     * 
     * @param enumeration The auxiliary brake enumeration to set.
     */
    public AuxiliaryBrakeStatus(AuxiliaryBrake enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the auxiliary brake enumeration.
     * 
     * @return The auxiliary brake enumeration.
     */
    public AuxiliaryBrake getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the auxiliary brake enumeration.
     * 
     * @param enumeration The auxiliary brake enumeration to set.
     */
    public void setEnumeration(AuxiliaryBrake enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, AuxiliaryBrakeStatus.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (AuxiliaryBrakeStatus.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AuxiliaryBrakeStatus element (%d)", AuxiliaryBrakeStatus.NUM_BITS));
        }

        int statusName = UPERInteger.decode(bits.substring(0, AuxiliaryBrakeStatus.NUM_BITS), 0);
        for (AuxiliaryBrake status : AuxiliaryBrake.values()) {

            if (statusName == status.getNumber()) {

                enumeration = status;
            }
        }
        return bits.substring(AuxiliaryBrakeStatus.NUM_BITS);
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
        if (!(object instanceof AuxiliaryBrakeStatus)) {

            return false;
        }
        AuxiliaryBrakeStatus element = (AuxiliaryBrakeStatus)object;
        return this.enumeration.equals(element.enumeration);
    }
}
