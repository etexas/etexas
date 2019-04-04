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
 * The transmission state element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class TransmissionState implements UnalignedPackedEncodingRules {

    /**
     * The enum of TransmissionState. This is created inside of the class because TransmissionState
     * is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum Transmission {
        NEUTRAL(0),
        PARK(1),
        FORWARD_GEARS(2),
        REVERSE_GEARS(3),
        RESERVED1(4),
        RESERVED2(5),
        RESERVED3(6),
        UNAVAILABLE(7);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>Transmission</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private Transmission(int number) {

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
     * The number of bits that cover a transmission state.
     */
    public static final int NUM_BITS = 3;

    /**
     * The transmission enumeration.
     */
    private Transmission enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public TransmissionState() {

        enumeration = Transmission.NEUTRAL;
    }

    /**
     * A constructor for the transmission state element.
     * 
     * @param enumeration The transmission enumeration to set.
     */
    public TransmissionState(Transmission enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the transmission enumeration.
     * 
     * @return The transmission enumeration.
     */
    public Transmission getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the transmission enumeration.
     * 
     * @param enumeration The transmission enumeration to set.
     */
    public void setEnumeration(Transmission enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, TransmissionState.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (TransmissionState.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a TransmissionState element (%d)", TransmissionState.NUM_BITS));
        }

        int stateNum = UPERInteger.decode(bits.substring(0, TransmissionState.NUM_BITS), 0);
        for (Transmission state : Transmission.values()) {

            if (stateNum == state.getNumber()) {

                enumeration = state;
            }
        }
        return bits.substring(TransmissionState.NUM_BITS);
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
        if (!(object instanceof TransmissionState)) {

            return false;
        }
        TransmissionState element = (TransmissionState)object;
        return this.enumeration.equals(element.enumeration);
    }
}
