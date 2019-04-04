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
 * The traction control status element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class TractionControlStatus implements UnalignedPackedEncodingRules {

    /**
     * The enum of TractionControlStatus. This is created inside of the class because
     * TractionControlStatus is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum TractionControl {
        UNAVAILABLE(0), // B'00, Not Equpped with traction control or status is unavailable
        OFF(1), // B'01, traction control is off
        ON(2), // B'10, traction control is on (but not Engaged)
        ENGAGED(3); // B'11, traction control is Engaged

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>TractionControl</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        TractionControl(int number) {

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
     * The number of bits that cover a traction control status.
     */
    public static final int NUM_BITS = 2;

    /**
     * The traction control enumeration.
     */
    private TractionControl enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public TractionControlStatus() {

        enumeration = TractionControl.UNAVAILABLE;
    }

    /**
     * A constructor for the traction control status element.
     * 
     * @param enumeration The traction control enumeration to set.
     */
    public TractionControlStatus(TractionControl enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the traction control enumeration.
     * 
     * @return The traction control enumeration.
     */
    public TractionControl getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the traction control enumeration.
     * 
     * @param enumeration The traction control enumeration to set.
     */
    public void setEnumeration(TractionControl enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, TractionControlStatus.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (TractionControlStatus.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a TractionControlStatus element (%d)", TractionControlStatus.NUM_BITS));
        }

        int statusNum = UPERInteger.decode(bits.substring(0, TractionControlStatus.NUM_BITS), 0);
        for (TractionControl status : TractionControl.values()) {

            if (statusNum == status.getNumber()) {

                enumeration = status;
            }
        }
        return bits.substring(TractionControlStatus.NUM_BITS);
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
        if (!(object instanceof TractionControlStatus)) {

            return false;
        }
        TractionControlStatus element = (TractionControlStatus)object;
        return this.enumeration.equals(element.enumeration);
    }
}
