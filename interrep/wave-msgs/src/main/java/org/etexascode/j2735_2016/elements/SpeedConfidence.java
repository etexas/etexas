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
 * The speed confidence element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SpeedConfidence implements UnalignedPackedEncodingRules {

    /**
     * The enum of SpeedConfidence. This is created inside of the class because SpeedConfidence is
     * an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum SpeedPrecision {
        UNAVAILABLE(0),
        PREC100MS(1),
        PREC10MS(2),
        PREC5MS(3),
        PREC1MS(4),
        PREC0_1MS(5),
        PREC0_05MS(6),
        PREC0_01MS(7);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>SpeedPrecision</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private SpeedPrecision(int number) {

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
     * The number of bits that cover a speed confidence.
     */
    public static final int NUM_BITS = 3;

    /**
     * The speed precision enumeration.
     */
    private SpeedPrecision enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SpeedConfidence() {

        enumeration = SpeedPrecision.UNAVAILABLE;
    }

    /**
     * A constructor for the speed confidence element.
     * 
     * @param enumeration The speed precision enumeration to set.
     */
    public SpeedConfidence(SpeedPrecision enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the speed precision enumeration.
     * 
     * @return The speed precision enumeration.
     */
    public SpeedPrecision getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the speed precision enumeration.
     * 
     * @param enumeration The speed precision enumeration to set.
     */
    public void setEnumeration(SpeedPrecision enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(enumeration.getNumber(), 0, SpeedConfidence.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (SpeedConfidence.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SpeedConfidence element (%d)", SpeedConfidence.NUM_BITS));
        }

        int precisionNum = UPERInteger.decode(bits.substring(0, SpeedConfidence.NUM_BITS), 0);
        for (SpeedPrecision speedPrecision : SpeedPrecision.values()) {

            if (precisionNum == speedPrecision.getNumber()) {

                enumeration = speedPrecision;
            }
        }
        return bits.substring(SpeedConfidence.NUM_BITS);
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
        if (!(object instanceof SpeedConfidence)) {

            return false;
        }
        SpeedConfidence element = (SpeedConfidence)object;
        return this.enumeration.equals(element.enumeration);
    }
}
