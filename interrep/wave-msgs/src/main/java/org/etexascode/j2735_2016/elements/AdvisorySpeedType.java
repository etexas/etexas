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
 * The advisory speed type element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AdvisorySpeedType implements UnalignedPackedEncodingRules {

    /**
     * The enum of AdvisorySpeedType. This is created inside of the class because AdvisorySpeedType
     * is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum Advisory {
        NONE(0),
        GREENWAVE(1),
        ECODRIVE(2),
        TRANSIT(3);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>Advisory</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private Advisory(int number) {

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
     * The number of bits that cover an advisory speed type.
     */
    public static final int NUM_BITS = 3;

    /**
     * The advisory enumeration.
     */
    private Advisory enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public AdvisorySpeedType() {

        enumeration = Advisory.NONE;
    }

    /**
     * A constructor for the advisory speed type element.
     * 
     * @param enumeration The advisory enumeration to set.
     */
    public AdvisorySpeedType(Advisory enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the advisory enumeration.
     * 
     * @return The advisory enumeration.
     */
    public Advisory getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the advisory enumeration.
     * 
     * @param enumeration The advisory enumeration to set.
     */
    public void setEnumeration(Advisory enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, AdvisorySpeedType.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (AdvisorySpeedType.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an AdvisorySpeedType element (%d)", AdvisorySpeedType.NUM_BITS));
        }

        String advisorySpeedTypeBits = bits.substring(0, AdvisorySpeedType.NUM_BITS);

        if (advisorySpeedTypeBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The AdvisorySpeedType extension is not supported");
        }

        int advisoryNum = UPERInteger.decode(advisorySpeedTypeBits.substring(1), 0);
        for (Advisory advisory : Advisory.values()) {

            if (advisoryNum == advisory.getNumber()) {

                enumeration = advisory;
            }
        }
        return bits.substring(AdvisorySpeedType.NUM_BITS);
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
        if (!(object instanceof AdvisorySpeedType)) {

            return false;
        }
        AdvisorySpeedType element = (AdvisorySpeedType)object;
        return this.enumeration.equals(element.enumeration);
    }
}
