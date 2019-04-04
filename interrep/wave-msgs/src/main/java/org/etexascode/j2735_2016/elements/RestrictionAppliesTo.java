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
 * The restriction applies to element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RestrictionAppliesTo implements UnalignedPackedEncodingRules {

    /**
     * The enum of RestrictionAppliesTo. This is created inside of the class because
     * RestrictionAppliesTo is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum Restriction {
        NONE(0),
        EQUIPPED_TRANSIT(1),
        EQUIPPED_TAXIS(2),
        EQUIPPED_OTHER(3),
        EMISSION_COMPLIANT(4),
        EQUIPPED_BICYCLE(5),
        WEIGHT_COMPLIANT(6),
        HEIGHT_COMPLIANT(7),
        PEDESTRIANS(8),
        SLOW_MOVING_PERSONS(9),
        WHEELCHAIR_USERS(10),
        VISUAL_DISABILITIES(11),
        AUDIO_DISABILITIES(12),
        OTHER_UNKNOWN_DISABILITIES(13);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>Restriction</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private Restriction(int number) {

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
     * The number of bits that cover a restriction applies to.
     */
    public static final int NUM_BITS = 5;

    /**
     * The restriction enumeration.
     */
    private Restriction enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RestrictionAppliesTo() {

        enumeration = Restriction.NONE;
    }

    /**
     * A constructor for the restriction applies to element.
     * 
     * @param enumeration The restriction enumeration to set.
     */
    public RestrictionAppliesTo(Restriction enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the restriction enumeration.
     * 
     * @return The restriction enumeration.
     */
    public Restriction getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the restriction enumeration.
     * 
     * @param enumeration The restriction enumeration to set.
     */
    public void setEnumeration(Restriction enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, RestrictionAppliesTo.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (RestrictionAppliesTo.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RestrictionAppliesTo element (%d)", RestrictionAppliesTo.NUM_BITS));
        }

        String restrictionAppliesToBits = bits.substring(0, RestrictionAppliesTo.NUM_BITS);

        if (restrictionAppliesToBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The RestrictionAppliesTo extension is not supported");
        }

        int restrictionNum = UPERInteger.decode(restrictionAppliesToBits.substring(1), 0);
        boolean found = false;
        for (Restriction restriction : Restriction.values()) {

            if (restrictionNum == restriction.getNumber()) {

                enumeration = restriction;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known Restriction enumeration");
        }
        return bits.substring(RestrictionAppliesTo.NUM_BITS);
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
        if (!(object instanceof RestrictionAppliesTo)) {

            return false;
        }
        RestrictionAppliesTo element = (RestrictionAppliesTo)object;
        return this.enumeration.equals(element.enumeration);
    }
}
