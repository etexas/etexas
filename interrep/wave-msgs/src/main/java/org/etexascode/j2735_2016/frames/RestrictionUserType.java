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

import java.util.Objects;

import org.etexascode.j2735_2016.elements.RestrictionAppliesTo;
import org.etexascode.j2735_2016.elements.RestrictionAppliesTo.Restriction;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The restriction user type frame (Choice) for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RestrictionUserType implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the restriction user type.
     */
    public static final int NUM_BITS = 2;

    /**
     * The restriction applies to element.
     */
    private RestrictionAppliesTo basicType;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RestrictionUserType() {

        basicType = null;
    }

    /**
     * A constructor for the restriction user type frame for the restriction applies to choice.
     * 
     * @param basicType The restriction applies to element.
     */
    public RestrictionUserType(RestrictionAppliesTo basicType) {

        this.basicType = Objects.requireNonNull(basicType);
    }

    /**
     * A constructor for the restriction user type frame for the restriction applies to choice.
     * (Primitive)
     * 
     * @param basicType The restriction applies to enumeration.
     */
    public RestrictionUserType(Restriction basicType) {

        this.basicType = new RestrictionAppliesTo(Objects.requireNonNull(basicType));
    }

    /**
     * A getter for the restriction applies to frame.
     * 
     * @return The restriction applies to frame.
     */
    public RestrictionAppliesTo getBasicType() {

        return basicType;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneTypeAttributes = new StringBuilder();
        laneTypeAttributes.append('0');

        if (basicType != null) {

            laneTypeAttributes.append(UPERInteger.encode(0, 0, RestrictionUserType.NUM_BITS - 1));
            laneTypeAttributes.append(basicType.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return laneTypeAttributes.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RestrictionUserType.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RestrictionUserType frame (%d)", RestrictionUserType.NUM_BITS));
        }

        String restrictionUserTypeChoiceBits = bits.substring(0, RestrictionUserType.NUM_BITS);

        if (restrictionUserTypeChoiceBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The RestrictionUserType extension is not supported");
        }

        bits = bits.substring(RestrictionUserType.NUM_BITS);

        int choice = UPERInteger.decode(restrictionUserTypeChoiceBits.substring(1), 0);

        switch (choice) {

            case 0:
                basicType = new RestrictionAppliesTo();
                bits = basicType.decodeUPER(bits);
                break;
            case 1:
                throw new IllegalArgumentException("The RestrictionUserType regional extension is not supported");
            default:
                throw new IllegalArgumentException(String.format("There should not be any more possible integers that would be returned, but received %d", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(basicType);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RestrictionUserType)) {

            return false;
        }
        RestrictionUserType frame = (RestrictionUserType)object;
        return Objects.equals(this.basicType, frame.basicType);
    }
}
