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

import java.util.Arrays;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The restriction user type list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RestrictionUserTypeList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the restriction user type list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the restriction user type list.
     */
    public static final int MAX_LIST_SIZE = 16;

    /**
     * The minimum size of the restriction user type list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The restriction user type list.
     */
    private RestrictionUserType[] restrictions;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RestrictionUserTypeList() {

        restrictions = new RestrictionUserType[0];
    }

    /**
     * A constructor for the restriction user type list frame, where the size will be validated.
     * 
     * @param size The restriction user type list size to set.
     */
    public RestrictionUserTypeList(int size) {

        this.restrictions = new RestrictionUserType[validate(size)];
    }

    /**
     * A getter for the restriction array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The restriction array.
     */
    public RestrictionUserType[] getRestrictionArray() {

        return restrictions;
    }

    /**
     * Validates the restriction user type list size.
     * 
     * @param size The size to be validated.
     * @return The restriction user type list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < RestrictionUserTypeList.MIN_LIST_SIZE || size > RestrictionUserTypeList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the restriction user type list frame must be in the range of %d to %d", RestrictionUserTypeList.MIN_LIST_SIZE, RestrictionUserTypeList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (restrictions.length < RestrictionUserTypeList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough RestrictionUserType objects to encode the list. minimum: %d", RestrictionUserTypeList.MIN_LIST_SIZE));
        }

        StringBuilder restrictionUserTypeListBits = new StringBuilder(UPERInteger.encode(restrictions.length, RestrictionUserTypeList.MIN_LIST_SIZE, RestrictionUserTypeList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < restrictions.length; i++) {

            if (restrictions[i] == null) {

                throw new IllegalStateException(
                        String.format("The restriction user type list frame was not filled up to the amount specified. specified: %d, received: %d", restrictions.length, i + 1));
            }
            restrictionUserTypeListBits.append(restrictions[i].encodeUPER());
        }
        return restrictionUserTypeListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RestrictionUserTypeList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RestrictionUserTypeList frame (%d)", RestrictionUserTypeList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, RestrictionUserTypeList.NUM_BITS_LIST_SIZE), RestrictionUserTypeList.MIN_LIST_SIZE));
        bits = bits.substring(RestrictionUserTypeList.NUM_BITS_LIST_SIZE);

        restrictions = new RestrictionUserType[size];
        for (int i = 0; i < size; i++) {

            RestrictionUserType restriction = new RestrictionUserType();
            bits = restriction.decodeUPER(bits);
            restrictions[i] = restriction;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(restrictions);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RestrictionUserTypeList)) {

            return false;
        }
        RestrictionUserTypeList frame = (RestrictionUserTypeList)object;
        return Arrays.equals(this.restrictions, frame.restrictions);
    }
}
