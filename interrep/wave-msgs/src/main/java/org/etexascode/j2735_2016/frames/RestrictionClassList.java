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
 * The restriction class list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RestrictionClassList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the restriction class list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 8;

    /**
     * The maximum size of the restriction class list.
     */
    public static final int MAX_LIST_SIZE = 254;

    /**
     * The minimum size of the restriction class list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The restriction class list.
     */
    private RestrictionClassAssignment[] restrictions;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RestrictionClassList() {

        restrictions = new RestrictionClassAssignment[0];
    }

    /**
     * A constructor for the restriction class list frame, where the size will be validated.
     * 
     * @param size The restriction class list size to set.
     */
    public RestrictionClassList(int size) {

        this.restrictions = new RestrictionClassAssignment[validate(size)];
    }

    /**
     * A getter for the restriction array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The restriction array.
     */
    public RestrictionClassAssignment[] getRestrictionArray() {

        return restrictions;
    }

    /**
     * Validates the restriction class list size.
     * 
     * @param size The size to be validated.
     * @return The restriction class list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < RestrictionClassList.MIN_LIST_SIZE || size > RestrictionClassList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the restriction class list frame must be in the range of %d to %d", RestrictionClassList.MIN_LIST_SIZE, RestrictionClassList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (restrictions.length < RestrictionClassList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough RestrictionClassAssignment objects to encode the list. minimum: %d", RestrictionClassList.MIN_LIST_SIZE));
        }

        StringBuilder restrictionClassListBits = new StringBuilder(UPERInteger.encode(restrictions.length, RestrictionClassList.MIN_LIST_SIZE, RestrictionClassList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < restrictions.length; i++) {

            if (restrictions[i] == null) {

                throw new IllegalStateException(String.format("The restriction class list frame was not filled up to the amount specified. specified: %d, received: %d", restrictions.length, i + 1));
            }
            restrictionClassListBits.append(restrictions[i].encodeUPER());
        }
        return restrictionClassListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RestrictionClassList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RestrictionClassList frame (%d)", RestrictionClassList.NUM_BITS_LIST_SIZE));
        }
        // 00000000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, RestrictionClassList.NUM_BITS_LIST_SIZE), RestrictionClassList.MIN_LIST_SIZE));
        bits = bits.substring(RestrictionClassList.NUM_BITS_LIST_SIZE);

        restrictions = new RestrictionClassAssignment[size];
        for (int i = 0; i < size; i++) {

            RestrictionClassAssignment restriction = new RestrictionClassAssignment();
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
        if (!(object instanceof RestrictionClassList)) {

            return false;
        }
        RestrictionClassList frame = (RestrictionClassList)object;
        return Arrays.equals(this.restrictions, frame.restrictions);
    }
}
