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

import org.etexascode.j2735_2016.elements.DrivenLineOffsetLg;
import org.etexascode.j2735_2016.elements.DrivenLineOffsetSm;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The computed lane offset x axis frame (Choice) for the j2735 2016 specification. NOTE: This frame
 * is not explicitly made in the specification, it's embedded in the computed lane frame.
 * 
 * @author ttevendale
 */
public class ComputedLaneOffsetXAxis implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the choice portion of the computed lane offset x axis.
     */
    public static final int NUM_BITS = 1;

    /**
     * The driven line offset small element.
     */
    private DrivenLineOffsetSm small;

    /**
     * The driven line offset large element.
     */
    private DrivenLineOffsetLg large;

    /**
     * A constructor setup only for decoding purposes.
     */
    public ComputedLaneOffsetXAxis() {

        small = null;
        large = null;
    }

    /**
     * A constructor for the computed lane offset x axis frame for the driven line offset small
     * choice.
     * 
     * @param small The driven line offset small element.
     */
    public ComputedLaneOffsetXAxis(DrivenLineOffsetSm small) {

        this.small = Objects.requireNonNull(small);
    }

    /**
     * A constructor for the computed lane offset x axis frame for the driven line offset large
     * choice.
     * 
     * @param large The driven line offset large element.
     */
    public ComputedLaneOffsetXAxis(DrivenLineOffsetLg large) {

        this.large = Objects.requireNonNull(large);
    }

    /**
     * A getter for the driven line offset small element.
     * 
     * @return The driven line offset small element.
     */
    public DrivenLineOffsetSm getSmall() {

        return small;
    }

    /**
     * A getter for the driven line offset large element.
     * 
     * @return The driven line offset large element.
     */
    public DrivenLineOffsetLg getLarge() {

        return large;
    }

    @Override
    public String encodeUPER() {

        StringBuilder computedLaneOffsetXAxisBits = new StringBuilder();

        if (small != null) {

            computedLaneOffsetXAxisBits.append(UPERInteger.encode(0, 0, ComputedLaneOffsetXAxis.NUM_BITS));
            computedLaneOffsetXAxisBits.append(small.encodeUPER());
        }
        else if (large != null) {

            computedLaneOffsetXAxisBits.append(UPERInteger.encode(1, 0, ComputedLaneOffsetXAxis.NUM_BITS));
            computedLaneOffsetXAxisBits.append(large.encodeUPER());
        }
        else {

            throw new IllegalStateException("None of the instance variables were initialized.");
        }

        return computedLaneOffsetXAxisBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ComputedLaneOffsetXAxis.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ComputedLaneOffsetXAxis frame (%d)", ComputedLaneOffsetXAxis.NUM_BITS));
        }

        String computedLaneOffsetXAxisChoiceBits = bits.substring(0, ComputedLaneOffsetXAxis.NUM_BITS);
        bits = bits.substring(ComputedLaneOffsetXAxis.NUM_BITS);

        int choice = UPERInteger.decode(computedLaneOffsetXAxisChoiceBits, 0);

        switch (choice) {

            case 0:
                small = new DrivenLineOffsetSm();
                bits = small.decodeUPER(bits);
                break;
            case 1:
                large = new DrivenLineOffsetLg();
                bits = large.decodeUPER(bits);
                break;
            default:
                throw new IllegalArgumentException(String.format("There should not be any more possible integers that would be returned, but received %d.", choice));
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(small, large);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ComputedLaneOffsetXAxis)) {

            return false;
        }
        ComputedLaneOffsetXAxis frame = (ComputedLaneOffsetXAxis)object;
        return Objects.equals(this.small, frame.small)
                && Objects.equals(this.large, frame.large);
    }
}
