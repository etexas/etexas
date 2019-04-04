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
 * The segment attribute XY element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SegmentAttributeXY implements UnalignedPackedEncodingRules {

    /**
     * The enum of SegmentAttributeXY. This is created inside of the class because
     * SegmentAttributeXY is an enum but I needed class controls to properly handle it.
     * 
     * @author ttevendale
     */
    public enum SegmentAttribute {
        RESERVED(0),
        DO_NOT_BLOCK(1),
        WHITE_LINE(2),
        MERGING_LANE_LEFT(3),
        MERGING_LANE_RIGHT(4),
        CURB_ON_LEFT(5),
        CURB_ON_RIGHT(6),
        LOADING_ZONE_ON_LEFT(7),
        LOADING_ZONE_ON_RIGHT(8),
        TURN_OUT_POINT_ON_LEFT(9),
        TURN_OUT_POINT_ON_RIGHT(10),
        ADJACENT_PARKING_ON_LEFT(11),
        ADJACENT_PARKING_ON_RIGHT(12),
        ADJACENT_BIKE_LANE_ON_LEFT(13),
        ADJACENT_BIKE_LANE_ON_RIGHT(14),
        SHARED_BIKE_LANE(15),
        BIKE_BOX_IN_FRONT(16),
        TRANSIT_STOP_ON_LEFT(17),
        TRANSIT_STOP_ON_RIGHT(18),
        TRANSIT_STOP_IN_LANE(19),
        SHARED_WITH_TRACKED_VEHICLE(20),
        SAFE_ISLAND(21),
        LOW_CURBS_PRESENT(22),
        RUMBLE_STRIP_PRESENT(23),
        AUDIBLE_SIGNALING_PRESENT(24),
        ADAPTIVE_TIMING_PRESENT(25),
        RF_SIGNAL_REQUEST_PRESENT(26),
        PARTIAL_CURB_INTRUSION(27),
        TAPER_TO_LEFT(28),
        TAPER_TO_RIGHT(29),
        TAPER_TO_CENTER_LINE(30),
        PARALLEL_PARKING(31),
        HEAD_IN_PARKING(32),
        FREE_PARKING(33),
        TIME_RESTRICTIONS_ON_PARKING(34),
        COST_TO_PARK(35),
        MID_BLOCK_CURB_PRESENT(36),
        UN_EVEN_PAVEMENT_PRESENT(37);

        /**
         * The integer representation of the enumeration.
         */
        private int number;

        /**
         * Creates a new <code>SegmentAttribute</code> with the specified number.
         * 
         * @param number The integer number to set.
         */
        private SegmentAttribute(int number) {

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
     * The number of bits that cover a segment attribute XY.
     */
    public static final int NUM_BITS = 7;

    /**
     * The segment attribute enumeration.
     */
    private SegmentAttribute enumeration;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SegmentAttributeXY() {

        enumeration = SegmentAttribute.RESERVED;
    }

    /**
     * A constructor for the segment attribute XY element.
     * 
     * @param enumeration The segment attribute enumeration to set.
     */
    public SegmentAttributeXY(SegmentAttribute enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    /**
     * A getter for the segment attribute enumeration.
     * 
     * @return The segment attribute enumeration.
     */
    public SegmentAttribute getEnumeration() {

        return enumeration;
    }

    /**
     * A setter for the segment attribute enumeration.
     * 
     * @param enumeration The segment attribute enumeration to set.
     */
    public void setEnumeration(SegmentAttribute enumeration) {

        this.enumeration = Objects.requireNonNull(enumeration);
    }

    @Override
    public String encodeUPER() {

        String extensionBit = "0";
        return extensionBit + UPERInteger.encode(enumeration.getNumber(), 0, SegmentAttributeXY.NUM_BITS - 1);
    }

    @Override
    public String decodeUPER(String bits) {

        if (SegmentAttributeXY.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SegmentAttributeXY element (%d)", SegmentAttributeXY.NUM_BITS));
        }

        String segmentAttributeXYBits = bits.substring(0, SegmentAttributeXY.NUM_BITS);

        if (segmentAttributeXYBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The SegmentAttributeXY extension is not supported");
        }

        int attributeNum = UPERInteger.decode(segmentAttributeXYBits.substring(1), 0);
        boolean found = false;
        for (SegmentAttribute attribute : SegmentAttribute.values()) {

            if (attributeNum == attribute.getNumber()) {

                enumeration = attribute;
                found = true;
            }
        }
        if (!found) {

            throw new IllegalArgumentException("The bits supplied did not match any known SegmentAttribute enumeration");
        }
        return bits.substring(SegmentAttributeXY.NUM_BITS);
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
        if (!(object instanceof SegmentAttributeXY)) {

            return false;
        }
        SegmentAttributeXY element = (SegmentAttributeXY)object;
        return this.enumeration.equals(element.enumeration);
    }
}
