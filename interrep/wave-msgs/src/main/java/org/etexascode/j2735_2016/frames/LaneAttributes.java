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

import org.etexascode.j2735_2016.elements.LaneDirection;
import org.etexascode.j2735_2016.elements.LaneSharing;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The lane attributes frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributes implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the lane attributes.
     */
    public static final int NUM_BITS = 1;

    /**
     * The lane direction element.
     */
    private LaneDirection directionalUse;

    /**
     * The lane sharing element.
     */
    private LaneSharing sharedWith;

    /**
     * The lane type attributes frame.
     */
    private LaneTypeAttributes laneType;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public LaneAttributes() {

        directionalUse = new LaneDirection();
        sharedWith = new LaneSharing();
        laneType = new LaneTypeAttributes();
    }

    /**
     * A constructor for the lane attributes frame for all required fields.
     * 
     * @param directionalUse The lane direction element.
     * @param sharedWith The lane sharing element.
     * @param laneType The lane type attributes frame.
     */
    public LaneAttributes(LaneDirection directionalUse, LaneSharing sharedWith, LaneTypeAttributes laneType) {

        this.directionalUse = Objects.requireNonNull(directionalUse);
        this.sharedWith = Objects.requireNonNull(sharedWith);
        this.laneType = Objects.requireNonNull(laneType);
    }

    /**
     * A getter for the lane direction element.
     * 
     * @return The lane direction element.
     */
    public LaneDirection getDirectionalUse() {

        return directionalUse;
    }

    /**
     * A setter for the lane direction element.
     * 
     * @param directionalUse The lane direction element to set.
     */
    public void setDirectionalUse(LaneDirection directionalUse) {

        this.directionalUse = Objects.requireNonNull(directionalUse);
    }

    /**
     * A getter for the lane sharing element.
     * 
     * @return The lane sharing element.
     */
    public LaneSharing getSharedWith() {

        return sharedWith;
    }

    /**
     * A setter for the lane sharing element.
     * 
     * @param sharedWith The lane sharing element to set.
     */
    public void setSharedWith(LaneSharing sharedWith) {

        this.sharedWith = Objects.requireNonNull(sharedWith);
    }

    /**
     * A getter for the lane type attributes element.
     * 
     * @return The lane type attributes element.
     */
    public LaneTypeAttributes getLaneType() {

        return laneType;
    }

    /**
     * A setter for the lane type attributes element.
     * 
     * @param laneType The lane type attributes element to set.
     */
    public void setLaneType(LaneTypeAttributes laneType) {

        this.laneType = Objects.requireNonNull(laneType);
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesBits = new StringBuilder();

        // we aren't planning on having the regional extension usable so setting to off.
        laneAttributesBits.append('0');
        laneAttributesBits.append(directionalUse.encodeUPER());
        laneAttributesBits.append(sharedWith.encodeUPER());
        laneAttributesBits.append(laneType.encodeUPER());

        return laneAttributesBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributes.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributes frame (%d)", LaneAttributes.NUM_BITS));
        }

        String laneAttributesOptionalBits = bits.substring(0, LaneAttributes.NUM_BITS);
        bits = bits.substring(LaneAttributes.NUM_BITS);

        if (laneAttributesOptionalBits.charAt(0) == '1') {

            throw new IllegalArgumentException("The LaneAttributes regional extension is not supported");
        }

        bits = directionalUse.decodeUPER(bits);
        bits = sharedWith.decodeUPER(bits);
        bits = laneType.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(directionalUse, sharedWith, laneType);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributes)) {

            return false;
        }
        LaneAttributes frame = (LaneAttributes)object;
        return this.directionalUse.equals(frame.directionalUse)
                && this.sharedWith.equals(frame.sharedWith)
                && this.laneType.equals(frame.laneType);
    }
}
