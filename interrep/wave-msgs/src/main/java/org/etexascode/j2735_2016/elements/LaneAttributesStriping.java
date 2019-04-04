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

/**
 * The lane attributes striping element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneAttributesStriping implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane attributes striping.
     */
    public static final int NUM_BITS = 16;

    /**
     * The stripe to connecting lanes revocable lane status.
     */
    private boolean stripeToConnectingLanesRevocableLane = false;

    /**
     * The stripe draw on left status.
     */
    private boolean stripeDrawOnLeft = false;

    /**
     * The stripe draw on right status.
     */
    private boolean stripeDrawOnRight = false;

    /**
     * The stripe to connecting lanes left status.
     */
    private boolean stripeToConnectingLanesLeft = false;

    /**
     * The stripe to connecting lanes right status.
     */
    private boolean stripeToConnectingLanesRight = false;

    /**
     * The stripe to connecting lanes ahead status.
     */
    private boolean stripeToConnectingLanesAhead = false;

    /**
     * A getter for the stripe to connecting lanes revocable lane status.
     * 
     * @return The stripe to connecting lanes revocable lane status.
     */
    public boolean isStripeToConnectingLanesRevocableLane() {

        return stripeToConnectingLanesRevocableLane;
    }

    /**
     * A setter for the stripe to connecting lanes revocable lane status.
     * 
     * @param stripeToConnectingLanesRevocableLane The stripe to connecting lanes revocable lane
     *        status to set.
     */
    public void setStripeToConnectingLanesRevocableLane(boolean stripeToConnectingLanesRevocableLane) {

        this.stripeToConnectingLanesRevocableLane = stripeToConnectingLanesRevocableLane;
    }

    /**
     * A getter for the stripe draw on left status.
     * 
     * @return The stripe draw on left status.
     */
    public boolean isStripeDrawOnLeft() {

        return stripeDrawOnLeft;
    }

    /**
     * A setter for the stripe draw on left status.
     * 
     * @param stripeDrawOnLeft The stripe draw on left status to set.
     */
    public void setStripeDrawOnLeft(boolean stripeDrawOnLeft) {

        this.stripeDrawOnLeft = stripeDrawOnLeft;
    }

    /**
     * A getter for the stripe draw on right status.
     * 
     * @return The stripe draw on right status.
     */
    public boolean isStripeDrawOnRight() {

        return stripeDrawOnRight;
    }

    /**
     * A setter for the stripe draw on right status.
     * 
     * @param stripeDrawOnRight The stripe draw on right status to set.
     */
    public void setStripeDrawOnRight(boolean stripeDrawOnRight) {

        this.stripeDrawOnRight = stripeDrawOnRight;
    }

    /**
     * A getter for the stripe to connecting lanes left status.
     * 
     * @return The stripe to connecting lanes left status.
     */
    public boolean isStripeToConnectingLanesLeft() {

        return stripeToConnectingLanesLeft;
    }

    /**
     * A setter for the stripe to connecting lanes left status.
     * 
     * @param stripeToConnectingLanesLeft The stripe to connecting lanes left status to set.
     */
    public void setStripeToConnectingLanesLeft(boolean stripeToConnectingLanesLeft) {

        this.stripeToConnectingLanesLeft = stripeToConnectingLanesLeft;
    }

    /**
     * A getter for the stripe to connecting lanes right status.
     * 
     * @return The stripe to connecting lanes right status.
     */
    public boolean isStripeToConnectingLanesRight() {

        return stripeToConnectingLanesRight;
    }

    /**
     * A setter for the stripe to connecting lanes right status.
     * 
     * @param stripeToConnectingLanesRight The stripe to connecting lanes right status to set.
     */
    public void setStripeToConnectingLanesRight(boolean stripeToConnectingLanesRight) {

        this.stripeToConnectingLanesRight = stripeToConnectingLanesRight;
    }

    /**
     * A getter for the stripe to connecting lanes ahead status.
     * 
     * @return The stripe to connecting lanes ahead status.
     */
    public boolean isStripeToConnectingLanesAhead() {

        return stripeToConnectingLanesAhead;
    }

    /**
     * A setter for the stripe to connecting lanes ahead status.
     * 
     * @param stripeToConnectingLanesAhead The stripe to connecting lanes ahead status to set.
     */
    public void setStripeToConnectingLanesAhead(boolean stripeToConnectingLanesAhead) {

        this.stripeToConnectingLanesAhead = stripeToConnectingLanesAhead;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneAttributesStriping = new StringBuilder(16);
        laneAttributesStriping.append(stripeToConnectingLanesRevocableLane ? '1' : '0');
        laneAttributesStriping.append(stripeDrawOnLeft ? '1' : '0');
        laneAttributesStriping.append(stripeDrawOnRight ? '1' : '0');
        laneAttributesStriping.append(stripeToConnectingLanesLeft ? '1' : '0');
        laneAttributesStriping.append(stripeToConnectingLanesRight ? '1' : '0');
        laneAttributesStriping.append(stripeToConnectingLanesAhead ? '1' : '0');
        laneAttributesStriping.append("0000000000");

        return laneAttributesStriping.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneAttributesStriping.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneAttributesStriping element (%d)", LaneAttributesStriping.NUM_BITS));
        }

        String laneAttributesStripingBits = bits.substring(0, LaneAttributesStriping.NUM_BITS);

        if (!"0000000000".equals(laneAttributesStripingBits.substring(6, 16))) {

            throw new IllegalArgumentException("The bits 6 - 15 must be set to zero, because the reserved bits are not currently in use.");
        }

        stripeToConnectingLanesRevocableLane = laneAttributesStripingBits.charAt(0) == '1';
        stripeDrawOnLeft = laneAttributesStripingBits.charAt(1) == '1';
        stripeDrawOnRight = laneAttributesStripingBits.charAt(2) == '1';
        stripeToConnectingLanesLeft = laneAttributesStripingBits.charAt(3) == '1';
        stripeToConnectingLanesRight = laneAttributesStripingBits.charAt(4) == '1';
        stripeToConnectingLanesAhead = laneAttributesStripingBits.charAt(5) == '1';

        return bits.substring(LaneAttributesStriping.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(stripeToConnectingLanesRevocableLane, stripeDrawOnLeft, stripeDrawOnRight, stripeToConnectingLanesLeft, stripeToConnectingLanesRight, stripeToConnectingLanesAhead);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneAttributesStriping)) {

            return false;
        }
        LaneAttributesStriping element = (LaneAttributesStriping)object;
        return this.stripeToConnectingLanesRevocableLane == element.stripeToConnectingLanesRevocableLane
                && this.stripeDrawOnLeft == element.stripeDrawOnLeft
                && this.stripeDrawOnRight == element.stripeDrawOnRight
                && this.stripeToConnectingLanesLeft == element.stripeToConnectingLanesLeft
                && this.stripeToConnectingLanesRight == element.stripeToConnectingLanesRight
                && this.stripeToConnectingLanesAhead == element.stripeToConnectingLanesAhead;
    }
}
