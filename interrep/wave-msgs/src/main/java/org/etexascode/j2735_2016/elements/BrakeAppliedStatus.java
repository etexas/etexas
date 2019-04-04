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
 * The brake applied status element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class BrakeAppliedStatus implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a brake applied status.
     */
    public static final int NUM_BITS = 5;

    /**
     * Is the brake applied on the left front tire.
     */
    private boolean leftFront = false;

    /**
     * Is the brake applied on the left rear tire.
     */
    private boolean leftRear = false;

    /**
     * Is the brake applied on the right front tire.
     */
    private boolean rightFront = false;

    /**
     * Is the brake applied on the right rear tire.
     */
    private boolean rightRear = false;

    /**
     * A getter for the left front tire brake applied status.
     * 
     * @return The left front tire brake applied status.
     */
    public boolean isLeftFront() {

        return leftFront;
    }

    /**
     * A setter for the left front tire brake applied status.
     * 
     * @param leftFront The left front to set.
     */
    public void setLeftFront(boolean leftFront) {

        this.leftFront = leftFront;
    }

    /**
     * A getter for the left rear tire brake applied status.
     * 
     * @return The left rear tire brake applied status.
     */
    public boolean isLeftRear() {

        return leftRear;
    }

    /**
     * A setter for the left rear tire brake applied status.
     * 
     * @param leftRear The left rear to set.
     */
    public void setLeftRear(boolean leftRear) {

        this.leftRear = leftRear;
    }

    /**
     * A getter for the right front tire brake applied status.
     * 
     * @return The right front tire brake applied status.
     */
    public boolean isRightFront() {

        return rightFront;
    }

    /**
     * A setter for the right front tire brake applied status.
     * 
     * @param rightFront The right front to set.
     */
    public void setRightFront(boolean rightFront) {

        this.rightFront = rightFront;
    }

    /**
     * A getter for the right rear tire brake applied status.
     * 
     * @return The right rear tire brake applied status.
     */
    public boolean isRightRear() {

        return rightRear;
    }

    /**
     * A setter for the right rear tire brake applied status.
     * 
     * @param rightRear The right rear to set.
     */
    public void setRightRear(boolean rightRear) {

        this.rightRear = rightRear;
    }

    @Override
    public String encodeUPER() {

        if (leftFront || leftRear || rightFront || rightRear) {

            StringBuilder appliedStatus = new StringBuilder(5);
            appliedStatus.append('0');
            appliedStatus.append(leftFront ? '1' : '0');
            appliedStatus.append(leftRear ? '1' : '0');
            appliedStatus.append(rightFront ? '1' : '0');
            appliedStatus.append(rightRear ? '1' : '0');

            return appliedStatus.toString();
        }

        return "10000";
    }

    @Override
    public String decodeUPER(String bits) {

        if (BrakeAppliedStatus.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a BrakeAppliedStatus element (%d)", BrakeAppliedStatus.NUM_BITS));
        }

        String brakeAppliedStatusBits = bits.substring(0, BrakeAppliedStatus.NUM_BITS);
        if (brakeAppliedStatusBits.charAt(0) == '1' && !"10000".equalsIgnoreCase(brakeAppliedStatusBits)) {

            throw new IllegalArgumentException("The unavailable bit is flipped to on, but one or more of the other bits were turned on.");
        }
        if ("00000".equalsIgnoreCase(brakeAppliedStatusBits)) {

            throw new IllegalArgumentException("The unavailable bit is flipped to off, but none of the other bits are turned on.");
        }

        leftFront = brakeAppliedStatusBits.charAt(1) == '1';
        leftRear = brakeAppliedStatusBits.charAt(2) == '1';
        rightFront = brakeAppliedStatusBits.charAt(3) == '1';
        rightRear = brakeAppliedStatusBits.charAt(4) == '1';

        return bits.substring(BrakeAppliedStatus.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(leftFront, leftRear, rightFront, rightRear);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof BrakeAppliedStatus)) {

            return false;
        }
        BrakeAppliedStatus element = (BrakeAppliedStatus)object;
        return this.leftFront == element.leftFront
                && this.leftRear == element.leftRear
                && this.rightFront == element.rightFront
                && this.rightRear == element.rightRear;
    }
}
