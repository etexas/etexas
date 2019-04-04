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
 * The steering wheel angle element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SteeringWheelAngle implements UnalignedPackedEncodingRules {

    /**
     * The value representing that the steering wheel angle is unavailable.
     */
    public static final int UNAVAILABLE = 127;

    /**
     * The maximum value of a steering wheel angle.
     */
    public static final int MAX = 127;

    /**
     * The minimum value of a steering wheel angle.
     */
    public static final int MIN = -126;

    /**
     * The number of bits that cover a steering wheel angle.
     */
    public static final int NUM_BITS = 8;

    /**
     * The steering wheel angle value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SteeringWheelAngle() {

        value = SteeringWheelAngle.MIN;
    }

    /**
     * A constructor for the steering wheel angle element, where the value will be validated.
     * 
     * @param value The steering wheel angle value to set.
     */
    public SteeringWheelAngle(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the steering wheel angle value.
     * 
     * @return The steering wheel angle value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the steering wheel angle value, which will be validated before setting.
     * 
     * @param value The steering wheel angle value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the steering wheel angle value.
     * 
     * @param value The value to be validated.
     * @return The steering wheel angle value that was successfully validated.
     */
    private int validate(int value) {

        if (value < SteeringWheelAngle.MIN || value > SteeringWheelAngle.MAX) {

            throw new IllegalArgumentException(String.format("The steering wheel angle must be in the range of %d to %d", SteeringWheelAngle.MIN, SteeringWheelAngle.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, SteeringWheelAngle.MIN, SteeringWheelAngle.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (SteeringWheelAngle.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SteeringWheelAngle element (%d)", SteeringWheelAngle.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, SteeringWheelAngle.NUM_BITS), SteeringWheelAngle.MIN));
        return bits.substring(SteeringWheelAngle.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hashCode(value);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof SteeringWheelAngle)) {

            return false;
        }
        SteeringWheelAngle element = (SteeringWheelAngle)object;
        return this.value == element.value;
    }
}
