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
 * The scale b12 element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ScaleB12 implements UnalignedPackedEncodingRules {

    /**
     * The maximum value of a scale b12.
     */
    public static final int MAX = 2047;

    /**
     * The minimum value of a scale b12.
     */
    public static final int MIN = -2048;

    /**
     * The number of bits that cover a scale b12.
     */
    public static final int NUM_BITS = 12;

    /**
     * The scale b12 value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public ScaleB12() {

        value = ScaleB12.MIN;
    }

    /**
     * A constructor for the scale b12 element, where the value will be validated.
     * 
     * @param value The scale b12 value to set.
     */
    public ScaleB12(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the scale b12 value.
     * 
     * @return The scale b12 value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the scale b12 value, which will be validated before setting.
     * 
     * @param value The scale b12 value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the scale b12 value.
     * 
     * @param value The scale b12 value to be validated.
     * @return The scale b12 value that was successfully validated.
     */
    private int validate(int value) {

        if (value < ScaleB12.MIN || value > ScaleB12.MAX) {

            throw new IllegalArgumentException(String.format("The scale b12 value must be in the range of %d to %d", ScaleB12.MIN, ScaleB12.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, ScaleB12.MIN, ScaleB12.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (ScaleB12.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ScaleB12 element (%d)", ScaleB12.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, ScaleB12.NUM_BITS), ScaleB12.MIN));
        return bits.substring(ScaleB12.NUM_BITS);
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
        if (!(object instanceof ScaleB12)) {

            return false;
        }
        ScaleB12 element = (ScaleB12)object;
        return this.value == element.value;
    }
}
