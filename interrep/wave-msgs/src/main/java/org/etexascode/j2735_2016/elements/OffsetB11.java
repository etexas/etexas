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
 * The offset b11 element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class OffsetB11 implements UnalignedPackedEncodingRules {

    /**
     * The value representing that the offset b11 is unavailable.
     */
    public static final int UNAVAILABLE = -1024;

    /**
     * The maximum value of an offset b11.
     */
    public static final int MAX = 1023;

    /**
     * The minimum value of an offset b11.
     */
    public static final int MIN = -1024;

    /**
     * The number of bits that cover an offset b11.
     */
    public static final int NUM_BITS = 11;

    /**
     * The offset b11 value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public OffsetB11() {

        value = OffsetB11.MIN;
    }

    /**
     * A constructor for the offset b11 element, where the value will be validated.
     * 
     * @param value The offset b11 value to set.
     */
    public OffsetB11(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the offset b11 value.
     * 
     * @return The offset b11 value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the offset b11 value, which will be validated before setting.
     * 
     * @param value The offset b11 value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the offset b11 value.
     * 
     * @param value The offset b11 value to be validated.
     * @return The offset b11 value that was successfully validated.
     */
    private int validate(int value) {

        if (value < OffsetB11.MIN || value > OffsetB11.MAX) {

            throw new IllegalArgumentException(String.format("The offset b11 value must be in the range of %d to %d", OffsetB11.MIN, OffsetB11.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, OffsetB11.MIN, OffsetB11.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (OffsetB11.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an OffsetB11 element (%d)", OffsetB11.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, OffsetB11.NUM_BITS), OffsetB11.MIN));
        return bits.substring(OffsetB11.NUM_BITS);
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
        if (!(object instanceof OffsetB11)) {

            return false;
        }
        OffsetB11 element = (OffsetB11)object;
        return this.value == element.value;
    }
}
