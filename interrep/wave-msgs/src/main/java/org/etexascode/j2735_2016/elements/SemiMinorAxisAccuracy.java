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
 * The semi minor axis accuracy element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class SemiMinorAxisAccuracy implements UnalignedPackedEncodingRules {

    /**
     * The value representing that the semi minor axis accuracy is unavailable.
     */
    public static final int UNAVAILABLE = 255;

    /**
     * The maximum value of a semi minor axis accuracy.
     */
    public static final int MAX = 255;

    /**
     * The minimum value of a semi minor axis accuracy.
     */
    public static final int MIN = 0;

    /**
     * The number of bits that cover a semi minor axis accuracy.
     */
    public static final int NUM_BITS = 8;

    /**
     * The semi minor axis accuracy value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public SemiMinorAxisAccuracy() {

        value = SemiMinorAxisAccuracy.MIN;
    }

    /**
     * A constructor for the semi minor axis accuracy element, where the value will be validated.
     * 
     * @param value The semi minor axis accuracy value to set.
     */
    public SemiMinorAxisAccuracy(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the semi minor axis accuracy value.
     * 
     * @return The semi minor axis accuracy value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the semi minor axis accuracy value, which will be validated before setting.
     * 
     * @param value The semi minor axis accuracy value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the semi minor axis accuracy value.
     * 
     * @param value The value to be validated.
     * @return The semi minor axis accuracy value that was successfully validated.
     */
    private int validate(int value) {

        if (value < SemiMinorAxisAccuracy.MIN || value > SemiMinorAxisAccuracy.MAX) {

            throw new IllegalArgumentException(String.format("The semi minor axis accuracy value must be in the range of %d to %d", SemiMinorAxisAccuracy.MIN, SemiMinorAxisAccuracy.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, SemiMinorAxisAccuracy.MIN, SemiMinorAxisAccuracy.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (SemiMinorAxisAccuracy.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a SemiMinorAxisAccuracy element (%d)", SemiMinorAxisAccuracy.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, SemiMinorAxisAccuracy.NUM_BITS), SemiMinorAxisAccuracy.MIN));
        return bits.substring(SemiMinorAxisAccuracy.NUM_BITS);
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
        if (!(object instanceof SemiMinorAxisAccuracy)) {

            return false;
        }
        SemiMinorAxisAccuracy element = (SemiMinorAxisAccuracy)object;
        return this.value == element.value;
    }
}
