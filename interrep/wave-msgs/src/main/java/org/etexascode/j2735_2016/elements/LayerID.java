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
 * The layer ID element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LayerID implements UnalignedPackedEncodingRules {

    /**
     * The maximum value of a layer ID.
     */
    public static final int MAX = 100;

    /**
     * The minimum value of a layer ID.
     */
    public static final int MIN = 0;

    /**
     * The number of bits that cover a layer ID.
     */
    public static final int NUM_BITS = 7;

    /**
     * The layer ID value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public LayerID() {

        value = LayerID.MIN;
    }

    /**
     * A constructor for the layer ID element, where the value will be validated.
     * 
     * @param value The layer ID value to set.
     */
    public LayerID(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the layer ID value.
     * 
     * @return The layer ID value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the layer ID value, which will be validated before setting.
     * 
     * @param value The layer ID value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the layer ID value.
     * 
     * @param value The layer ID value to be validated.
     * @return The layer ID value that was successfully validated.
     */
    private int validate(int value) {

        if (value < LayerID.MIN || value > LayerID.MAX) {

            throw new IllegalArgumentException(String.format("The layer ID value must be in the range of %d to %d", LayerID.MIN, LayerID.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, LayerID.MIN, LayerID.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (LayerID.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LayerID element (%d)", LayerID.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, LayerID.NUM_BITS), LayerID.MIN));
        return bits.substring(LayerID.NUM_BITS);
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
        if (!(object instanceof LayerID)) {

            return false;
        }
        LayerID element = (LayerID)object;
        return this.value == element.value;
    }
}
