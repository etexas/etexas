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
 * The road regulator ID element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RoadRegulatorID implements UnalignedPackedEncodingRules {

    /**
     * The maximum value of a road regulator ID.
     */
    public static final int MAX = 65535;

    /**
     * The minimum value of a road regulator ID.
     */
    public static final int MIN = 0;

    /**
     * The number of bits that cover a road regulator ID.
     */
    public static final int NUM_BITS = 16;

    /**
     * The road regulator ID value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RoadRegulatorID() {

        value = RoadRegulatorID.MIN;
    }

    /**
     * A constructor for the road regulator ID element, where the value will be validated.
     * 
     * @param value The road regulator ID value to set.
     */
    public RoadRegulatorID(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the road regulator ID value.
     * 
     * @return The road regulator ID value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the road regulator ID value, which will be validated before setting.
     * 
     * @param value The road regulator ID value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the road regulator ID value.
     * 
     * @param value The value to be validated.
     * @return The road regulator ID value that was successfully validated.
     */
    private int validate(int value) {

        if (value < RoadRegulatorID.MIN || value > RoadRegulatorID.MAX) {

            throw new IllegalArgumentException(String.format("The road regulator ID value must be in the range of %d to %d", RoadRegulatorID.MIN, RoadRegulatorID.MAX));
        }

        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, RoadRegulatorID.MIN, RoadRegulatorID.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (RoadRegulatorID.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RoadRegulatorID element (%d)", RoadRegulatorID.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, RoadRegulatorID.NUM_BITS), RoadRegulatorID.MIN));
        return bits.substring(RoadRegulatorID.NUM_BITS);
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
        if (!(object instanceof RoadRegulatorID)) {

            return false;
        }
        RoadRegulatorID element = (RoadRegulatorID)object;
        return this.value == element.value;
    }
}
