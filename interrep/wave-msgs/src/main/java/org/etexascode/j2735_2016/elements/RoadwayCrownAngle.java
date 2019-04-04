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
 * The roadway crown angle element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RoadwayCrownAngle implements UnalignedPackedEncodingRules {

    /**
     * The value representing that the roadway crown angle is unknown.
     */
    public static final int UNKNOWN = -128;

    /**
     * The maximum value of a roadway crown angle.
     */
    public static final int MAX = 127;

    /**
     * The minimum value of a roadway crown angle.
     */
    public static final int MIN = -128;

    /**
     * The number of bits that cover a roadway crown angle.
     */
    public static final int NUM_BITS = 8;

    /**
     * The roadway crown angle value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RoadwayCrownAngle() {

        value = RoadwayCrownAngle.MIN;
    }

    /**
     * A constructor for the roadway crown angle element, where the value will be validated.
     * 
     * @param value The roadway crown angle value to set.
     */
    public RoadwayCrownAngle(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the roadway crown angle value.
     * 
     * @return The roadway crown angle value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the roadway crown angle value, which will be validated before setting.
     * 
     * @param value The roadway crown angle value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the roadway crown angle value.
     * 
     * @param value The roadway crown angle value to be validated.
     * @return The roadway crown angle value that was successfully validated.
     */
    private int validate(int value) {

        if (value < RoadwayCrownAngle.MIN || value > RoadwayCrownAngle.MAX) {

            throw new IllegalArgumentException(String.format("The roadway crown angle value must be in the range of %d to %d", RoadwayCrownAngle.MIN, RoadwayCrownAngle.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, RoadwayCrownAngle.MIN, RoadwayCrownAngle.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (RoadwayCrownAngle.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RoadwayCrownAngle element (%d)", RoadwayCrownAngle.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, RoadwayCrownAngle.NUM_BITS), RoadwayCrownAngle.MIN));
        return bits.substring(RoadwayCrownAngle.NUM_BITS);
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
        if (!(object instanceof RoadwayCrownAngle)) {

            return false;
        }
        RoadwayCrownAngle element = (RoadwayCrownAngle)object;
        return this.value == element.value;
    }
}
