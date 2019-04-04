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
import org.etexascode.j2735_2016.util.UPERLong;

/**
 * The longitude element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class Longitude implements UnalignedPackedEncodingRules {

    /**
     * The value representing that the longitude is unavailable.
     */
    public static final int UNAVAILABLE = 1800000001;

    /**
     * The maximum value of a longitude.
     */
    public static final int MAX = 1800000001;

    /**
     * The minimum value of a longitude.
     */
    public static final int MIN = -1799999999;

    /**
     * The number of bits that cover a longitude.
     */
    public static final int NUM_BITS = 32;

    /**
     * The longitude value.
     */
    private long value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public Longitude() {

        value = Longitude.MIN;
    }

    /**
     * A constructor for the longitude element, where the value will be validated.
     * 
     * @param value The longitude value to set.
     */
    public Longitude(long value) {

        this.value = validate(value);
    }

    /**
     * A getter for the longitude value.
     * 
     * @return The longitude value.
     */
    public long getValue() {

        return value;
    }

    /**
     * A setter for the longitude value, which will be validated before setting.
     * 
     * @param value The longitude value to set.
     */
    public void setValue(long value) {

        this.value = validate(value);
    }

    /**
     * Validates the longitude value.
     * 
     * @param value The value to be validated.
     * @return The longitude value that was successfully validated.
     */
    private long validate(long value) {

        if (value < Longitude.MIN || value > Longitude.MAX) {

            throw new IllegalArgumentException(String.format("The longitude value must be in the range of %d to %d", Longitude.MIN, Longitude.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERLong.encode(value, Longitude.MIN, Longitude.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (Longitude.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a Longitude element (%d)", Longitude.NUM_BITS));
        }
        value = validate(UPERLong.decode(bits.substring(0, Longitude.NUM_BITS), Longitude.MIN));
        return bits.substring(Longitude.NUM_BITS);
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
        if (!(object instanceof Longitude)) {

            return false;
        }
        Longitude element = (Longitude)object;
        return this.value == element.value;
    }
}
