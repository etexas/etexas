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
 * The pedestrian bicycle detect element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class PedestrianBicycleDetect implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a pedestrian bicycle detect.
     */
    public static final int NUM_BITS = 1;

    /**
     * The pedestrian bicycle detect value.
     */
    private boolean value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public PedestrianBicycleDetect() {

        value = false;
    }

    /**
     * A constructor for the pedestrian bicycle detect element.
     * 
     * @param value The pedestrian bicycle detect value to set.
     */
    public PedestrianBicycleDetect(boolean value) {

        this.value = value;
    }

    /**
     * A getter for the pedestrian bicycle detect value.
     * 
     * @return The pedestrian bicycle detect value.
     */
    public boolean getValue() {

        return value;
    }

    /**
     * A setter for the pedestrian bicycle detect value.
     * 
     * @param value The pedestrian bicycle detect value to set.
     */
    public void setValue(boolean value) {

        this.value = value;
    }

    @Override
    public String encodeUPER() {

        return value ? "1" : "0";
    }

    @Override
    public String decodeUPER(String bits) {

        if (PedestrianBicycleDetect.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a PedestrianBicycleDetect element (%d)", PedestrianBicycleDetect.NUM_BITS));
        }

        value = bits.charAt(0) == '1';

        return bits.substring(PedestrianBicycleDetect.NUM_BITS);
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
        if (!(object instanceof PedestrianBicycleDetect)) {

            return false;
        }
        PedestrianBicycleDetect element = (PedestrianBicycleDetect)object;
        return this.value == element.value;
    }
}
