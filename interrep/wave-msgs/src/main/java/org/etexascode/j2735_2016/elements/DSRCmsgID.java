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
 * The DSRC message ID element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class DSRCmsgID implements UnalignedPackedEncodingRules {

    /**
     * The DSRC message ID for a MAP message.
     */
    public static final int MAP = 18;

    /**
     * The DSRC message ID for a SPAT message.
     */
    public static final int SPAT = 19;

    /**
     * The DSRC message ID for a BSM message.
     */
    public static final int BSM = 20;

    /**
     * The DSRC message ID for a CSR message.
     */
    public static final int CSR = 21;

    /**
     * The maximum value of a DSRC message ID.
     */
    public static final int MAX = 32767;

    /**
     * The minimum value of a DSRC message ID.
     */
    public static final int MIN = 0;

    /**
     * The number of bits that cover a DSRC message ID.
     */
    public static final int NUM_BITS = 15;

    /**
     * The DSRC message ID value.
     */
    private int value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public DSRCmsgID() {

        value = DSRCmsgID.MIN;
    }

    /**
     * A constructor for the DSRC message ID element, where the value will be validated.
     * 
     * @param value The DSRC message ID value to set.
     */
    public DSRCmsgID(int value) {

        this.value = validate(value);
    }

    /**
     * A getter for the DSRC message ID value.
     * 
     * @return The DSRC message ID value.
     */
    public int getValue() {

        return value;
    }

    /**
     * A setter for the DSRC message ID value, which will be validated before setting.
     * 
     * @param value The DSRC message ID value to set.
     */
    public void setValue(int value) {

        this.value = validate(value);
    }

    /**
     * Validates the DSRC message ID value.
     * 
     * @param value The value to be validated.
     * @return The DSRC message ID value that was successfully validated.
     */
    private int validate(int value) {

        if (value < DSRCmsgID.MIN || value > DSRCmsgID.MAX) {

            throw new IllegalArgumentException(String.format("The DSRC message ID value must be in the range of %d to %d", DSRCmsgID.MIN, DSRCmsgID.MAX));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPERInteger.encode(value, DSRCmsgID.MIN, DSRCmsgID.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (DSRCmsgID.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a DSRCmsgID element (%d)", DSRCmsgID.NUM_BITS));
        }
        value = validate(UPERInteger.decode(bits.substring(0, DSRCmsgID.NUM_BITS), DSRCmsgID.MIN));
        return bits.substring(DSRCmsgID.NUM_BITS);
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
        if (!(object instanceof DSRCmsgID)) {

            return false;
        }
        DSRCmsgID element = (DSRCmsgID)object;
        return this.value == element.value;
    }
}
