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
import org.etexascode.j2735_2016.util.UPEROctetString;

/**
 * The temporary ID element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class TemporaryID implements UnalignedPackedEncodingRules {

    /**
     * The number of octets that cover a temporary ID.
     */
    public static final int NUM_OCTETS = 4;

    /**
     * The number of bits that cover a temporary ID.
     */
    public static final int NUM_BITS = 32;

    /**
     * The temporary ID value. represented as 4 octets, EX: "01010101"
     */
    private String value;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public TemporaryID() {

        value = "00000000";
    }

    /**
     * A constructor for the temporary ID element, where the value will be validated.
     * 
     * @param value The temporary ID value to set.
     */
    public TemporaryID(String value) {

        this.setValue(value);
    }

    /**
     * A constructor for the temporary ID element, where an integer value will be converted into the
     * octet string.
     * 
     * @param value The temporary ID value to convert and set.
     */
    public TemporaryID(int value) {

        this.setValue(value);
    }

    /**
     * A getter for the temporary ID value.
     * 
     * @return The temporary ID value.
     */
    public String getValue() {

        return value;
    }

    /**
     * A setter for the temporary ID value, which will be validated before setting.
     * 
     * @param value The temporary ID value to set.
     */
    public void setValue(String value) {

        this.value = validate(value);
    }

    /**
     * A setter for the temporary ID value, where an integer value will be converted into the octet
     * string.
     * 
     * @param value The temporary ID value to convert and set.
     */
    public void setValue(int value) {

        StringBuilder hexString = new StringBuilder(TemporaryID.NUM_OCTETS * 2);
        long num = (long)value - Integer.MIN_VALUE;
        hexString.append(Long.toHexString(num));

        while (hexString.length() < TemporaryID.NUM_OCTETS * 2) {

            hexString.insert(0, "0");
        }

        this.value = hexString.toString();
    }

    /**
     * Validates the temporary ID value.
     * 
     * @param value The value to be validated.
     * @return The temporary ID value that was successfully validated.
     */
    private String validate(String value) {

        Objects.requireNonNull(value);
        if (value.length() != TemporaryID.NUM_OCTETS * 2) {

            throw new IllegalArgumentException(String.format("The temporary ID value must be %d bytes, example: \"01010101\"", TemporaryID.NUM_OCTETS));
        }
        return value;
    }

    @Override
    public String encodeUPER() {

        return UPEROctetString.encode(value, TemporaryID.NUM_BITS);
    }

    @Override
    public String decodeUPER(String bits) {

        if (TemporaryID.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a TemporaryID element (%d)", TemporaryID.NUM_BITS));
        }
        value = validate(UPEROctetString.decode(bits.substring(0, TemporaryID.NUM_BITS), TemporaryID.NUM_OCTETS));
        return bits.substring(TemporaryID.NUM_BITS);
    }

    /**
     * As an extra note: the TemporaryID sting is case insensitive so the this method and the equals
     * method must toLowerCase the String
     */
    @Override
    public int hashCode() {

        return Objects.hashCode(value.toLowerCase());
    }

    /**
     * As an extra note: the TemporaryID sting is case insensitive so the this method and the
     * hashCode method must toLowerCase the String
     */
    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof TemporaryID)) {

            return false;
        }
        TemporaryID element = (TemporaryID)object;
        return this.value.equalsIgnoreCase(element.value);
    }
}
