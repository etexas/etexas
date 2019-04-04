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
package org.etexascode.j2735_2016.util;

/**
 * Helper class for Encoding and decoding UPER octet strings.
 * 
 * @author ttevendale
 */
public class UPEROctetString {

    /**
     * Hides the constructor since this class is not meant to be instantiated.
     */
    private UPEROctetString() {}

    /**
     * Encodes an octet string into a bit string.
     * 
     * @param value The octet string to encode.
     * @param bitSize The number of bits the bit string should be.
     * @return The bit string that represents the octet string.
     */
    public static String encode(String value, int bitSize) {

        long decimal = Long.parseLong(value, 16);
        String bitString = Long.toBinaryString(decimal);

        return UPERUtil.addEmptyBitsToFront(bitString, bitSize);
    }

    /**
     * Decodes a bit string into an octet string.
     * 
     * @param bits The bits that represent an octet string.
     * @param byteSize The number of bytes the octet string should be.
     * @return The octet string that the bits represented.
     */
    public static String decode(String bits, int byteSize) {

        long decimal = Long.parseLong(bits, 2);
        String byteString = Long.toHexString(decimal);

        return UPERUtil.addEmptyBytesToFront(byteString, byteSize);
    }
}
