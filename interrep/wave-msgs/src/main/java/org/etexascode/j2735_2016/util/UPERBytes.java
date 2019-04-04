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
 * Helper class for Encoding and Decoding UPER Bytes.
 * 
 * @author ttevendale
 */
public class UPERBytes {

    /**
     * Hides the constructor since this class is not meant to be instantiated.
     */
    private UPERBytes() {}

    /**
     * Encodes a byte string into a bit string.
     * 
     * @param bytes The bytes to encode.
     * @return The bit string that represents the byte string.
     */
    public static String encode(String bytes) {

        StringBuilder binaryString = new StringBuilder(bytes.length() * 4);
        for (int i = 0; i < bytes.length(); i++) {

            String currentBits = Integer.toBinaryString(Integer.parseInt(bytes.substring(i, i + 1), 16));
            binaryString.append(UPERUtil.addEmptyBitsToFront(currentBits, 4));
        }
        return binaryString.toString();
    }

    /**
     * Decodes a bit string into a byte string. NOTE: The bit string must already be properly
     * padded.
     * 
     * @param bits The bits to decode
     * @return The bytes that the bits represented.
     */
    public static String decode(String bits) {

        if (bits.length() % 8 != 0) {

            throw new IllegalStateException("The bits must be properly padded in order to decode.");
        }
        int numHex = bits.length() / 4;
        StringBuilder hexString = new StringBuilder(numHex);

        for (int i = 0; i < numHex; i++) {

            int decimal = Integer.parseInt(bits.substring(0, 4), 2);
            bits = bits.substring(4);
            hexString.append(Integer.toHexString(decimal));
        }

        return hexString.toString();
    }
}
