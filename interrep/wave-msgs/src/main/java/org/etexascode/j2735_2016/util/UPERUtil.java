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
 * Helper class for UPER.
 * 
 * @author ttevendale
 */
class UPERUtil {

    /**
     * Hides the constructor since this class is not meant to be instantiated.
     */
    private UPERUtil() {}

    /**
     * Adds leading zeros to a bit string based on the bit length needed.
     * 
     * @param bitString The bit string that may need leading zeros.
     * @param bitLength The length that the bit string needs to be.
     * @return The bit string with leading zeros if needed.
     */
    public static String addEmptyBitsToFront(String bitString, int bitLength) {

        return addZerosToFront(bitString, bitLength);
    }

    /**
     * Adds leading zeros to a byte string based on the byte length needed.
     * 
     * @param byteString The byte string that may need leading zeros.
     * @param byteLength The length that the byte string needs to be.
     * @return The byte string with leading zeros if needed.
     */
    public static String addEmptyBytesToFront(String byteString, int byteLength) {

        return addZerosToFront(byteString, byteLength * 2);
    }

    /**
     * Adds leading zeros to the string based on the string length needed.
     * 
     * @param string The string that may need leading zeros.
     * @param length The length that the string needs to be.
     * @return The string with leading zeros if needed.
     */
    private static String addZerosToFront(String string, int length) {

        int remainingBits = length - string.length();
        StringBuilder sb = new StringBuilder(length);

        for (int i = 0; i < remainingBits; i++) {

            sb.append("0");
        }

        sb.append(string);

        return sb.toString();
    }
}
