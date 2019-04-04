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
 * Helper class for Encoding and decoding UPER Integers.
 * 
 * @author ttevendale
 */
public class UPERInteger {

    /**
     * Hides the constructor since this class is not meant to be instantiated.
     */
    private UPERInteger() {}

    /**
     * Encodes an integer into a bit string.
     * 
     * @param value The integer to encode.
     * @param minimum The minimum integer value that the object can be. Example: (value, minimum,
     *        biteSize): (0,0,2) = 00, (0,-1,2) = 01, (0,-2,2) = 10
     * @param bitSize The number of bits the bit string should be.
     * @return The bit string that represents the integer.
     */
    public static String encode(int value, int minimum, int bitSize) {

        String bitString = Integer.toBinaryString(value - minimum);

        return UPERUtil.addEmptyBitsToFront(bitString, bitSize);
    }

    /**
     * Decodes a bit string into an integer.
     * 
     * @param bits The bits that represent an integer.
     * @param minimum The minimum integer value that the bits represent. Example: (value, minimum):
     *        (0,0) = 0, (0,-1) = -1, (0,-2) = -2
     * @return The integer that the bits represented.
     */
    public static int decode(String bits, int minimum) {

        return Integer.parseInt(bits, 2) + minimum;
    }
}
