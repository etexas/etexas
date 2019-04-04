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
 * Helper class for Encoding and decoding UPER Longs.
 * 
 * @author ttevendale
 */
public class UPERLong {

    /**
     * Hides the constructor since this class is not meant to be instantiated.
     */
    private UPERLong() {}

    /**
     * Encodes an long into a bit string.
     * 
     * @param value The long to encode.
     * @param minimum The minimum long value that the object can be. Example: (value, minimum,
     *        biteSize): (0,0,2) = 00, (0,-1,2) = 01, (0,-2,2) = 10
     * @param bitSize The number of bits the bit string should be.
     * @return The bit string that represents the long.
     */
    public static String encode(long value, long minimum, int bitSize) {

        String bitString = Long.toBinaryString(value - minimum);

        return UPERUtil.addEmptyBitsToFront(bitString, bitSize);
    }

    /**
     * Decodes a bit string into an long.
     * 
     * @param bits The bits that represent an long.
     * @param minimum The minimum integer value that the bits represent. Example: (value, minimum):
     *        (0,0) = 0, (0,-1) = -1, (0,-2) = -2
     * @return The long that the bits represented.
     */
    public static long decode(String bits, int minimum) {

        return Long.parseLong(bits, 2) + minimum;
    }
}
