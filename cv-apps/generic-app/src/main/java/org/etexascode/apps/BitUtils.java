/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.apps;

import java.nio.ByteBuffer;
import java.util.BitSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to read and write from BitSet to byte[] to ByteBuffer, and to concatenate and split
 * BitSets. Also has special use methods to write the vehicle length and width from ints to 10 and
 * 10 bits and store in a byte[3] array wrapped by a ByteBuffer, and read the bits from the
 * ByteBuffer back into ints.
 * 
 * @author bmauldon
 */
public class BitUtils {

    /**
     * static Logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(BitUtils.class);

    /**
     * concatenate two BitSets, with the lengths given. Neither BitSet.length() nor BitSet.size()
     * will reliably give the length you need.
     * 
     * @param length1 The first bits length.
     * @param length2 The second bits length.
     * @param bits1 The first bits.
     * @param bits2 The second bits.
     * @return The bit set.
     */
    public static BitSet concatenateBitSets(int length1, int length2, BitSet bits1, BitSet bits2) {

        int length = length1 + length2;
        int j;
        BitSet totalbits = new BitSet(length);
        for (int i = 0; i < length; i++) {
            if (i < length1) {
                totalbits.set(i, bits1.get(i));
            }
            else {
                j = i - length1;
                totalbits.set(i, bits2.get(j));
            }

        }
        LOGGER.debug(totalbits.toString());
        return totalbits;
    }

    /**
     * Returns a bitSet containing the values in bytes.
     * 
     * @param bytes The bytes array.
     * @return BitSet
     */
    public static BitSet fromByteArray(byte[] bytes) {
        BitSet bits = new BitSet();
        for (int i = 0; i < bytes.length * 8; i++) {
            if ((bytes[bytes.length - i / 8 - 1] & 1 << i % 8) > 0) {
                bits.set(i);
            }
        }
        return bits;
    }

    /**
     * Get vehicle length as int from sub-BitSet. startbit is inclusive, endbit is exclusive
     * 
     * @param bits The bits.
     * @param startbit The start bit.
     * @param endbit The end bit.
     * @return int
     */
    public static int getLength(BitSet bits, int startbit, int endbit) {
        BitSet lengthBits = bits.get(startbit, endbit);
        byte[] length = toByteArray(lengthBits, 2);
        ByteBuffer bytebuff = ByteBuffer.wrap(length);
        return bytebuff.getShort();

    }

    /**
     * Get ByteBuffer wrapping a byte[3] array from two ints, width and length.
     * 
     * @param width The width.
     * @param length The length.
     * @param numwidthbits The number of width bits.
     * @param numlengthbits The number of length bits.
     * @return The byte buffer.
     */
    public static ByteBuffer getVehicleSizeBytes(int width, int length, int numwidthbits, int numlengthbits) {
        ByteBuffer widthbuffer = ByteBuffer.allocate(4);
        ByteBuffer lengthbuffer = ByteBuffer.allocate(4);
        widthbuffer.putInt(width);
        lengthbuffer.putInt(length);
        byte[] widthbytes = widthbuffer.array();
        byte[] lengthbytes = lengthbuffer.array();
        BitSet widthbits = fromByteArray(widthbytes);
        BitSet lengthbits = fromByteArray(lengthbytes);
        BitSet totalbits = concatenateBitSets(numwidthbits, numlengthbits, widthbits, lengthbits);
        byte[] totalbytes = toByteArray(totalbits, 3);
        ByteBuffer vehicleSize = ByteBuffer.wrap(totalbytes);
        return vehicleSize;

    }

    /**
     * get short vehicle width from 10 bits in blob1
     * 
     * @param bits The bits.
     * @param startbit The start bit.
     * @param endbit The end bit.
     * @return short
     */
    public static short getWidth(BitSet bits, int startbit, int endbit) {
        BitSet widthBits = bits.get(startbit, endbit);
        byte[] width = toByteArray(widthBits, 2);
        ByteBuffer bytebuff = ByteBuffer.wrap(width);
        return bytebuff.getShort();

    }

    /**
     * Write a a BitSet to a byte[size] array
     * 
     * @param bits The bits.
     * @param size The size of the bits.
     * @return The byte array.
     */
    public static byte[] toByteArray(BitSet bits, int size) {
        byte[] bytes = new byte[size];
        for (int i = 0; i < bits.length(); i++) {
            if (bits.get(i)) {
                bytes[bytes.length - i / 8 - 1] |= 1 << i % 8;
            }
        }

        return bytes;
    }

}
