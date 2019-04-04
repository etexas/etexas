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

import static org.junit.Assert.assertEquals;

import java.nio.ByteBuffer;
import java.util.BitSet;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BitUtilsTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(BitUtilsTest.class);

    int length = 634;

    int width = 40;

    String slength = Integer.toBinaryString(length);

    String swidth = Integer.toBinaryString(width);

    String five = Integer.toBinaryString(5);

    BitSet bits1 = new BitSet(10);

    BitSet bits2 = new BitSet(14);

    BitSet total = new BitSet();

    ByteBuffer buffer;

    byte[] bytes;

    byte[] sizebytes;

    @Before
    public void setUp() throws Exception {
        LOGGER.debug("Length: " + slength + ", Width: " + swidth + ", five: " + five);
        bits1.set(3);
        bits1.set(5);
        bits2.set(1);
        bits2.set(3);
        bits2.set(4);
        bits2.set(5);
        bits2.set(6);
        bits2.set(9);
        total = BitUtils.concatenateBitSets(10, 14, bits1, bits2);
        bytes = BitUtils.toByteArray(bits1, 2);
        buffer = BitUtils.getVehicleSizeBytes(width, length, 10, 14);
        sizebytes = buffer.array();
        LOGGER.debug("Total Bitset :" + total.toString());
        LOGGER.debug("Byte array length: " + new Integer(bytes.length).toString());

    }

    @After
    public void tearDown() throws Exception {}

    @Test
    public void testConcatenateBitSets() {
        assertEquals(bits1, total.get(0, 10));
        assertEquals(bits2, total.get(10, 24));

    }

    @Test
    public void testFromByteArray() {
        BitSet testbits = BitUtils.fromByteArray(bytes);
        assertEquals(bits1, testbits);

    }

    @Test
    public void testGetLength() {
        int l = BitUtils.getLength(total, 10, 24);
        assertEquals(length, l);
    }

    @Test
    public void testGetVehicleSizeBytes() {
        BitSet testbits = BitUtils.fromByteArray(sizebytes);
        assertEquals(total, testbits);

    }

    @Test
    public void testGetWidth() {
        short w = BitUtils.getWidth(total, 0, 10);
        LOGGER.debug(Integer.toBinaryString(w));
        assertEquals(width, w);
    }

    @Test
    public void testToByteArray() {
        BitSet testbits = BitUtils.fromByteArray(bytes);
        assertEquals(bits1, testbits);
    }

}
