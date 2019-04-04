/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.SegmentAttributeXY.SegmentAttribute;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the segment attribute xy element.
 * 
 * @author ttevendale
 */
public class SegmentAttributeXYTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.RESERVED);
        assertTrue(SegmentAttribute.RESERVED.equals(attribute.getEnumeration()));

        // max value
        attribute = new SegmentAttributeXY(SegmentAttribute.UN_EVEN_PAVEMENT_PRESENT);
        assertTrue(SegmentAttribute.UN_EVEN_PAVEMENT_PRESENT.equals(attribute.getEnumeration()));

        attribute = new SegmentAttributeXY(SegmentAttribute.DO_NOT_BLOCK);
        assertTrue(SegmentAttribute.DO_NOT_BLOCK.equals(attribute.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.RESERVED);
        String encodedStatus = attribute.encodeUPER();
        assertTrue("0000000".equals(encodedStatus));

        // test max
        attribute = new SegmentAttributeXY(SegmentAttribute.UN_EVEN_PAVEMENT_PRESENT);
        encodedStatus = attribute.encodeUPER();
        assertTrue("0100101".equals(encodedStatus));

        attribute = new SegmentAttributeXY(SegmentAttribute.WHITE_LINE);
        encodedStatus = attribute.encodeUPER();
        assertTrue("0000010".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        SegmentAttributeXY attribute = new SegmentAttributeXY();

        // test min
        String remainingBits = attribute.decodeUPER("0000000");
        assertTrue("".equals(remainingBits));
        assertTrue(SegmentAttribute.RESERVED.equals(attribute.getEnumeration()));

        // test max
        remainingBits = attribute.decodeUPER("0100101");
        assertTrue("".equals(remainingBits));
        assertTrue(SegmentAttribute.UN_EVEN_PAVEMENT_PRESENT.equals(attribute.getEnumeration()));

        remainingBits = attribute.decodeUPER("0000011");
        assertTrue("".equals(remainingBits));
        assertTrue(SegmentAttribute.MERGING_LANE_LEFT.equals(attribute.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("0100110");
    }

    @Test
    public void testDecodeUPERExtension() {

        SegmentAttributeXY attribute = new SegmentAttributeXY();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("1000000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        SegmentAttributeXY attribute = new SegmentAttributeXY();
        thrown.expect(IllegalArgumentException.class);
        attribute.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        SegmentAttributeXY attribute = new SegmentAttributeXY();
        String remainingBits = attribute.decodeUPER("00001000");
        assertTrue("0".equals(remainingBits));
        assertTrue(SegmentAttribute.MERGING_LANE_RIGHT.equals(attribute.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.CURB_ON_LEFT);
        SegmentAttributeXY attribute2 = new SegmentAttributeXY(SegmentAttribute.CURB_ON_RIGHT);

        assertFalse(attribute.hashCode() == attribute2.hashCode());
        assertTrue(attribute.hashCode() == attribute.hashCode());
        assertTrue(attribute2.hashCode() == attribute2.hashCode());

        SegmentAttributeXY attribute3 = new SegmentAttributeXY(attribute.getEnumeration());

        assertTrue(attribute.hashCode() == attribute3.hashCode());
        assertFalse(attribute2.hashCode() == attribute3.hashCode());
    }

    @Test
    public void testEquals() {

        SegmentAttributeXY attribute = new SegmentAttributeXY(SegmentAttribute.LOADING_ZONE_ON_LEFT);

        assertFalse(attribute.equals(null));

        assertTrue(attribute.equals(attribute));

        SegmentAttributeXY attribute2 = new SegmentAttributeXY(SegmentAttribute.LOADING_ZONE_ON_RIGHT);

        assertFalse(attribute.equals(new String()));
        assertFalse(attribute.equals(attribute2));

        attribute2.setEnumeration(attribute.getEnumeration());
        assertTrue(attribute.equals(attribute2));
    }
}
