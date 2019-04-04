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

import org.etexascode.j2735_2016.elements.LayerType.Layer;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the layer type element.
 * 
 * @author ttevendale
 */
public class LayerTypeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        LayerType type = new LayerType(Layer.NONE);
        assertTrue(Layer.NONE.equals(type.getEnumeration()));

        // max value
        type = new LayerType(Layer.SHARED_LANE_DATA);
        assertTrue(Layer.SHARED_LANE_DATA.equals(type.getEnumeration()));

        type = new LayerType(Layer.MIXED_CONTENT);
        assertTrue(Layer.MIXED_CONTENT.equals(type.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        LayerType type = new LayerType(Layer.NONE);
        String encodedStatus = type.encodeUPER();
        assertTrue("0000".equals(encodedStatus));

        // test max
        type = new LayerType(Layer.SHARED_LANE_DATA);
        encodedStatus = type.encodeUPER();
        assertTrue("0111".equals(encodedStatus));

        type = new LayerType(Layer.GENERAL_MAP_DATA);
        encodedStatus = type.encodeUPER();
        assertTrue("0010".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        LayerType type = new LayerType();

        // test min
        String remainingBits = type.decodeUPER("0000");
        assertTrue("".equals(remainingBits));
        assertTrue(Layer.NONE.equals(type.getEnumeration()));

        // test max
        remainingBits = type.decodeUPER("0111");
        assertTrue("".equals(remainingBits));
        assertTrue(Layer.SHARED_LANE_DATA.equals(type.getEnumeration()));

        remainingBits = type.decodeUPER("0011");
        assertTrue("".equals(remainingBits));
        assertTrue(Layer.INTERSECTION_DATA.equals(type.getEnumeration()));
    }

    @Test
    public void testDecodeUPERExtension() {

        LayerType type = new LayerType();
        thrown.expect(IllegalArgumentException.class);
        type.decodeUPER("1000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        LayerType type = new LayerType();
        thrown.expect(IllegalArgumentException.class);
        type.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        LayerType type = new LayerType();
        String remainingBits = type.decodeUPER("0100101");
        assertTrue("101".equals(remainingBits));
        assertTrue(Layer.CURVE_DATA.equals(type.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        LayerType type = new LayerType(Layer.ROADWAY_SECTION_DATA);
        LayerType type2 = new LayerType(Layer.PARKING_AREA_DATA);

        assertFalse(type.hashCode() == type2.hashCode());
        assertTrue(type.hashCode() == type.hashCode());
        assertTrue(type2.hashCode() == type2.hashCode());

        LayerType type3 = new LayerType(type.getEnumeration());

        assertTrue(type.hashCode() == type3.hashCode());
        assertFalse(type2.hashCode() == type3.hashCode());
    }

    @Test
    public void testEquals() {

        LayerType type = new LayerType(Layer.ROADWAY_SECTION_DATA);

        assertFalse(type.equals(null));

        assertTrue(type.equals(type));

        LayerType type2 = new LayerType(Layer.PARKING_AREA_DATA);

        assertFalse(type.equals(new String()));
        assertFalse(type.equals(type2));

        type2.setEnumeration(type.getEnumeration());
        assertTrue(type.equals(type2));
    }
}
