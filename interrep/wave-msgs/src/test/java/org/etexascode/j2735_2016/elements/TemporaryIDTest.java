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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the temporary ID element.
 * 
 * @author ttevendale
 */
public class TemporaryIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        String minValue = "00000000";
        TemporaryID temporaryId = new TemporaryID(minValue);
        assertTrue(minValue.equalsIgnoreCase(temporaryId.getValue()));

        String maxValue = "ffffffff";
        temporaryId = new TemporaryID(maxValue);
        assertTrue(maxValue.equalsIgnoreCase(temporaryId.getValue()));

        String randomValue = "0B20a548";
        temporaryId = new TemporaryID(randomValue);
        assertTrue(randomValue.equalsIgnoreCase(temporaryId.getValue()));
    }

    @Test
    public void testConstructorLessBytes() {

        thrown.expect(IllegalArgumentException.class);
        new TemporaryID("000000");
    }

    @Test
    public void testConstructorMoreBytes() {

        thrown.expect(IllegalArgumentException.class);
        new TemporaryID("1100000011");
    }

    @Test
    public void testConstructorPrimitive() {

        TemporaryID temporaryId = new TemporaryID(Integer.MAX_VALUE);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("ffffffff"));

        temporaryId = new TemporaryID(Integer.MIN_VALUE);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("00000000"));

        temporaryId = new TemporaryID(0);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("80000000"));

        temporaryId = new TemporaryID(5451);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("8000154b"));

        temporaryId = new TemporaryID(-12340);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("7fffcfcc"));
    }

    @Test
    public void testSetValue() {

        TemporaryID temporaryId = new TemporaryID();

        String minValue = "00000000";
        temporaryId.setValue(minValue);
        assertTrue(minValue.equalsIgnoreCase(temporaryId.getValue()));

        String maxValue = "ffffffff";
        temporaryId.setValue(maxValue);
        assertTrue(maxValue.equalsIgnoreCase(temporaryId.getValue()));

        String randomValue = "0B2ba248";
        temporaryId.setValue(randomValue);
        assertTrue(randomValue.equalsIgnoreCase(temporaryId.getValue()));
    }

    @Test
    public void testSetValueLessBytes() {

        TemporaryID temporaryId = new TemporaryID();
        thrown.expect(IllegalArgumentException.class);
        temporaryId.setValue("000000");
    }

    @Test
    public void testSetValueMoreBytes() {

        TemporaryID temporaryId = new TemporaryID();
        thrown.expect(IllegalArgumentException.class);
        temporaryId.setValue("1100000011");
    }

    @Test
    public void testSetValuePrimitive() {

        TemporaryID temporaryId = new TemporaryID();
        temporaryId.setValue(Integer.MAX_VALUE);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("ffffffff"));

        temporaryId.setValue(Integer.MIN_VALUE);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("00000000"));

        temporaryId.setValue(0);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("80000000"));

        temporaryId.setValue(549815);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("800863b7"));

        temporaryId.setValue(-7889412);
        assertTrue(temporaryId.getValue().equalsIgnoreCase("7f879dfc"));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        TemporaryID temporaryId = new TemporaryID("00000000");
        String encodedTemporaryId = temporaryId.encodeUPER();
        assertTrue("00000000000000000000000000000000".equals(encodedTemporaryId));

        // test max
        temporaryId = new TemporaryID("ffffffff");
        encodedTemporaryId = temporaryId.encodeUPER();
        assertTrue("11111111111111111111111111111111".equals(encodedTemporaryId));

        String randomValue = "2abc329f";
        temporaryId = new TemporaryID(randomValue);
        encodedTemporaryId = temporaryId.encodeUPER();
        assertTrue("00101010101111000011001010011111".equals(encodedTemporaryId));
    }

    @Test
    public void testDecodeUPER() {

        TemporaryID temporaryId = new TemporaryID();

        // test min
        String remainingBits = temporaryId.decodeUPER("00000000000000000000000000000000");
        assertTrue("".equals(remainingBits));
        assertTrue("00000000".equalsIgnoreCase(temporaryId.getValue()));

        // test max
        remainingBits = temporaryId.decodeUPER("11111111111111111111111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue("ffffffff".equalsIgnoreCase(temporaryId.getValue()));

        String expectedValue = "2abc329f";
        remainingBits = temporaryId.decodeUPER("00101010101111000011001010011111");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedValue.equalsIgnoreCase(temporaryId.getValue()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        TemporaryID temporaryId = new TemporaryID();
        thrown.expect(IllegalArgumentException.class);
        temporaryId.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        TemporaryID temporaryId = new TemporaryID();
        String remainingBits = temporaryId.decodeUPER("010101001010101001010100100010100"); // 33
        assertTrue("0".equals(remainingBits));
        assertTrue("54AA548A".equalsIgnoreCase(temporaryId.getValue()));
    }

    @Test
    public void testHashCode() {

        TemporaryID temporaryId = new TemporaryID("abcd1234");
        TemporaryID temporaryId2 = new TemporaryID("4321fedc");

        assertFalse(temporaryId.hashCode() == temporaryId2.hashCode());
        assertTrue(temporaryId.hashCode() == temporaryId.hashCode());
        assertTrue(temporaryId2.hashCode() == temporaryId2.hashCode());

        TemporaryID temporaryId3 = new TemporaryID(temporaryId.getValue());

        assertTrue(temporaryId.hashCode() == temporaryId3.hashCode());
        assertFalse(temporaryId2.hashCode() == temporaryId3.hashCode());
    }

    @Test
    public void testHashCodeIgnoresCase() {

        TemporaryID temporaryId = new TemporaryID("A1A1A1A1");
        TemporaryID temporaryId2 = new TemporaryID("a1a1a1a1");
        assertTrue(temporaryId.hashCode() == temporaryId2.hashCode());
    }

    @Test
    public void testEquals() {

        TemporaryID temporaryId = new TemporaryID("01010101");

        assertFalse(temporaryId.equals(null));

        assertTrue(temporaryId.equals(temporaryId));

        TemporaryID temporaryId2 = new TemporaryID("a1a1a1a1");

        assertFalse(temporaryId.equals(new String()));
        assertFalse(temporaryId.equals(temporaryId2));

        temporaryId2.setValue(temporaryId.getValue());
        assertTrue(temporaryId.equals(temporaryId2));
    }

    @Test
    public void testEqualsIgnoresCase() {

        TemporaryID temporaryId = new TemporaryID("A1A1A1A1");
        TemporaryID temporaryId2 = new TemporaryID("a1a1a1a1");
        assertTrue(temporaryId.equals(temporaryId2));
    }
}
