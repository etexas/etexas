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
 * Unit tests for the approach ID element.
 * 
 * @author ttevendale
 */
public class ApproachIDTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        ApproachID approachId = new ApproachID(ApproachID.MIN);
        assertTrue(ApproachID.MIN == approachId.getValue());

        thrown.expect(IllegalArgumentException.class);
        approachId = new ApproachID(ApproachID.MIN - 1);
    }

    @Test
    public void testConstructorMax() {

        ApproachID approachId = new ApproachID(ApproachID.MAX);
        assertTrue(ApproachID.MAX == approachId.getValue());

        thrown.expect(IllegalArgumentException.class);
        approachId = new ApproachID(ApproachID.MAX + 1);
    }

    @Test
    public void testConstructor() {

        int randomNum = 11;
        ApproachID approachId = new ApproachID(randomNum);
        assertTrue(randomNum == approachId.getValue());
    }

    @Test
    public void testSetValueMin() {

        ApproachID approachId = new ApproachID();
        approachId.setValue(ApproachID.MIN);
        assertTrue(ApproachID.MIN == approachId.getValue());

        thrown.expect(IllegalArgumentException.class);
        approachId.setValue(ApproachID.MIN - 1);
    }

    @Test
    public void testSetValueMax() {

        ApproachID approachId = new ApproachID();
        approachId.setValue(ApproachID.MAX);
        assertTrue(ApproachID.MAX == approachId.getValue());

        thrown.expect(IllegalArgumentException.class);
        approachId.setValue(ApproachID.MAX + 1);
    }

    @Test
    public void testSetValue() {

        ApproachID approachId = new ApproachID();
        int randomNum = 2;
        approachId.setValue(randomNum);
        assertTrue(randomNum == approachId.getValue());
    }

    @Test
    public void testEncodeUPER() {

        // test min
        ApproachID approachId = new ApproachID(ApproachID.MIN);
        String encodedApproachId = approachId.encodeUPER();
        assertTrue("0000".equals(encodedApproachId));

        // test max
        approachId = new ApproachID(ApproachID.MAX);
        encodedApproachId = approachId.encodeUPER();
        assertTrue("1111".equals(encodedApproachId));

        int randomNumber = 3;
        approachId = new ApproachID(randomNumber);
        encodedApproachId = approachId.encodeUPER();
        assertTrue("0011".equals(encodedApproachId));
    }

    @Test
    public void testDecodeUPER() {

        ApproachID approachId = new ApproachID();

        // test min
        String remainingBits = approachId.decodeUPER("0000");
        assertTrue("".equals(remainingBits));
        assertTrue(ApproachID.MIN == approachId.getValue());

        // test max
        remainingBits = approachId.decodeUPER("1111");
        assertTrue("".equals(remainingBits));
        assertTrue(ApproachID.MAX == approachId.getValue());

        int expectedNumber = 10;
        remainingBits = approachId.decodeUPER("1010");
        assertTrue("".equals(remainingBits));
        assertTrue(expectedNumber == approachId.getValue());
    }

    @Test
    public void testDecodeUPERLessBits() {

        ApproachID approachId = new ApproachID();
        thrown.expect(IllegalArgumentException.class);
        approachId.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        ApproachID approachId = new ApproachID();
        String remainingBits = approachId.decodeUPER("1100110");
        assertTrue("110".equals(remainingBits));
        assertTrue(approachId.getValue() == 12);
    }

    @Test
    public void testHashCode() {

        ApproachID approachId = new ApproachID(5);
        ApproachID approachId2 = new ApproachID(10);

        assertFalse(approachId.hashCode() == approachId2.hashCode());
        assertTrue(approachId.hashCode() == approachId.hashCode());
        assertTrue(approachId2.hashCode() == approachId2.hashCode());

        ApproachID approachId3 = new ApproachID(approachId.getValue());

        assertTrue(approachId.hashCode() == approachId3.hashCode());
        assertFalse(approachId2.hashCode() == approachId3.hashCode());
    }

    @Test
    public void testEquals() {

        ApproachID approachId = new ApproachID(5);

        assertFalse(approachId.equals(null));

        assertTrue(approachId.equals(approachId));

        ApproachID approachId2 = new ApproachID(approachId.getValue() + 5);

        assertFalse(approachId.equals(new String()));
        assertFalse(approachId.equals(approachId2));

        approachId2.setValue(approachId.getValue());
        assertTrue(approachId.equals(approachId2));
    }
}
