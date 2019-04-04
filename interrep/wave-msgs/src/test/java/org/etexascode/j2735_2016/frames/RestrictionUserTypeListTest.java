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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.RestrictionAppliesTo.Restriction;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the restriction user type list frame.
 * 
 * @author ttevendale
 */
public class RestrictionUserTypeListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MIN_LIST_SIZE);
        assertTrue(restrictionUserTypes.getRestrictionArray().length == RestrictionUserTypeList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MAX_LIST_SIZE);
        assertTrue(restrictionUserTypes.getRestrictionArray().length == RestrictionUserTypeList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numRestrictions = 12;
        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(numRestrictions);
        assertTrue(restrictionUserTypes.getRestrictionArray().length == numRestrictions);
    }

    @Test
    public void testEncodeUPERMin() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MIN_LIST_SIZE);
        RestrictionUserType restrictionType = new RestrictionUserType(Restriction.SLOW_MOVING_PERSONS);
        RestrictionUserType[] restrictionTypeArray = restrictionUserTypes.getRestrictionArray();
        restrictionTypeArray[0] = restrictionType;

        String listSize = "0000";
        String remainingBits = restrictionType.encodeUPER();

        assertTrue((listSize + remainingBits).equals(restrictionUserTypes.encodeUPER()));

        restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        restrictionUserTypes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MAX_LIST_SIZE);

        RestrictionUserType[] restrictionTypeArray = restrictionUserTypes.getRestrictionArray();
        for (int i = 0; i < restrictionTypeArray.length; i++) {

            RestrictionUserType restrictionType = new RestrictionUserType(Restriction.WHEELCHAIR_USERS);
            restrictionTypeArray[i] = restrictionType;
            remainingBits += restrictionType.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(restrictionUserTypes.encodeUPER()));

        restrictionUserTypes = new RestrictionUserTypeList(RestrictionUserTypeList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        restrictionUserTypes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList();
        thrown.expect(IllegalStateException.class);
        restrictionUserTypes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        RestrictionUserType restrictionType = new RestrictionUserType(Restriction.HEIGHT_COMPLIANT);
        String listSize = "0000";
        String remainingBits = restrictionType.encodeUPER();

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList();
        restrictionUserTypes.decodeUPER(listSize + remainingBits);
        RestrictionUserType[] restrictionTypeArray = restrictionUserTypes.getRestrictionArray();
        assertTrue(RestrictionUserTypeList.MIN_LIST_SIZE == restrictionTypeArray.length);
        assertTrue(restrictionType.equals(restrictionTypeArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        RestrictionUserType restrictionType = new RestrictionUserType(Restriction.VISUAL_DISABILITIES);
        RestrictionUserType restrictionType2 = new RestrictionUserType(Restriction.AUDIO_DISABILITIES);

        String listSize = "1111";
        String remainingBits = restrictionType.encodeUPER();

        for (int i = 0; i < RestrictionUserTypeList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += restrictionType2.encodeUPER();
        }

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList();
        restrictionUserTypes.decodeUPER(listSize + remainingBits);

        RestrictionUserType[] restrictionTypeArray = restrictionUserTypes.getRestrictionArray();
        assertTrue(RestrictionUserTypeList.MAX_LIST_SIZE == restrictionTypeArray.length);
        assertTrue(restrictionType.equals(restrictionTypeArray[0]));
        for (int i = 1; i < RestrictionUserTypeList.MAX_LIST_SIZE; i++) {

            assertTrue(restrictionType2.equals(restrictionTypeArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList();
        thrown.expect(IllegalArgumentException.class);
        restrictionUserTypes.decodeUPER("001");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList();
        thrown.expect(IllegalArgumentException.class);
        // 0010 = 3 objects, but there's none
        restrictionUserTypes.decodeUPER("00101010100");
    }

    @Test
    public void testHashCode() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(1);
        restrictionUserTypes.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        assertTrue(restrictionUserTypes.hashCode() == restrictionUserTypes.hashCode());

        RestrictionUserTypeList restrictionUserTypes2 = new RestrictionUserTypeList(2);

        assertFalse(restrictionUserTypes.hashCode() == restrictionUserTypes2.hashCode());

        restrictionUserTypes2 = new RestrictionUserTypeList(1);
        restrictionUserTypes2.getRestrictionArray()[0] = new RestrictionUserType(Restriction.WEIGHT_COMPLIANT);

        assertFalse(restrictionUserTypes.hashCode() == restrictionUserTypes2.hashCode());

        restrictionUserTypes2.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        assertTrue(restrictionUserTypes.hashCode() == restrictionUserTypes2.hashCode());
    }

    @Test
    public void testEquals() {

        RestrictionUserTypeList restrictionUserTypes = new RestrictionUserTypeList(1);
        restrictionUserTypes.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        assertTrue(restrictionUserTypes.equals(restrictionUserTypes));
        assertFalse(restrictionUserTypes.equals(null));
        assertFalse(restrictionUserTypes.equals(new String()));

        RestrictionUserTypeList restrictionUserTypes2 = new RestrictionUserTypeList(2);

        assertFalse(restrictionUserTypes.equals(restrictionUserTypes2));

        restrictionUserTypes2 = new RestrictionUserTypeList(1);
        restrictionUserTypes2.getRestrictionArray()[0] = new RestrictionUserType(Restriction.WEIGHT_COMPLIANT);

        assertFalse(restrictionUserTypes.equals(restrictionUserTypes2));

        restrictionUserTypes2.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);

        assertTrue(restrictionUserTypes.equals(restrictionUserTypes2));
    }
}
