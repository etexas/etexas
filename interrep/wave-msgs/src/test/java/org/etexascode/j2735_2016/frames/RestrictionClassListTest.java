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
import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the restriction class list frame.
 * 
 * @author ttevendale
 */
public class RestrictionClassListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    RestrictionClassID id;

    RestrictionUserTypeList users;

    RestrictionClassID id2;

    RestrictionUserTypeList users2;

    @Before
    public void init() {

        id = new RestrictionClassID(121);
        users = new RestrictionUserTypeList(2);
        users.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EQUIPPED_TRANSIT);
        users.getRestrictionArray()[1] = new RestrictionUserType(Restriction.NONE);

        id2 = new RestrictionClassID(5);
        users2 = new RestrictionUserTypeList(1);
        users2.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EMISSION_COMPLIANT);
    }

    @Test
    public void testConstructorMin() {

        RestrictionClassList restrictionClasses = new RestrictionClassList(RestrictionClassList.MIN_LIST_SIZE);
        assertTrue(restrictionClasses.getRestrictionArray().length == RestrictionClassList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        restrictionClasses = new RestrictionClassList(RestrictionClassList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        RestrictionClassList restrictionClasses = new RestrictionClassList(RestrictionClassList.MAX_LIST_SIZE);
        assertTrue(restrictionClasses.getRestrictionArray().length == RestrictionClassList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        restrictionClasses = new RestrictionClassList(RestrictionClassList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numRestrictions = 3;
        RestrictionClassList restrictionClasses = new RestrictionClassList(numRestrictions);
        assertTrue(restrictionClasses.getRestrictionArray().length == numRestrictions);
    }

    @Test
    public void testEncodeUPERMin() {

        RestrictionClassList restrictionClasses = new RestrictionClassList(RestrictionClassList.MIN_LIST_SIZE);
        RestrictionClassAssignment restriction = new RestrictionClassAssignment(id, users);
        RestrictionClassAssignment[] restrictionArray = restrictionClasses.getRestrictionArray();
        restrictionArray[0] = restriction;

        String listSize = "00000000";
        String remainingBits = restriction.encodeUPER();

        assertTrue((listSize + remainingBits).equals(restrictionClasses.encodeUPER()));

        restrictionClasses = new RestrictionClassList(RestrictionClassList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        restrictionClasses.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111101";
        String remainingBits = "";

        RestrictionClassList restrictionClasses = new RestrictionClassList(RestrictionClassList.MAX_LIST_SIZE);

        RestrictionClassAssignment[] restrictionArray = restrictionClasses.getRestrictionArray();
        for (int i = 0; i < restrictionArray.length; i++) {

            RestrictionClassAssignment restriction = new RestrictionClassAssignment(id, users);
            restrictionArray[i] = restriction;
            remainingBits += restriction.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(restrictionClasses.encodeUPER()));

        restrictionClasses = new RestrictionClassList(RestrictionClassList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        restrictionClasses.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        RestrictionClassList restrictionClasses = new RestrictionClassList();
        thrown.expect(IllegalStateException.class);
        restrictionClasses.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        RestrictionClassAssignment restriction = new RestrictionClassAssignment(id, users);
        String listSize = "00000000";
        String remainingBits = restriction.encodeUPER();

        RestrictionClassList restrictionClasses = new RestrictionClassList();
        restrictionClasses.decodeUPER(listSize + remainingBits);
        RestrictionClassAssignment[] restrictionArray = restrictionClasses.getRestrictionArray();
        assertTrue(RestrictionClassList.MIN_LIST_SIZE == restrictionArray.length);
        assertTrue(restriction.equals(restrictionArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        RestrictionClassAssignment restriction = new RestrictionClassAssignment(id, users);
        RestrictionClassAssignment restriction2 = new RestrictionClassAssignment(id2, users2);

        String listSize = "11111101";
        String remainingBits = restriction.encodeUPER();

        for (int i = 0; i < RestrictionClassList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += restriction2.encodeUPER();
        }

        RestrictionClassList restrictionClasses = new RestrictionClassList();
        restrictionClasses.decodeUPER(listSize + remainingBits);

        RestrictionClassAssignment[] restrictionArray = restrictionClasses.getRestrictionArray();
        assertTrue(RestrictionClassList.MAX_LIST_SIZE == restrictionArray.length);
        assertTrue(restriction.equals(restrictionArray[0]));
        for (int i = 1; i < RestrictionClassList.MAX_LIST_SIZE; i++) {

            assertTrue(restriction2.equals(restrictionArray[i]));
        }
    }

    @Test
    public void testDecodeUPERAboveMax() {

        String listSize = "11111111";
        RestrictionClassList restrictionClasses = new RestrictionClassList();
        thrown.expect(IllegalArgumentException.class);
        restrictionClasses.decodeUPER(listSize);
    }

    @Test
    public void testDecodeUPERLessBits() {

        RestrictionClassList restrictionClasses = new RestrictionClassList();
        thrown.expect(IllegalArgumentException.class);
        restrictionClasses.decodeUPER("1101001");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        RestrictionClassList restrictionClasses = new RestrictionClassList();
        thrown.expect(IllegalArgumentException.class);
        // 10011010 = 155 objects, but there's none
        restrictionClasses.decodeUPER("10011010100");
    }

    @Test
    public void testHashCode() {

        RestrictionClassList restrictionClasses = new RestrictionClassList(1);
        restrictionClasses.getRestrictionArray()[0] = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClasses.hashCode() == restrictionClasses.hashCode());

        RestrictionClassList restrictionClasses2 = new RestrictionClassList(2);

        assertFalse(restrictionClasses.hashCode() == restrictionClasses2.hashCode());

        restrictionClasses2 = new RestrictionClassList(1);
        restrictionClasses2.getRestrictionArray()[0] = new RestrictionClassAssignment(id2, users2);

        assertFalse(restrictionClasses.hashCode() == restrictionClasses2.hashCode());

        restrictionClasses2.getRestrictionArray()[0] = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClasses.hashCode() == restrictionClasses2.hashCode());
    }

    @Test
    public void testEquals() {

        RestrictionClassList restrictionClasses = new RestrictionClassList(1);
        restrictionClasses.getRestrictionArray()[0] = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClasses.equals(restrictionClasses));
        assertFalse(restrictionClasses.equals(null));
        assertFalse(restrictionClasses.equals(new String()));

        RestrictionClassList restrictionClasses2 = new RestrictionClassList(2);

        assertFalse(restrictionClasses.equals(restrictionClasses2));

        restrictionClasses2 = new RestrictionClassList(1);
        restrictionClasses2.getRestrictionArray()[0] = new RestrictionClassAssignment(id2, users2);

        assertFalse(restrictionClasses.equals(restrictionClasses2));

        restrictionClasses2.getRestrictionArray()[0] = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClasses.equals(restrictionClasses2));
    }
}
