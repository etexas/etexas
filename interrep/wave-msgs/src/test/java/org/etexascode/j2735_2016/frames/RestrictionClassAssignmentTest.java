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
 * Unit tests for the restriction class assignment frame.
 * 
 * @author ttevendale
 */
public class RestrictionClassAssignmentTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    RestrictionClassID id;

    RestrictionUserTypeList users;

    @Before
    public void init() {

        id = new RestrictionClassID(51);
        users = new RestrictionUserTypeList(1);
        users.getRestrictionArray()[0] = new RestrictionUserType(Restriction.EQUIPPED_TRANSIT);
    }

    @Test
    public void testConstructor() {

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment(id, users);

        assertTrue(id.equals(restrictionClass.getId()));
        assertTrue(users.equals(restrictionClass.getUsers()));
    }

    @Test
    public void testConstructorNullId() {

        thrown.expect(NullPointerException.class);
        new RestrictionClassAssignment(null, new RestrictionUserTypeList());
    }

    @Test
    public void testConstructorNullUsers() {

        thrown.expect(NullPointerException.class);
        new RestrictionClassAssignment(new RestrictionClassID(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        int id = 83;

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment(id, users);

        assertTrue(id == restrictionClass.getId().getValue());
        assertTrue(users.equals(restrictionClass.getUsers()));
    }

    @Test
    public void testConstructorPrimitiveNullUsers() {

        thrown.expect(NullPointerException.class);
        new RestrictionClassAssignment(0, null);
    }

    @Test
    public void testSetId() {

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment();
        restrictionClass.setId(id);

        assertTrue(id.equals(restrictionClass.getId()));

        thrown.expect(NullPointerException.class);
        restrictionClass.setId(null);
    }

    @Test
    public void testSetIdPrimitive() {

        int id = 35;

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment();
        restrictionClass.setId(id);

        assertTrue(id == restrictionClass.getId().getValue());
    }

    @Test
    public void testSetUsers() {

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment();
        restrictionClass.setUsers(users);

        assertTrue(users.equals(restrictionClass.getUsers()));

        thrown.expect(NullPointerException.class);
        restrictionClass.setUsers(null);
    }

    @Test
    public void testEncodeUPER() {

        String encodedBits = id.encodeUPER() + users.encodeUPER();
        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment(id, users);
        assertTrue(encodedBits.equalsIgnoreCase(restrictionClass.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        String encodedBits = id.encodeUPER() + users.encodeUPER();
        RestrictionClassAssignment restrictionClassAssignment = new RestrictionClassAssignment();
        restrictionClassAssignment.decodeUPER(encodedBits);

        assertTrue(id.equals(restrictionClassAssignment.getId()));
        assertTrue(users.equals(restrictionClassAssignment.getUsers()));
    }

    @Test
    public void testHashCode() {

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment(id, users);

        int id = restrictionClass.getId().getValue();
        RestrictionUserTypeList users = restrictionClass.getUsers();

        RestrictionUserTypeList diffUsers = new RestrictionUserTypeList(users.getRestrictionArray().length + 1);

        RestrictionClassAssignment restrictionClass2 = new RestrictionClassAssignment(id + 1, diffUsers);

        assertFalse(restrictionClass.hashCode() == restrictionClass2.hashCode());
        assertTrue(restrictionClass.hashCode() == restrictionClass.hashCode());
        assertTrue(restrictionClass2.hashCode() == restrictionClass2.hashCode());

        RestrictionClassAssignment restrictionClass3 = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClass.hashCode() == restrictionClass3.hashCode());
        assertFalse(restrictionClass2.hashCode() == restrictionClass3.hashCode());
    }

    @Test
    public void testEquals() {

        RestrictionClassAssignment restrictionClass = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClass.equals(restrictionClass));
        assertFalse(restrictionClass.equals(null));
        assertFalse(restrictionClass.equals(new String()));

        int id = restrictionClass.getId().getValue();
        RestrictionUserTypeList users = restrictionClass.getUsers();

        RestrictionUserTypeList diffUsers = new RestrictionUserTypeList(users.getRestrictionArray().length + 1);

        // different
        RestrictionClassAssignment restrictionClass2 = new RestrictionClassAssignment(id + 1, diffUsers);

        assertFalse(restrictionClass.equals(restrictionClass2));

        // different id
        restrictionClass2 = new RestrictionClassAssignment(id + 1, users);

        assertFalse(restrictionClass.equals(restrictionClass2));

        // different users
        restrictionClass2 = new RestrictionClassAssignment(id, diffUsers);

        assertFalse(restrictionClass.equals(restrictionClass2));

        // same
        restrictionClass2 = new RestrictionClassAssignment(id, users);

        assertTrue(restrictionClass.equals(restrictionClass2));
    }
}
