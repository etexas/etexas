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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.RestrictionAppliesTo;
import org.etexascode.j2735_2016.elements.RestrictionAppliesTo.Restriction;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the restriction user type frame (Choice).
 * 
 * @author ttevendale
 */
public class RestrictionUserTypeTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorRestrictionAppliesTo() {

        RestrictionAppliesTo restrictionAppliesTo = new RestrictionAppliesTo(Restriction.EQUIPPED_TAXIS);

        RestrictionUserType restrictionType = new RestrictionUserType(restrictionAppliesTo);

        assertTrue(restrictionAppliesTo.equals(restrictionType.getBasicType()));

        thrown.expect(NullPointerException.class);
        new RestrictionUserType((RestrictionAppliesTo)null);
    }

    @Test
    public void testEncodeUPERRestrictionAppliesTo() {

        RestrictionAppliesTo restrictionAppliesTo = new RestrictionAppliesTo(Restriction.AUDIO_DISABILITIES);
        RestrictionUserType restrictionType = new RestrictionUserType(restrictionAppliesTo);

        String restrictionTypeChoice = "00";
        String remainingBits = restrictionAppliesTo.encodeUPER();

        assertTrue((restrictionTypeChoice + remainingBits).equals(restrictionType.encodeUPER()));
    }

    @Test
    public void testEncodeUPERAllNull() {

        RestrictionUserType restrictionType = new RestrictionUserType();
        thrown.expect(IllegalStateException.class);
        restrictionType.encodeUPER();
    }

    @Test
    public void testDecodeUPERRestrictionAppliesTo() {

        RestrictionAppliesTo restrictionAppliesTo = new RestrictionAppliesTo(Restriction.NONE);

        String restrictionTypeChoice = "00";

        RestrictionUserType restrictionType = new RestrictionUserType();
        restrictionType.decodeUPER(restrictionTypeChoice + restrictionAppliesTo.encodeUPER());

        assertTrue(restrictionAppliesTo.equals(restrictionType.getBasicType()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String choice = "10";

        RestrictionUserType restrictionType = new RestrictionUserType();
        thrown.expect(IllegalArgumentException.class);
        restrictionType.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String choice = "01";

        RestrictionUserType restrictionType = new RestrictionUserType();
        thrown.expect(IllegalArgumentException.class);
        restrictionType.decodeUPER(choice);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String choice = "";

        RestrictionUserType restrictionType = new RestrictionUserType();
        thrown.expect(IllegalArgumentException.class);
        restrictionType.decodeUPER(choice);
    }

    @Test
    public void testHashCode() {

        RestrictionUserType restrictionType = new RestrictionUserType(Restriction.EQUIPPED_TAXIS);

        RestrictionUserType restrictionType2 = new RestrictionUserType(Restriction.OTHER_UNKNOWN_DISABILITIES);

        assertFalse(restrictionType.hashCode() == restrictionType2.hashCode());
        assertTrue(restrictionType.hashCode() == restrictionType.hashCode());
        assertTrue(restrictionType2.hashCode() == restrictionType2.hashCode());

        RestrictionUserType restrictionType3 = new RestrictionUserType(Restriction.EQUIPPED_TAXIS);

        assertTrue(restrictionType.hashCode() == restrictionType3.hashCode());
        assertFalse(restrictionType2.hashCode() == restrictionType3.hashCode());
    }

    @Test
    public void testEquals() {

        RestrictionUserType restrictionType = new RestrictionUserType(Restriction.EQUIPPED_TAXIS);

        assertTrue(restrictionType.equals(restrictionType));
        assertFalse(restrictionType.equals(null));
        assertFalse(restrictionType.equals(new String()));

        // different restriction applies to
        RestrictionUserType restrictionType2 = new RestrictionUserType(Restriction.PEDESTRIANS);
        assertFalse(restrictionType.equals(restrictionType2));

        // same restriction applies to
        restrictionType2 = new RestrictionUserType(Restriction.EQUIPPED_TAXIS);
        assertTrue(restrictionType.equals(restrictionType2));
    }
}
