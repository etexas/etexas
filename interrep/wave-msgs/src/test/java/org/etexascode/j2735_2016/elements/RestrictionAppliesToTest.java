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

import org.etexascode.j2735_2016.elements.RestrictionAppliesTo.Restriction;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the restriction applies to element.
 * 
 * @author ttevendale
 */
public class RestrictionAppliesToTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        RestrictionAppliesTo restriction = new RestrictionAppliesTo(Restriction.NONE);
        assertTrue(Restriction.NONE.equals(restriction.getEnumeration()));

        // max value
        restriction = new RestrictionAppliesTo(Restriction.OTHER_UNKNOWN_DISABILITIES);
        assertTrue(Restriction.OTHER_UNKNOWN_DISABILITIES.equals(restriction.getEnumeration()));

        restriction = new RestrictionAppliesTo(Restriction.EQUIPPED_TRANSIT);
        assertTrue(Restriction.EQUIPPED_TRANSIT.equals(restriction.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        RestrictionAppliesTo restriction = new RestrictionAppliesTo(Restriction.NONE);
        String encodedStatus = restriction.encodeUPER();
        assertTrue("00000".equals(encodedStatus));

        // test max
        restriction = new RestrictionAppliesTo(Restriction.OTHER_UNKNOWN_DISABILITIES);
        encodedStatus = restriction.encodeUPER();
        assertTrue("01101".equals(encodedStatus));

        restriction = new RestrictionAppliesTo(Restriction.EQUIPPED_TAXIS);
        encodedStatus = restriction.encodeUPER();
        assertTrue("00010".equals(encodedStatus));
    }

    @Test
    public void testDecodeUPER() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo();

        // test min
        String remainingBits = restriction.decodeUPER("00000");
        assertTrue("".equals(remainingBits));
        assertTrue(Restriction.NONE.equals(restriction.getEnumeration()));

        // test max
        remainingBits = restriction.decodeUPER("01101");
        assertTrue("".equals(remainingBits));
        assertTrue(Restriction.OTHER_UNKNOWN_DISABILITIES.equals(restriction.getEnumeration()));

        remainingBits = restriction.decodeUPER("00011");
        assertTrue("".equals(remainingBits));
        assertTrue(Restriction.EQUIPPED_OTHER.equals(restriction.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        restriction.decodeUPER("01110");
    }

    @Test
    public void testDecodeUPERExtension() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo();
        thrown.expect(IllegalArgumentException.class);
        restriction.decodeUPER("10000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo();
        thrown.expect(IllegalArgumentException.class);
        restriction.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo();
        String remainingBits = restriction.decodeUPER("00100110");
        assertTrue("110".equals(remainingBits));
        assertTrue(Restriction.EMISSION_COMPLIANT.equals(restriction.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo(Restriction.EQUIPPED_BICYCLE);
        RestrictionAppliesTo restriction2 = new RestrictionAppliesTo(Restriction.WEIGHT_COMPLIANT);

        assertFalse(restriction.hashCode() == restriction2.hashCode());
        assertTrue(restriction.hashCode() == restriction.hashCode());
        assertTrue(restriction2.hashCode() == restriction2.hashCode());

        RestrictionAppliesTo restriction3 = new RestrictionAppliesTo(restriction.getEnumeration());

        assertTrue(restriction.hashCode() == restriction3.hashCode());
        assertFalse(restriction2.hashCode() == restriction3.hashCode());
    }

    @Test
    public void testEquals() {

        RestrictionAppliesTo restriction = new RestrictionAppliesTo(Restriction.HEIGHT_COMPLIANT);

        assertFalse(restriction.equals(null));

        assertTrue(restriction.equals(restriction));

        RestrictionAppliesTo restriction2 = new RestrictionAppliesTo(Restriction.PEDESTRIANS);

        assertFalse(restriction.equals(new String()));
        assertFalse(restriction.equals(restriction2));

        restriction2.setEnumeration(restriction.getEnumeration());
        assertTrue(restriction.equals(restriction2));
    }
}
