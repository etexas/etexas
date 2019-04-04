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
 * Unit tests for the allowed maneuvers element.
 * 
 * @author ttevendale
 */
public class AllowedManeuversTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();

        // min
        assertTrue("000000000000".equals(maneuvers.encodeUPER()));

        maneuvers.setManeuverStraightAllowed(true);
        maneuvers.setManeuverLeftAllowed(true);

        // some turned on
        assertTrue("110000000000".equals(maneuvers.encodeUPER()));

        maneuvers.setManeuverRightAllowed(true);
        maneuvers.setManeuverUTurnAllowed(true);
        maneuvers.setManeuverLeftTurnOnRedAllowed(true);
        maneuvers.setManeuverRightTurnOnRedAllowed(true);
        maneuvers.setManeuverLaneChangeAllowed(true);
        maneuvers.setManeuverNoStoppingAllowed(true);
        maneuvers.setYieldAllwaysRequired(true);
        maneuvers.setGoWithHalt(true);
        maneuvers.setCaution(true);
        maneuvers.setReserved1(true);

        // max
        assertTrue("111111111111".equals(maneuvers.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();

        // min
        String remainingBits = maneuvers.decodeUPER("000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());
        assertFalse(maneuvers.isReserved1());

        maneuvers.setManeuverUTurnAllowed(true);
        maneuvers.setManeuverNoStoppingAllowed(true);
        maneuvers.setYieldAllwaysRequired(true);

        // some turned on
        remainingBits = maneuvers.decodeUPER("000100011000");
        assertTrue("".equals(remainingBits));
        assertFalse(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.isManeuverRightAllowed());
        assertTrue(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverLaneChangeAllowed());
        assertTrue(maneuvers.isManeuverNoStoppingAllowed());
        assertTrue(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());
        assertFalse(maneuvers.isReserved1());

        maneuvers.setManeuverStraightAllowed(true);
        maneuvers.setManeuverLeftAllowed(true);
        maneuvers.setManeuverRightAllowed(true);
        maneuvers.setManeuverLeftTurnOnRedAllowed(true);
        maneuvers.setManeuverRightTurnOnRedAllowed(true);
        maneuvers.setManeuverLaneChangeAllowed(true);
        maneuvers.setGoWithHalt(true);
        maneuvers.setCaution(true);
        maneuvers.setReserved1(true);

        // max
        remainingBits = maneuvers.decodeUPER("111111111111");
        assertTrue("".equals(remainingBits));
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertTrue(maneuvers.isManeuverLeftAllowed());
        assertTrue(maneuvers.isManeuverRightAllowed());
        assertTrue(maneuvers.isManeuverUTurnAllowed());
        assertTrue(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertTrue(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertTrue(maneuvers.isManeuverLaneChangeAllowed());
        assertTrue(maneuvers.isManeuverNoStoppingAllowed());
        assertTrue(maneuvers.isYieldAllwaysRequired());
        assertTrue(maneuvers.isGoWithHalt());
        assertTrue(maneuvers.isCaution());
        assertTrue(maneuvers.isReserved1());
    }

    @Test
    public void testDecodeUPERLessBits() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();
        thrown.expect(IllegalArgumentException.class);
        maneuvers.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();
        String remainingBits = maneuvers.decodeUPER("10101010000011100");
        assertTrue("11100".equals(remainingBits));
        assertTrue(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.isManeuverLeftAllowed());
        assertTrue(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.isManeuverUTurnAllowed());
        assertTrue(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertTrue(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.isCaution());
        assertFalse(maneuvers.isReserved1());
    }

    @Test
    public void testHashCode() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();
        maneuvers.setManeuverLeftTurnOnRedAllowed(true);
        maneuvers.setCaution(true);

        AllowedManeuvers maneuvers2 = new AllowedManeuvers();
        maneuvers2.setManeuverLeftTurnOnRedAllowed(false);
        maneuvers2.setCaution(false);

        assertFalse(maneuvers.hashCode() == maneuvers2.hashCode());
        assertTrue(maneuvers.hashCode() == maneuvers.hashCode());
        assertTrue(maneuvers2.hashCode() == maneuvers2.hashCode());

        AllowedManeuvers maneuvers3 = new AllowedManeuvers();
        maneuvers3.setManeuverStraightAllowed(maneuvers.isManeuverStraightAllowed());
        maneuvers3.setManeuverLeftAllowed(maneuvers.isManeuverLeftAllowed());
        maneuvers3.setManeuverRightAllowed(maneuvers.isManeuverRightAllowed());
        maneuvers3.setManeuverUTurnAllowed(maneuvers.isManeuverUTurnAllowed());
        maneuvers3.setManeuverLeftTurnOnRedAllowed(maneuvers.isManeuverLeftTurnOnRedAllowed());
        maneuvers3.setManeuverRightTurnOnRedAllowed(maneuvers.isManeuverRightTurnOnRedAllowed());
        maneuvers3.setManeuverLaneChangeAllowed(maneuvers.isManeuverLaneChangeAllowed());
        maneuvers3.setManeuverNoStoppingAllowed(maneuvers.isManeuverNoStoppingAllowed());
        maneuvers3.setYieldAllwaysRequired(maneuvers.isYieldAllwaysRequired());
        maneuvers3.setGoWithHalt(maneuvers.isGoWithHalt());
        maneuvers3.setCaution(maneuvers.isCaution());
        maneuvers3.setReserved1(maneuvers.isReserved1());

        assertTrue(maneuvers.hashCode() == maneuvers3.hashCode());
        assertFalse(maneuvers2.hashCode() == maneuvers3.hashCode());
    }

    @Test
    public void testEquals() {

        AllowedManeuvers maneuvers = new AllowedManeuvers();
        maneuvers.setManeuverStraightAllowed(true);
        maneuvers.setManeuverLeftAllowed(true);
        maneuvers.setManeuverRightAllowed(true);
        maneuvers.setManeuverUTurnAllowed(true);
        maneuvers.setManeuverLeftTurnOnRedAllowed(true);
        maneuvers.setManeuverRightTurnOnRedAllowed(true);
        maneuvers.setManeuverLaneChangeAllowed(true);
        maneuvers.setManeuverNoStoppingAllowed(true);
        maneuvers.setYieldAllwaysRequired(true);
        maneuvers.setGoWithHalt(true);
        maneuvers.setCaution(true);
        maneuvers.setReserved1(true);

        assertFalse(maneuvers.equals(null));

        assertTrue(maneuvers.equals(maneuvers));

        AllowedManeuvers maneuvers2 = new AllowedManeuvers();
        maneuvers2.setManeuverStraightAllowed(false);
        maneuvers2.setManeuverLeftAllowed(false);
        maneuvers2.setManeuverRightAllowed(false);
        maneuvers2.setManeuverUTurnAllowed(false);
        maneuvers2.setManeuverLeftTurnOnRedAllowed(false);
        maneuvers2.setManeuverRightTurnOnRedAllowed(false);
        maneuvers2.setManeuverLaneChangeAllowed(false);
        maneuvers2.setManeuverNoStoppingAllowed(false);
        maneuvers2.setYieldAllwaysRequired(false);
        maneuvers2.setGoWithHalt(false);
        maneuvers2.setCaution(false);
        maneuvers2.setReserved1(false);

        assertFalse(maneuvers.equals(new String()));
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverStraightAllowed(maneuvers.isManeuverStraightAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverLeftAllowed(maneuvers.isManeuverLeftAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverRightAllowed(maneuvers.isManeuverRightAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverUTurnAllowed(maneuvers.isManeuverUTurnAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverLeftTurnOnRedAllowed(maneuvers.isManeuverLeftTurnOnRedAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverRightTurnOnRedAllowed(maneuvers.isManeuverRightTurnOnRedAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverLaneChangeAllowed(maneuvers.isManeuverLaneChangeAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setManeuverNoStoppingAllowed(maneuvers.isManeuverNoStoppingAllowed());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setYieldAllwaysRequired(maneuvers.isYieldAllwaysRequired());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setGoWithHalt(maneuvers.isGoWithHalt());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setCaution(maneuvers.isCaution());
        assertFalse(maneuvers.equals(maneuvers2));

        maneuvers2.setReserved1(maneuvers.isReserved1());
        assertTrue(maneuvers.equals(maneuvers2));
    }
}
