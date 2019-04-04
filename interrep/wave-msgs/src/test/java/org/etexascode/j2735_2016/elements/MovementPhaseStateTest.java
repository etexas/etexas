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

import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the movement phase state element.
 * 
 * @author ttevendale
 */
public class MovementPhaseStateTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        MovementPhaseState state = new MovementPhaseState(MovementPhase.UNAVAILABLE);
        assertTrue(MovementPhase.UNAVAILABLE.equals(state.getEnumeration()));

        // max value
        state = new MovementPhaseState(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        assertTrue(MovementPhase.CAUTION_CONFLICTING_TRAFFIC.equals(state.getEnumeration()));

        state = new MovementPhaseState(MovementPhase.DARK);
        assertTrue(MovementPhase.DARK.equals(state.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        MovementPhaseState state = new MovementPhaseState(MovementPhase.UNAVAILABLE);
        String encodedState = state.encodeUPER();
        assertTrue("0000".equals(encodedState));

        // test max
        state = new MovementPhaseState(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);
        encodedState = state.encodeUPER();
        assertTrue("1001".equals(encodedState));

        state = new MovementPhaseState(MovementPhase.PERMISSIVE_CLEARANCE);
        encodedState = state.encodeUPER();
        assertTrue("0111".equals(encodedState));
    }

    @Test
    public void testDecodeUPER() {

        MovementPhaseState state = new MovementPhaseState();

        // test min
        String remainingBits = state.decodeUPER("0000");
        assertTrue("".equals(remainingBits));
        assertTrue(MovementPhase.UNAVAILABLE.equals(state.getEnumeration()));

        // test max
        remainingBits = state.decodeUPER("1001");
        assertTrue("".equals(remainingBits));
        assertTrue(MovementPhase.CAUTION_CONFLICTING_TRAFFIC.equals(state.getEnumeration()));

        remainingBits = state.decodeUPER("0010");
        assertTrue("".equals(remainingBits));
        assertTrue(MovementPhase.STOP_THEN_PROCEED.equals(state.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        state.decodeUPER("1010");
    }

    @Test
    public void testDecodeUPERLessBits() {

        MovementPhaseState state = new MovementPhaseState();
        thrown.expect(IllegalArgumentException.class);
        state.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        MovementPhaseState state = new MovementPhaseState();
        String remainingBits = state.decodeUPER("0011010"); // 7
        assertTrue("010".equals(remainingBits));
        assertTrue(MovementPhase.STOP_AND_REMAIN.equals(state.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        MovementPhaseState state = new MovementPhaseState(MovementPhase.PRE_MOVEMENT);
        MovementPhaseState state2 = new MovementPhaseState(MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED);

        assertFalse(state.hashCode() == state2.hashCode());
        assertTrue(state.hashCode() == state.hashCode());
        assertTrue(state2.hashCode() == state2.hashCode());

        MovementPhaseState state3 = new MovementPhaseState(state.getEnumeration());

        assertTrue(state.hashCode() == state3.hashCode());
        assertFalse(state2.hashCode() == state3.hashCode());
    }

    @Test
    public void testEquals() {

        MovementPhaseState state = new MovementPhaseState(MovementPhase.PROTECTED_MOVEMENT_ALLOWED);

        assertFalse(state.equals(null));

        assertTrue(state.equals(state));

        MovementPhaseState state2 = new MovementPhaseState(MovementPhase.PROTECTED_CLEARANCE);

        assertFalse(state.equals(new String()));
        assertFalse(state.equals(state2));

        state2.setEnumeration(state.getEnumeration());
        assertTrue(state.equals(state2));
    }
}
