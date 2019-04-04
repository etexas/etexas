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

import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the movement list frame.
 * 
 * @author ttevendale
 */
public class MovementListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        MovementList movements = new MovementList(MovementList.MIN_LIST_SIZE);
        assertTrue(movements.getMovementArray().length == MovementList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        movements = new MovementList(MovementList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        MovementList movements = new MovementList(MovementList.MAX_LIST_SIZE);
        assertTrue(movements.getMovementArray().length == MovementList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        movements = new MovementList(MovementList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numMovements = 12;
        MovementList movements = new MovementList(numMovements);
        assertTrue(movements.getMovementArray().length == numMovements);
    }

    @Test
    public void testEncodeUPERMin() {

        MovementList movements = new MovementList(MovementList.MIN_LIST_SIZE);

        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);
        MovementState movementState = new MovementState(23, stateTimeSpeed);

        MovementState[] movementArray = movements.getMovementArray();
        movementArray[0] = movementState;

        String listSize = "00000000";
        String remainingBits = movementState.encodeUPER();

        assertTrue((listSize + remainingBits).equals(movements.encodeUPER()));

        movements = new MovementList(MovementList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        movements.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111110";
        String remainingBits = "";

        MovementList movements = new MovementList(MovementList.MAX_LIST_SIZE);

        MovementState[] movementArray = movements.getMovementArray();
        for (int i = 0; i < movementArray.length; i++) {

            MovementEventList stateTimeSpeed = new MovementEventList(1);
            stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

            MovementState movementState = new MovementState(i, stateTimeSpeed);
            movementArray[i] = movementState;
            remainingBits += movementState.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(movements.encodeUPER()));

        movements = new MovementList(MovementList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        movements.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        MovementList movements = new MovementList();
        thrown.expect(IllegalStateException.class);
        movements.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);
        MovementState movementState = new MovementState(254, stateTimeSpeed);

        String listSize = "00000000";
        String remainingBits = movementState.encodeUPER();

        MovementList movements = new MovementList();
        movements.decodeUPER(listSize + remainingBits);
        MovementState[] movementArray = movements.getMovementArray();
        assertTrue(MovementList.MIN_LIST_SIZE == movementArray.length);
        assertTrue(movementState.equals(movementArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        String listSize = "11111110";
        String remainingBits = "";

        MovementList expectedMovements = new MovementList(MovementList.MAX_LIST_SIZE);

        for (int i = 0; i < MovementList.MAX_LIST_SIZE; i++) {

            MovementEventList stateTimeSpeed = new MovementEventList(1);
            stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);
            MovementState movementState = new MovementState(i, stateTimeSpeed);

            expectedMovements.getMovementArray()[i] = movementState;
            remainingBits += movementState.encodeUPER();
        }

        MovementList movements = new MovementList();
        movements.decodeUPER(listSize + remainingBits);

        assertTrue(expectedMovements.equals(movements));
    }

    @Test
    public void testDecodeUPERAboveMax() {

        String listSize = "11111111";
        MovementList movements = new MovementList();
        thrown.expect(IllegalArgumentException.class);
        movements.decodeUPER(listSize);
    }

    @Test
    public void testDecodeUPERLessBits() {

        MovementList movements = new MovementList();
        thrown.expect(IllegalArgumentException.class);
        movements.decodeUPER("010");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        MovementList movements = new MovementList();
        thrown.expect(IllegalArgumentException.class);
        // 1011 = 12 objects, but there's none
        movements.decodeUPER("10111010100");
    }

    @Test
    public void testHashCode() {

        MovementList movements = new MovementList(1);
        movements.getMovementArray()[0] = createMovementState();

        assertTrue(movements.hashCode() == movements.hashCode());

        MovementList movements2 = new MovementList(2);

        assertFalse(movements.hashCode() == movements2.hashCode());

        movements2 = new MovementList(1);
        movements2.getMovementArray()[0] = createMovementState2();

        assertFalse(movements.hashCode() == movements2.hashCode());

        movements2.getMovementArray()[0] = createMovementState();

        assertTrue(movements.hashCode() == movements2.hashCode());
    }

    @Test
    public void testEquals() {

        MovementList movements = new MovementList(1);
        movements.getMovementArray()[0] = createMovementState();

        assertTrue(movements.equals(movements));
        assertFalse(movements.equals(null));
        assertFalse(movements.equals(new String()));

        MovementList movements2 = new MovementList(2);

        assertFalse(movements.equals(movements2));

        movements2 = new MovementList(1);
        movements2.getMovementArray()[0] = createMovementState2();

        assertFalse(movements.equals(movements2));

        movements2.getMovementArray()[0] = createMovementState();

        assertTrue(movements.equals(movements2));
    }

    private MovementState createMovementState() {

        MovementEventList stateTimeSpeed = new MovementEventList(1);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        return new MovementState(151, stateTimeSpeed);
    }

    private MovementState createMovementState2() {

        MovementEventList stateTimeSpeed = new MovementEventList(3);
        stateTimeSpeed.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PROTECTED_CLEARANCE);
        stateTimeSpeed.getMovementEventArray()[1] = new MovementEvent(MovementPhase.STOP_AND_REMAIN);
        stateTimeSpeed.getMovementEventArray()[2] = new MovementEvent(MovementPhase.STOP_THEN_PROCEED);

        return new MovementState(10, stateTimeSpeed);
    }
}
