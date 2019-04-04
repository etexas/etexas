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

import org.etexascode.j2735_2016.elements.LaneID;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the enabled lane list frame.
 * 
 * @author ttevendale
 */
public class EnabledLaneListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        EnabledLaneList enabledLanes = new EnabledLaneList(EnabledLaneList.MIN_LIST_SIZE);
        assertTrue(enabledLanes.getEnabledLaneArray().length == EnabledLaneList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        enabledLanes = new EnabledLaneList(EnabledLaneList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        EnabledLaneList enabledLanes = new EnabledLaneList(EnabledLaneList.MAX_LIST_SIZE);
        assertTrue(enabledLanes.getEnabledLaneArray().length == EnabledLaneList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        enabledLanes = new EnabledLaneList(EnabledLaneList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numEnabledLanes = 12;
        EnabledLaneList enabledLanes = new EnabledLaneList(numEnabledLanes);
        assertTrue(enabledLanes.getEnabledLaneArray().length == numEnabledLanes);
    }

    @Test
    public void testEncodeUPERMin() {

        EnabledLaneList enabledLanes = new EnabledLaneList(EnabledLaneList.MIN_LIST_SIZE);
        LaneID id = new LaneID(20);
        LaneID[] enabledLaneArray = enabledLanes.getEnabledLaneArray();
        enabledLaneArray[0] = id;

        String listSize = "0000";
        String remainingBits = id.encodeUPER();

        assertTrue((listSize + remainingBits).equals(enabledLanes.encodeUPER()));

        enabledLanes = new EnabledLaneList(EnabledLaneList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        enabledLanes.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        EnabledLaneList enabledLanes = new EnabledLaneList(EnabledLaneList.MAX_LIST_SIZE);

        LaneID[] enabledLaneArray = enabledLanes.getEnabledLaneArray();
        for (int i = 0; i < enabledLaneArray.length; i++) {

            LaneID id = new LaneID(5);
            enabledLaneArray[i] = id;
            remainingBits += id.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(enabledLanes.encodeUPER()));

        enabledLanes = new EnabledLaneList(EnabledLaneList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        enabledLanes.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        EnabledLaneList enabledLanes = new EnabledLaneList();
        thrown.expect(IllegalStateException.class);
        enabledLanes.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        LaneID id = new LaneID(255);
        String listSize = "0000";
        String remainingBits = id.encodeUPER();

        EnabledLaneList enabledLanes = new EnabledLaneList();
        enabledLanes.decodeUPER(listSize + remainingBits);
        LaneID[] enabledLaneArray = enabledLanes.getEnabledLaneArray();
        assertTrue(EnabledLaneList.MIN_LIST_SIZE == enabledLaneArray.length);
        assertTrue(id.equals(enabledLaneArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneID id = new LaneID(12);
        LaneID id2 = new LaneID(5);

        String listSize = "1111";
        String remainingBits = id.encodeUPER();

        for (int i = 0; i < EnabledLaneList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += id2.encodeUPER();
        }

        EnabledLaneList enabledLanes = new EnabledLaneList();
        enabledLanes.decodeUPER(listSize + remainingBits);

        LaneID[] enabledLaneArray = enabledLanes.getEnabledLaneArray();
        assertTrue(EnabledLaneList.MAX_LIST_SIZE == enabledLaneArray.length);
        assertTrue(id.equals(enabledLaneArray[0]));
        for (int i = 1; i < EnabledLaneList.MAX_LIST_SIZE; i++) {

            assertTrue(id2.equals(enabledLaneArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        EnabledLaneList enabledLanes = new EnabledLaneList();
        thrown.expect(IllegalArgumentException.class);
        enabledLanes.decodeUPER("010");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        EnabledLaneList enabledLanes = new EnabledLaneList();
        thrown.expect(IllegalArgumentException.class);
        // 1011 = 12 objects, but there's none
        enabledLanes.decodeUPER("10111010100");
    }

    @Test
    public void testHashCode() {

        EnabledLaneList enabledLanes = new EnabledLaneList(1);
        enabledLanes.getEnabledLaneArray()[0] = new LaneID(150);

        assertTrue(enabledLanes.hashCode() == enabledLanes.hashCode());

        EnabledLaneList enabledLanes2 = new EnabledLaneList(2);

        assertFalse(enabledLanes.hashCode() == enabledLanes2.hashCode());

        enabledLanes2 = new EnabledLaneList(1);
        enabledLanes2.getEnabledLaneArray()[0] = new LaneID(10);

        assertFalse(enabledLanes.hashCode() == enabledLanes2.hashCode());

        enabledLanes2.getEnabledLaneArray()[0] = new LaneID(150);

        assertTrue(enabledLanes.hashCode() == enabledLanes2.hashCode());
    }

    @Test
    public void testEquals() {

        EnabledLaneList enabledLanes = new EnabledLaneList(1);
        enabledLanes.getEnabledLaneArray()[0] = new LaneID(150);

        assertTrue(enabledLanes.equals(enabledLanes));
        assertFalse(enabledLanes.equals(null));
        assertFalse(enabledLanes.equals(new String()));

        EnabledLaneList enabledLanes2 = new EnabledLaneList(2);

        assertFalse(enabledLanes.equals(enabledLanes2));

        enabledLanes2 = new EnabledLaneList(1);
        enabledLanes2.getEnabledLaneArray()[0] = new LaneID(10);

        assertFalse(enabledLanes.equals(enabledLanes2));

        enabledLanes2.getEnabledLaneArray()[0] = new LaneID(150);

        assertTrue(enabledLanes.equals(enabledLanes2));
    }
}
