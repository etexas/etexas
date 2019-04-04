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
 * Unit tests for the intersection status object element.
 * 
 * @author ttevendale
 */
public class IntersectionStatusObjectTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testEncodeUPER() {

        IntersectionStatusObject status = new IntersectionStatusObject();

        // min
        assertTrue("0000000000000000".equals(status.encodeUPER()));

        status.setFailureFlash(true);
        status.setFixedTimeOperation(true);

        // some turned on
        assertTrue("0010010000000000".equals(status.encodeUPER()));

        status.setManualControlEnabled(true);
        status.setStopTimeActivated(true);
        status.setPreemptActive(true);
        status.setSignalPriorityActive(true);
        status.setTrafficDependentOperation(true);
        status.setStandbyOperation(true);
        status.setFailureMode(true);
        status.setOff(true);
        status.setRecentMapMessageUpdate(true);
        status.setRecentChangeInMapAssignedLaneIdsUsed(true);
        status.setNoValidMapAvailableAtThisTime(true);
        status.setNoValidSpatAvailableAtThisTime(true);

        // max
        assertTrue("1111111111111100".equals(status.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        IntersectionStatusObject status = new IntersectionStatusObject();

        // min
        String remainingBits = status.decodeUPER("0000000000000000");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isManualControlEnabled());
        assertFalse(status.isStopTimeActivated());
        assertFalse(status.isFailureFlash());
        assertFalse(status.isPreemptActive());
        assertFalse(status.isSignalPriorityActive());
        assertFalse(status.isFixedTimeOperation());
        assertFalse(status.isTrafficDependentOperation());
        assertFalse(status.isStandbyOperation());
        assertFalse(status.isFailureMode());
        assertFalse(status.isOff());
        assertFalse(status.isRecentMapMessageUpdate());
        assertFalse(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertFalse(status.isNoValidMapAvailableAtThisTime());
        assertFalse(status.isNoValidSpatAvailableAtThisTime());

        status.setPreemptActive(true);
        status.setOff(true);
        status.setRecentChangeInMapAssignedLaneIdsUsed(true);
        status.setNoValidSpatAvailableAtThisTime(true);

        // some turned on
        remainingBits = status.decodeUPER("0001000001010100");
        assertTrue("".equals(remainingBits));
        assertFalse(status.isManualControlEnabled());
        assertFalse(status.isStopTimeActivated());
        assertFalse(status.isFailureFlash());
        assertTrue(status.isPreemptActive());
        assertFalse(status.isSignalPriorityActive());
        assertFalse(status.isFixedTimeOperation());
        assertFalse(status.isTrafficDependentOperation());
        assertFalse(status.isStandbyOperation());
        assertFalse(status.isFailureMode());
        assertTrue(status.isOff());
        assertFalse(status.isRecentMapMessageUpdate());
        assertTrue(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertFalse(status.isNoValidMapAvailableAtThisTime());
        assertTrue(status.isNoValidSpatAvailableAtThisTime());

        status.setManualControlEnabled(true);
        status.setStopTimeActivated(true);
        status.setFailureFlash(true);
        status.setSignalPriorityActive(true);
        status.setFixedTimeOperation(true);
        status.setTrafficDependentOperation(true);
        status.setStandbyOperation(true);
        status.setFailureMode(true);
        status.setRecentMapMessageUpdate(true);
        status.setNoValidMapAvailableAtThisTime(true);

        // max
        remainingBits = status.decodeUPER("1111111111111100");
        assertTrue("".equals(remainingBits));
        assertTrue(status.isManualControlEnabled());
        assertTrue(status.isStopTimeActivated());
        assertTrue(status.isFailureFlash());
        assertTrue(status.isPreemptActive());
        assertTrue(status.isSignalPriorityActive());
        assertTrue(status.isFixedTimeOperation());
        assertTrue(status.isTrafficDependentOperation());
        assertTrue(status.isStandbyOperation());
        assertTrue(status.isFailureMode());
        assertTrue(status.isOff());
        assertTrue(status.isRecentMapMessageUpdate());
        assertTrue(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertTrue(status.isNoValidMapAvailableAtThisTime());
        assertTrue(status.isNoValidSpatAvailableAtThisTime());
    }

    @Test
    public void testDecodeUPERReservedBit14() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("0000000000000010");
    }

    @Test
    public void testDecodeUPERReservedBit15() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("0000000000000001");
    }

    @Test
    public void testDecodeUPERLessBits() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        thrown.expect(IllegalArgumentException.class);
        status.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        String remainingBits = status.decodeUPER("0101000100010100110");
        assertTrue("110".equals(remainingBits));
        assertFalse(status.isManualControlEnabled());
        assertTrue(status.isStopTimeActivated());
        assertFalse(status.isFailureFlash());
        assertTrue(status.isPreemptActive());
        assertFalse(status.isSignalPriorityActive());
        assertFalse(status.isFixedTimeOperation());
        assertFalse(status.isTrafficDependentOperation());
        assertTrue(status.isStandbyOperation());
        assertFalse(status.isFailureMode());
        assertFalse(status.isOff());
        assertFalse(status.isRecentMapMessageUpdate());
        assertTrue(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertFalse(status.isNoValidMapAvailableAtThisTime());
        assertTrue(status.isNoValidSpatAvailableAtThisTime());
    }

    @Test
    public void testHashCode() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidMapAvailableAtThisTime(true);

        IntersectionStatusObject status2 = new IntersectionStatusObject();
        status2.setNoValidMapAvailableAtThisTime(false);

        assertFalse(status.hashCode() == status2.hashCode());
        assertTrue(status.hashCode() == status.hashCode());
        assertTrue(status2.hashCode() == status2.hashCode());

        IntersectionStatusObject status3 = new IntersectionStatusObject();
        status3.setManualControlEnabled(status.isManualControlEnabled());
        status3.setStopTimeActivated(status.isStopTimeActivated());
        status3.setFailureFlash(status.isFailureFlash());
        status3.setPreemptActive(status.isPreemptActive());
        status3.setSignalPriorityActive(status.isSignalPriorityActive());
        status3.setFixedTimeOperation(status.isFixedTimeOperation());
        status3.setTrafficDependentOperation(status.isTrafficDependentOperation());
        status3.setStandbyOperation(status.isStandbyOperation());
        status3.setFailureMode(status.isFailureMode());
        status3.setOff(status.isOff());
        status3.setRecentMapMessageUpdate(status.isRecentMapMessageUpdate());
        status3.setRecentChangeInMapAssignedLaneIdsUsed(status.isRecentChangeInMapAssignedLaneIdsUsed());
        status3.setNoValidMapAvailableAtThisTime(status.isNoValidMapAvailableAtThisTime());
        status3.setNoValidSpatAvailableAtThisTime(status.isNoValidSpatAvailableAtThisTime());

        assertTrue(status.hashCode() == status3.hashCode());
        assertFalse(status2.hashCode() == status3.hashCode());
    }

    @Test
    public void testEquals() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setFailureFlash(true);
        status.setFixedTimeOperation(true);
        status.setManualControlEnabled(true);
        status.setStopTimeActivated(true);
        status.setPreemptActive(true);
        status.setSignalPriorityActive(true);
        status.setTrafficDependentOperation(true);
        status.setStandbyOperation(true);
        status.setFailureMode(true);
        status.setOff(true);
        status.setRecentMapMessageUpdate(true);
        status.setRecentChangeInMapAssignedLaneIdsUsed(true);
        status.setNoValidMapAvailableAtThisTime(true);
        status.setNoValidSpatAvailableAtThisTime(true);

        assertFalse(status.equals(null));

        assertTrue(status.equals(status));

        IntersectionStatusObject status2 = new IntersectionStatusObject();
        status2.setFailureFlash(false);
        status2.setFixedTimeOperation(false);
        status2.setManualControlEnabled(false);
        status2.setStopTimeActivated(false);
        status2.setPreemptActive(false);
        status2.setSignalPriorityActive(false);
        status2.setTrafficDependentOperation(false);
        status2.setStandbyOperation(false);
        status2.setFailureMode(false);
        status2.setOff(false);
        status2.setRecentMapMessageUpdate(false);
        status2.setRecentChangeInMapAssignedLaneIdsUsed(false);
        status2.setNoValidMapAvailableAtThisTime(false);
        status2.setNoValidSpatAvailableAtThisTime(false);

        assertFalse(status.equals(new String()));
        assertFalse(status.equals(status2));

        status2.setManualControlEnabled(status.isManualControlEnabled());
        assertFalse(status.equals(status2));

        status2.setStopTimeActivated(status.isStopTimeActivated());
        assertFalse(status.equals(status2));

        status2.setFailureFlash(status.isFailureFlash());
        assertFalse(status.equals(status2));

        status2.setPreemptActive(status.isPreemptActive());
        assertFalse(status.equals(status2));

        status2.setSignalPriorityActive(status.isSignalPriorityActive());
        assertFalse(status.equals(status2));

        status2.setFixedTimeOperation(status.isFixedTimeOperation());
        assertFalse(status.equals(status2));

        status2.setTrafficDependentOperation(status.isTrafficDependentOperation());
        assertFalse(status.equals(status2));

        status2.setStandbyOperation(status.isStandbyOperation());
        assertFalse(status.equals(status2));

        status2.setFailureMode(status.isFailureMode());
        assertFalse(status.equals(status2));

        status2.setOff(status.isOff());
        assertFalse(status.equals(status2));

        status2.setRecentMapMessageUpdate(status.isRecentMapMessageUpdate());
        assertFalse(status.equals(status2));

        status2.setRecentChangeInMapAssignedLaneIdsUsed(status.isRecentChangeInMapAssignedLaneIdsUsed());
        assertFalse(status.equals(status2));

        status2.setNoValidMapAvailableAtThisTime(status.isNoValidMapAvailableAtThisTime());
        assertFalse(status.equals(status2));

        status2.setNoValidSpatAvailableAtThisTime(status.isNoValidSpatAvailableAtThisTime());
        assertTrue(status.equals(status2));
    }
}
