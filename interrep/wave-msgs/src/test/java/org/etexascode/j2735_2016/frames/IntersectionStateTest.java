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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.DSecond;
import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the intersection state frame.
 * 
 * @author ttevendale
 */
public class IntersectionStateTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    IntersectionState intersectionState;

    @Before
    public void init() {

        IntersectionReferenceID id = new IntersectionReferenceID(100);
        id.setRegion(879);
        MsgCount revision = new MsgCount(111);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setManualControlEnabled(true);
        status.setFailureFlash(true);
        MinuteOfTheYear moy = new MinuteOfTheYear(5151);
        DSecond timeStamp = new DSecond(8787);

        EnabledLaneList enabledLanes = new EnabledLaneList(2);
        enabledLanes.getEnabledLaneArray()[0] = new LaneID(54);
        enabledLanes.getEnabledLaneArray()[1] = new LaneID(254);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(23);
        assist.setAvailableStorageLength(1000);

        maneuverAssistList.getManeuverAssistArray()[0] = assist;

        intersectionState = new IntersectionState(id, revision, status, states);
        intersectionState.setMoy(moy);
        intersectionState.setTimeStamp(timeStamp);
        intersectionState.setEnabledLanes(enabledLanes);
        intersectionState.setManeuverAssistList(maneuverAssistList);
    }

    @Test
    public void testConstructor() {

        IntersectionReferenceID id = new IntersectionReferenceID(100);
        MsgCount revision = new MsgCount(111);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setOff(true);
        status.setFailureFlash(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        IntersectionState intersectionState = new IntersectionState(id, revision, status, states);

        assertTrue(id.equals(intersectionState.getId()));
        assertTrue(revision.equals(intersectionState.getRevision()));
        assertTrue(status.equals(intersectionState.getStatus()));
        assertTrue(states.equals(intersectionState.getStates()));

    }

    @Test
    public void testConstructorNullId() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(null, new MsgCount(), new IntersectionStatusObject(), new MovementList());
    }

    @Test
    public void testConstructorNullRevision() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(new IntersectionReferenceID(), null, new IntersectionStatusObject(), new MovementList());
    }

    @Test
    public void testConstructorNullStatus() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(new IntersectionReferenceID(), new MsgCount(), null, new MovementList());
    }

    @Test
    public void testConstructorNullStates() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(new IntersectionReferenceID(), new MsgCount(), new IntersectionStatusObject(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        IntersectionReferenceID id = new IntersectionReferenceID(25);
        int revision = 12;
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setOff(true);
        status.setFailureFlash(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.DARK);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        IntersectionState intersectionState = new IntersectionState(id, revision, status, states);

        assertTrue(id.equals(intersectionState.getId()));
        assertTrue(revision == intersectionState.getRevision().getValue());
        assertTrue(status.equals(intersectionState.getStatus()));
        assertTrue(states.equals(intersectionState.getStates()));
    }

    @Test
    public void testConstructorPrimitiveNullId() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(null, 0, new IntersectionStatusObject(), new MovementList());
    }

    @Test
    public void testConstructorPrimitiveNullStatus() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(new IntersectionReferenceID(), 0, null, new MovementList());
    }

    @Test
    public void testConstructorPrimitiveNullStates() {

        thrown.expect(NullPointerException.class);
        new IntersectionState(new IntersectionReferenceID(), 0, new IntersectionStatusObject(), null);
    }

    @Test
    public void testSetId() {

        IntersectionReferenceID id = new IntersectionReferenceID(111);

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setId(id);

        assertTrue(id.equals(intersectionState.getId()));

        thrown.expect(NullPointerException.class);
        intersectionState.setId(null);
    }

    @Test
    public void testSetRevision() {

        MsgCount revision = new MsgCount(23);

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setRevision(revision);

        assertTrue(revision.equals(intersectionState.getRevision()));

        thrown.expect(NullPointerException.class);
        intersectionState.setRevision(null);
    }

    @Test
    public void testSetRevisionPrimitive() {

        int revision = 120;

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setRevision(revision);

        assertTrue(revision == intersectionState.getRevision().getValue());
    }

    @Test
    public void testSetStatus() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidMapAvailableAtThisTime(true);

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setStatus(status);

        assertTrue(status.equals(intersectionState.getStatus()));

        thrown.expect(NullPointerException.class);
        intersectionState.setStatus(null);
    }

    @Test
    public void testSetMoyPrimitive() {

        int moy = 814;

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setMoy(moy);

        assertTrue(moy == intersectionState.getMoy().getValue());

        moy = 112;

        intersectionState.setMoy(moy);

        assertTrue(moy == intersectionState.getMoy().getValue());
    }

    @Test
    public void testSetTimeStampPrimitive() {

        int timeStamp = 14;

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setTimeStamp(timeStamp);

        assertTrue(timeStamp == intersectionState.getTimeStamp().getValue());

        timeStamp = 11;

        intersectionState.setTimeStamp(timeStamp);

        assertTrue(timeStamp == intersectionState.getTimeStamp().getValue());
    }

    @Test
    public void testSetStates() {

        MovementList states = new MovementList(2);

        IntersectionState intersectionState = new IntersectionState();
        intersectionState.setStates(states);

        assertTrue(states.equals(intersectionState.getStates()));

        thrown.expect(NullPointerException.class);
        intersectionState.setStates(null);
    }

    @Test
    public void testEncodeUPERMin() {

        IntersectionReferenceID id = new IntersectionReferenceID(99);
        MsgCount revision = new MsgCount(87);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setManualControlEnabled(true);
        status.setFailureFlash(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);

        MovementState state = new MovementState(1, events);

        states.getMovementArray()[0] = state;

        IntersectionState intersectionState = new IntersectionState(id, revision, status, states);

        String intersectionStateOptionals = "0000000";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + status.encodeUPER() + states.encodeUPER();
        assertTrue((intersectionStateOptionals + remainingBits).equals(intersectionState.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        IntersectionReferenceID id = new IntersectionReferenceID(5);
        id.setRegion(2);
        MsgCount revision = new MsgCount(1);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidMapAvailableAtThisTime(true);
        status.setFailureFlash(true);
        MinuteOfTheYear moy = new MinuteOfTheYear(2132);
        DSecond timeStamp = new DSecond(60001);

        EnabledLaneList enabledLanes = new EnabledLaneList(2);
        enabledLanes.getEnabledLaneArray()[0] = new LaneID(1);
        enabledLanes.getEnabledLaneArray()[1] = new LaneID(3);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PRE_MOVEMENT);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(2);
        assist.setAvailableStorageLength(551);

        maneuverAssistList.getManeuverAssistArray()[0] = assist;

        IntersectionState intersectionState = new IntersectionState(id, revision, status, states);
        intersectionState.setMoy(moy);
        intersectionState.setTimeStamp(timeStamp);
        intersectionState.setEnabledLanes(enabledLanes);
        intersectionState.setManeuverAssistList(maneuverAssistList);

        String intersectionStateOptionals = "0011110";
        String remainingBits = id.encodeUPER() + revision.encodeUPER() + status.encodeUPER() + moy.encodeUPER() + timeStamp.encodeUPER() + enabledLanes.encodeUPER() + states.encodeUPER()
                + maneuverAssistList.encodeUPER();
        assertTrue((intersectionStateOptionals + remainingBits).equals(intersectionState.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        IntersectionReferenceID id = new IntersectionReferenceID(99);
        MsgCount revision = new MsgCount(87);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setManualControlEnabled(true);
        status.setFailureFlash(true);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.CAUTION_CONFLICTING_TRAFFIC);

        MovementState state = new MovementState(1, events);

        states.getMovementArray()[0] = state;

        String intersectionStateOptionals = "0000000";

        IntersectionState intersectionState = new IntersectionState();
        String remainingBits = intersectionState.decodeUPER(intersectionStateOptionals + id.encodeUPER() + revision.encodeUPER() + status.encodeUPER() + states.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(intersectionState.getMoy());
        assertNull(intersectionState.getTimeStamp());
        assertNull(intersectionState.getEnabledLanes());
        assertNull(intersectionState.getManeuverAssistList());
        assertTrue(id.equals(intersectionState.getId()));
        assertTrue(revision.equals(intersectionState.getRevision()));
        assertTrue(status.equals(intersectionState.getStatus()));
        assertTrue(states.equals(intersectionState.getStates()));
    }

    @Test
    public void testDecodeUPERMax() {

        IntersectionReferenceID id = new IntersectionReferenceID(5);
        id.setRegion(2);
        MsgCount revision = new MsgCount(1);
        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setNoValidMapAvailableAtThisTime(true);
        status.setFailureFlash(true);
        MinuteOfTheYear moy = new MinuteOfTheYear(2132);
        DSecond timeStamp = new DSecond(60001);

        EnabledLaneList enabledLanes = new EnabledLaneList(2);
        enabledLanes.getEnabledLaneArray()[0] = new LaneID(1);
        enabledLanes.getEnabledLaneArray()[1] = new LaneID(3);

        MovementList states = new MovementList(1);

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = new MovementEvent(MovementPhase.PRE_MOVEMENT);

        MovementState state = new MovementState(21, events);

        states.getMovementArray()[0] = state;

        ManeuverAssistList maneuverAssistList = new ManeuverAssistList(1);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(2);
        assist.setAvailableStorageLength(551);

        maneuverAssistList.getManeuverAssistArray()[0] = assist;

        String intersectionStateOptionals = "0011110";

        IntersectionState intersectionState = new IntersectionState();
        String remainingBits = intersectionState.decodeUPER(intersectionStateOptionals + id.encodeUPER() + revision.encodeUPER() + status.encodeUPER() + moy.encodeUPER() + timeStamp.encodeUPER()
                + enabledLanes.encodeUPER() + states.encodeUPER() + maneuverAssistList.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(id.equals(intersectionState.getId()));
        assertTrue(revision.equals(intersectionState.getRevision()));
        assertTrue(status.equals(intersectionState.getStatus()));
        assertTrue(moy.equals(intersectionState.getMoy()));
        assertTrue(timeStamp.equals(intersectionState.getTimeStamp()));
        assertTrue(enabledLanes.equals(intersectionState.getEnabledLanes()));
        assertTrue(states.equals(intersectionState.getStates()));
        assertTrue(maneuverAssistList.equals(intersectionState.getManeuverAssistList()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String intersectionStateOptionals = "1000000";

        IntersectionState intersectionState = new IntersectionState();
        thrown.expect(IllegalArgumentException.class);
        intersectionState.decodeUPER(intersectionStateOptionals);
    }

    @Test
    public void testDecodeUPERDescriptiveName() {

        String intersectionStateOptionals = "0100000";

        IntersectionState intersectionState = new IntersectionState();
        thrown.expect(IllegalArgumentException.class);
        intersectionState.decodeUPER(intersectionStateOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String intersectionStateOptionals = "0000001";

        IntersectionState intersectionState = new IntersectionState();
        thrown.expect(IllegalArgumentException.class);
        intersectionState.decodeUPER(intersectionStateOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String intersectionStateOptionals = "";

        IntersectionState intersectionState = new IntersectionState();
        thrown.expect(IllegalArgumentException.class);
        intersectionState.decodeUPER(intersectionStateOptionals);
    }

    @Test
    public void testHashCode() {

        IntersectionReferenceID id = intersectionState.getId();
        int revision = intersectionState.getRevision().getValue();
        IntersectionStatusObject status = intersectionState.getStatus();
        int moy = intersectionState.getMoy().getValue();
        int timeStamp = intersectionState.getTimeStamp().getValue();
        EnabledLaneList enabledLanes = intersectionState.getEnabledLanes();
        MovementList states = intersectionState.getStates();
        ManeuverAssistList maneuverAssistList = intersectionState.getManeuverAssistList();

        IntersectionReferenceID diffId = new IntersectionReferenceID(id.getId().getValue() + 1);
        IntersectionStatusObject diffStatus = new IntersectionStatusObject();
        diffStatus.setFailureFlash(!status.isFailureFlash());
        EnabledLaneList diffEnabledLanes = new EnabledLaneList(enabledLanes.getEnabledLaneArray().length + 1);
        MovementList diffStates = new MovementList(states.getMovementArray().length + 1);
        ManeuverAssistList diffManeuverAssistList = new ManeuverAssistList(maneuverAssistList.getManeuverAssistArray().length + 1);

        IntersectionState intersectionState2 = new IntersectionState(diffId, revision + 1, diffStatus, diffStates);
        intersectionState2.setMoy(moy + 1);
        intersectionState2.setTimeStamp(timeStamp + 1);
        intersectionState2.setEnabledLanes(diffEnabledLanes);
        intersectionState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(intersectionState.hashCode() == intersectionState2.hashCode());
        assertTrue(intersectionState.hashCode() == intersectionState.hashCode());
        assertTrue(intersectionState2.hashCode() == intersectionState2.hashCode());

        IntersectionState intersectionState3 = new IntersectionState(id, revision, status, states);
        intersectionState3.setMoy(moy);
        intersectionState3.setTimeStamp(timeStamp);
        intersectionState3.setEnabledLanes(enabledLanes);
        intersectionState3.setManeuverAssistList(maneuverAssistList);

        assertTrue(intersectionState.hashCode() == intersectionState3.hashCode());
        assertFalse(intersectionState2.hashCode() == intersectionState3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(intersectionState.equals(intersectionState));
        assertFalse(intersectionState.equals(null));
        assertFalse(intersectionState.equals(new String()));

        IntersectionReferenceID id = intersectionState.getId();
        int revision = intersectionState.getRevision().getValue();
        IntersectionStatusObject status = intersectionState.getStatus();
        int moy = intersectionState.getMoy().getValue();
        int timeStamp = intersectionState.getTimeStamp().getValue();
        EnabledLaneList enabledLanes = intersectionState.getEnabledLanes();
        MovementList states = intersectionState.getStates();
        ManeuverAssistList maneuverAssistList = intersectionState.getManeuverAssistList();

        IntersectionReferenceID diffId = new IntersectionReferenceID(id.getId().getValue() + 1);
        IntersectionStatusObject diffStatus = new IntersectionStatusObject();
        diffStatus.setFailureFlash(!status.isFailureFlash());
        EnabledLaneList diffEnabledLanes = new EnabledLaneList(enabledLanes.getEnabledLaneArray().length + 1);
        MovementList diffStates = new MovementList(states.getMovementArray().length + 1);
        ManeuverAssistList diffManeuverAssistList = new ManeuverAssistList(maneuverAssistList.getManeuverAssistArray().length + 1);

        // different
        IntersectionState intersectionState2 = new IntersectionState(diffId, revision + 1, diffStatus, diffStates);
        intersectionState2.setMoy(moy + 1);
        intersectionState2.setTimeStamp(timeStamp + 1);
        intersectionState2.setEnabledLanes(diffEnabledLanes);
        intersectionState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different id
        intersectionState2 = new IntersectionState(diffId, revision, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different revision
        intersectionState2 = new IntersectionState(id, revision + 1, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different status
        intersectionState2 = new IntersectionState(id, revision, diffStatus, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different moy
        intersectionState2 = new IntersectionState(id, revision, status, states);
        intersectionState2.setMoy(moy + 1);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different time stamp
        intersectionState2 = new IntersectionState(id, revision, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp + 1);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different enabled lanes
        intersectionState2 = new IntersectionState(id, revision, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(diffEnabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different states
        intersectionState2 = new IntersectionState(id, revision, status, diffStates);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // different maneuver assist list
        intersectionState2 = new IntersectionState(id, revision, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(diffManeuverAssistList);

        assertFalse(intersectionState.equals(intersectionState2));

        // same
        intersectionState2 = new IntersectionState(id, revision, status, states);
        intersectionState2.setMoy(moy);
        intersectionState2.setTimeStamp(timeStamp);
        intersectionState2.setEnabledLanes(enabledLanes);
        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertTrue(intersectionState.equals(intersectionState2));
    }

    @Test
    public void testEqualsNull() {

        IntersectionReferenceID id = intersectionState.getId();
        int revision = intersectionState.getRevision().getValue();
        IntersectionStatusObject status = intersectionState.getStatus();
        int moy = intersectionState.getMoy().getValue();
        int timeStamp = intersectionState.getTimeStamp().getValue();
        EnabledLaneList enabledLanes = intersectionState.getEnabledLanes();
        MovementList states = intersectionState.getStates();
        ManeuverAssistList maneuverAssistList = intersectionState.getManeuverAssistList();

        IntersectionState intersectionState2 = new IntersectionState(id, revision, status, states);

        assertFalse(intersectionState.equals(intersectionState2));

        intersectionState2.setMoy(moy);

        assertFalse(intersectionState.equals(intersectionState2));

        intersectionState2.setTimeStamp(timeStamp);

        assertFalse(intersectionState.equals(intersectionState2));

        intersectionState2.setEnabledLanes(enabledLanes);

        assertFalse(intersectionState.equals(intersectionState2));

        intersectionState2.setManeuverAssistList(maneuverAssistList);

        assertTrue(intersectionState.equals(intersectionState2));
    }
}
