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

import org.etexascode.j2735_2016.elements.LaneConnectionID;
import org.etexascode.j2735_2016.elements.PedestrianBicycleDetect;
import org.etexascode.j2735_2016.elements.WaitOnStopLine;
import org.etexascode.j2735_2016.elements.ZoneLength;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the connection maneuver assist frame.
 * 
 * @author ttevendale
 */
public class ConnectionManeuverAssistTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    ConnectionManeuverAssist assist;

    @Before
    public void init() {

        LaneConnectionID connectionId = new LaneConnectionID(38);
        ZoneLength queueLength = new ZoneLength(5484);
        ZoneLength availableStorageLength = new ZoneLength(5);
        WaitOnStopLine waitOnStop = new WaitOnStopLine(true);
        PedestrianBicycleDetect pedBicycleDetect = new PedestrianBicycleDetect(false);

        assist = new ConnectionManeuverAssist(connectionId);
        assist.setQueueLength(queueLength);
        assist.setAvailableStorageLength(availableStorageLength);
        assist.setWaitOnStop(waitOnStop);
        assist.setPedBicycleDetect(pedBicycleDetect);
    }

    @Test
    public void testConstructor() {

        LaneConnectionID connectionId = new LaneConnectionID(15);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(connectionId);

        assertNull(assist.getQueueLength());
        assertNull(assist.getAvailableStorageLength());
        assertNull(assist.getWaitOnStop());
        assertNull(assist.getPedBicycleDetect());
        assertTrue(connectionId.equals(assist.getConnectionId()));

        thrown.expect(NullPointerException.class);
        assist = new ConnectionManeuverAssist(null);
    }

    @Test
    public void testConstructorPrimitive() {

        int connectionId = 151;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(connectionId);

        assertTrue(connectionId == assist.getConnectionId().getValue());
    }

    @Test
    public void testSetConnectionId() {

        LaneConnectionID connectionId = new LaneConnectionID(124);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setConnectionId(connectionId);

        assertTrue(connectionId.equals(assist.getConnectionId()));

        thrown.expect(NullPointerException.class);
        assist.setConnectionId(null);
    }

    @Test
    public void testSetConnectionIdPrimitive() {

        int connectionId = 120;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setConnectionId(connectionId);

        assertTrue(connectionId == assist.getConnectionId().getValue());
    }

    @Test
    public void testSetQueueLengthPrimitive() {

        int queueLength = 879;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setQueueLength(queueLength);

        assertTrue(queueLength == assist.getQueueLength().getValue());

        queueLength = 182;

        assist.setQueueLength(queueLength);

        assertTrue(queueLength == assist.getQueueLength().getValue());
    }

    @Test
    public void testSetAvailableStorageLengthPrimitive() {

        int availableStorageLength = 8484;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setAvailableStorageLength(availableStorageLength);

        assertTrue(availableStorageLength == assist.getAvailableStorageLength().getValue());

        availableStorageLength = 9;

        assist.setAvailableStorageLength(availableStorageLength);

        assertTrue(availableStorageLength == assist.getAvailableStorageLength().getValue());
    }

    @Test
    public void testSetWaitOnStopPrimitive() {

        boolean waitOnStop = true;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setWaitOnStop(waitOnStop);

        assertTrue(waitOnStop == assist.getWaitOnStop().getValue());

        waitOnStop = false;

        assist.setWaitOnStop(waitOnStop);

        assertTrue(waitOnStop == assist.getWaitOnStop().getValue());
    }

    @Test
    public void testSetPedBicycleDetectPrimitive() {

        boolean pedBicycleDetect = true;

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        assist.setPedBicycleDetect(pedBicycleDetect);

        assertTrue(pedBicycleDetect == assist.getPedBicycleDetect().getValue());

        pedBicycleDetect = false;

        assist.setPedBicycleDetect(pedBicycleDetect);

        assertTrue(pedBicycleDetect == assist.getPedBicycleDetect().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        LaneConnectionID connectionId = new LaneConnectionID(1);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(connectionId);

        String assistOptionals = "000000";
        String remainingBits = connectionId.encodeUPER();
        assertTrue((assistOptionals + remainingBits).equals(assist.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        LaneConnectionID connectionId = new LaneConnectionID(2);
        ZoneLength queueLength = new ZoneLength(124);
        ZoneLength availableStorageLength = new ZoneLength(5321);
        WaitOnStopLine waitOnStop = new WaitOnStopLine(true);
        PedestrianBicycleDetect pedBicycleDetect = new PedestrianBicycleDetect(false);

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(connectionId);
        assist.setQueueLength(queueLength);
        assist.setAvailableStorageLength(availableStorageLength);
        assist.setWaitOnStop(waitOnStop);
        assist.setPedBicycleDetect(pedBicycleDetect);

        String assistOptionals = "011110";
        String remainingBits = connectionId.encodeUPER() + queueLength.encodeUPER() + availableStorageLength.encodeUPER() + waitOnStop.encodeUPER() + pedBicycleDetect.encodeUPER();
        assertTrue((assistOptionals + remainingBits).equals(assist.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        LaneConnectionID connectionId = new LaneConnectionID(3);

        String assistOptionals = "000000";

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        String remainingBits = assist.decodeUPER(assistOptionals + connectionId.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(assist.getQueueLength());
        assertNull(assist.getAvailableStorageLength());
        assertNull(assist.getWaitOnStop());
        assertNull(assist.getPedBicycleDetect());
        assertTrue(connectionId.equals(assist.getConnectionId()));
    }

    @Test
    public void testDecodeUPERMax() {

        LaneConnectionID connectionId = new LaneConnectionID(4);
        ZoneLength queueLength = new ZoneLength(2);
        ZoneLength availableStorageLength = new ZoneLength(1234);
        WaitOnStopLine waitOnStop = new WaitOnStopLine(false);
        PedestrianBicycleDetect pedBicycleDetect = new PedestrianBicycleDetect(true);

        String assistOptionals = "011110";

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        String remainingBits = assist
                .decodeUPER(assistOptionals + connectionId.encodeUPER() + queueLength.encodeUPER() + availableStorageLength.encodeUPER() + waitOnStop.encodeUPER() + pedBicycleDetect.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(connectionId.equals(assist.getConnectionId()));
        assertTrue(queueLength.equals(assist.getQueueLength()));
        assertTrue(availableStorageLength.equals(assist.getAvailableStorageLength()));
        assertTrue(waitOnStop.equals(assist.getWaitOnStop()));
        assertTrue(pedBicycleDetect.equals(assist.getPedBicycleDetect()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String assistOptionals = "100000";

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        thrown.expect(IllegalArgumentException.class);
        assist.decodeUPER(assistOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String assistOptionals = "001111";

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        thrown.expect(IllegalArgumentException.class);
        assist.decodeUPER(assistOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String assistOptionals = "01110";

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist();
        thrown.expect(IllegalArgumentException.class);
        assist.decodeUPER(assistOptionals);
    }

    @Test
    public void testHashCode() {

        int connectionId = assist.getConnectionId().getValue();
        int queueLength = assist.getQueueLength().getValue();
        int availableStorageLength = assist.getAvailableStorageLength().getValue();
        boolean waitOnStop = assist.getWaitOnStop().getValue();
        boolean pedBicycleDetect = assist.getPedBicycleDetect().getValue();

        ConnectionManeuverAssist assist2 = new ConnectionManeuverAssist(connectionId + 1);
        assist2.setQueueLength(queueLength + 1);
        assist2.setAvailableStorageLength(availableStorageLength + 1);
        assist2.setWaitOnStop(!waitOnStop);
        assist2.setPedBicycleDetect(!pedBicycleDetect);

        assertFalse(assist.hashCode() == assist2.hashCode());
        assertTrue(assist.hashCode() == assist.hashCode());
        assertTrue(assist2.hashCode() == assist2.hashCode());

        ConnectionManeuverAssist assist3 = new ConnectionManeuverAssist(connectionId);
        assist3.setQueueLength(queueLength);
        assist3.setAvailableStorageLength(availableStorageLength);
        assist3.setWaitOnStop(waitOnStop);
        assist3.setPedBicycleDetect(pedBicycleDetect);

        assertTrue(assist.hashCode() == assist3.hashCode());
        assertFalse(assist2.hashCode() == assist3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(assist.equals(assist));
        assertFalse(assist.equals(null));
        assertFalse(assist.equals(new String()));

        int connectionId = assist.getConnectionId().getValue();
        int queueLength = assist.getQueueLength().getValue();
        int availableStorageLength = assist.getAvailableStorageLength().getValue();
        boolean waitOnStop = assist.getWaitOnStop().getValue();
        boolean pedBicycleDetect = assist.getPedBicycleDetect().getValue();

        // different
        ConnectionManeuverAssist assist2 = new ConnectionManeuverAssist(connectionId + 1);
        assist2.setQueueLength(queueLength + 1);
        assist2.setAvailableStorageLength(availableStorageLength + 1);
        assist2.setWaitOnStop(!waitOnStop);
        assist2.setPedBicycleDetect(!pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // different connection ID
        assist2 = new ConnectionManeuverAssist(connectionId + 1);
        assist2.setQueueLength(queueLength);
        assist2.setAvailableStorageLength(availableStorageLength);
        assist2.setWaitOnStop(waitOnStop);
        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // different queue length
        assist2 = new ConnectionManeuverAssist(connectionId);
        assist2.setQueueLength(queueLength + 1);
        assist2.setAvailableStorageLength(availableStorageLength);
        assist2.setWaitOnStop(waitOnStop);
        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // different available storage length
        assist2 = new ConnectionManeuverAssist(connectionId);
        assist2.setQueueLength(queueLength);
        assist2.setAvailableStorageLength(availableStorageLength + 1);
        assist2.setWaitOnStop(waitOnStop);
        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // different wait on stop
        assist2 = new ConnectionManeuverAssist(connectionId);
        assist2.setQueueLength(queueLength);
        assist2.setAvailableStorageLength(availableStorageLength);
        assist2.setWaitOnStop(!waitOnStop);
        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // different ped bicycle detect
        assist2 = new ConnectionManeuverAssist(connectionId);
        assist2.setQueueLength(queueLength);
        assist2.setAvailableStorageLength(availableStorageLength);
        assist2.setWaitOnStop(waitOnStop);
        assist2.setPedBicycleDetect(!pedBicycleDetect);

        assertFalse(assist.equals(assist2));

        // same
        assist2 = new ConnectionManeuverAssist(connectionId);
        assist2.setQueueLength(queueLength);
        assist2.setAvailableStorageLength(availableStorageLength);
        assist2.setWaitOnStop(waitOnStop);
        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertTrue(assist.equals(assist2));
    }

    @Test
    public void testEqualsNull() {

        int connectionId = assist.getConnectionId().getValue();
        int queueLength = assist.getQueueLength().getValue();
        int availableStorageLength = assist.getAvailableStorageLength().getValue();
        boolean waitOnStop = assist.getWaitOnStop().getValue();
        boolean pedBicycleDetect = assist.getPedBicycleDetect().getValue();

        ConnectionManeuverAssist assist2 = new ConnectionManeuverAssist(connectionId);

        assertFalse(assist.equals(assist2));

        assist2.setQueueLength(queueLength);

        assertFalse(assist.equals(assist2));

        assist2.setAvailableStorageLength(availableStorageLength);

        assertFalse(assist.equals(assist2));

        assist2.setWaitOnStop(waitOnStop);

        assertFalse(assist.equals(assist2));

        assist2.setPedBicycleDetect(pedBicycleDetect);

        assertTrue(assist.equals(assist2));
    }
}
