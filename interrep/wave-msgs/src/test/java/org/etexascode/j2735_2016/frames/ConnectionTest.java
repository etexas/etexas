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
import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.etexascode.j2735_2016.elements.SignalGroupID;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the connection frame.
 * 
 * @author ttevendale
 */
public class ConnectionTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    Connection connection;

    @Before
    public void init() {

        ConnectingLane connectingLane = new ConnectingLane(5);
        IntersectionReferenceID remoteIntersection = new IntersectionReferenceID(10);
        SignalGroupID signalGroup = new SignalGroupID(111);
        RestrictionClassID userClass = new RestrictionClassID(54);
        LaneConnectionID connectionId = new LaneConnectionID(1);

        connection = new Connection(connectingLane);
        connection.setRemoteIntersection(remoteIntersection);
        connection.setSignalGroup(signalGroup);
        connection.setUserClass(userClass);
        connection.setConnectionId(connectionId);
    }

    @Test
    public void testConstructor() {

        ConnectingLane connectingLane = new ConnectingLane(51);

        Connection connection = new Connection(connectingLane);

        assertNull(connection.getRemoteIntersection());
        assertNull(connection.getSignalGroup());
        assertNull(connection.getUserClass());
        assertNull(connection.getConnectionId());
        assertTrue(connectingLane.equals(connection.getConnectingLane()));

        thrown.expect(NullPointerException.class);
        new Connection(null);
    }

    @Test
    public void testSetConnectingLane() {

        ConnectingLane connectingLane = new ConnectingLane(15);

        Connection connection = new Connection();
        connection.setConnectingLane(connectingLane);

        assertTrue(connectingLane.equals(connection.getConnectingLane()));

        thrown.expect(NullPointerException.class);
        connection.setConnectingLane(null);
    }

    @Test
    public void testSetSignalGroupPrimitive() {

        int signalGroup = 111;

        Connection connection = new Connection();
        connection.setSignalGroup(signalGroup);

        assertTrue(signalGroup == connection.getSignalGroup().getValue());

        signalGroup = 123;

        connection.setSignalGroup(signalGroup);

        assertTrue(signalGroup == connection.getSignalGroup().getValue());
    }

    @Test
    public void testSetUserClassPrimitive() {

        int userClass = 1;

        Connection connection = new Connection();
        connection.setUserClass(userClass);

        assertTrue(userClass == connection.getUserClass().getValue());

        userClass = 2;

        connection.setUserClass(userClass);

        assertTrue(userClass == connection.getUserClass().getValue());
    }

    @Test
    public void testSetConnectionIdPrimitive() {

        int connectionId = 3;

        Connection connection = new Connection();
        connection.setConnectionId(connectionId);

        assertTrue(connectionId == connection.getConnectionId().getValue());

        connectionId = 4;

        connection.setConnectionId(connectionId);

        assertTrue(connectionId == connection.getConnectionId().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        ConnectingLane connectingLane = new ConnectingLane(5);

        Connection connection = new Connection(connectingLane);

        String connectionOptionals = "0000";
        String remainingBits = connectingLane.encodeUPER();
        assertTrue((connectionOptionals + remainingBits).equals(connection.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        ConnectingLane connectingLane = new ConnectingLane(10);
        IntersectionReferenceID remoteIntersection = new IntersectionReferenceID(20);
        SignalGroupID signalGroup = new SignalGroupID(222);
        RestrictionClassID userClass = new RestrictionClassID(108);
        LaneConnectionID connectionId = new LaneConnectionID(2);

        Connection connection = new Connection(connectingLane);
        connection.setRemoteIntersection(remoteIntersection);
        connection.setSignalGroup(signalGroup);
        connection.setUserClass(userClass);
        connection.setConnectionId(connectionId);

        String connectionOptionals = "1111";
        String remainingBits = connectingLane.encodeUPER() + remoteIntersection.encodeUPER() + signalGroup.encodeUPER() + userClass.encodeUPER() + connectionId.encodeUPER();
        assertTrue((connectionOptionals + remainingBits).equals(connection.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        ConnectingLane connectingLane = new ConnectingLane(20);

        String connectionOptionals = "0000";

        Connection connection = new Connection();
        String remainingBits = connection.decodeUPER(connectionOptionals + connectingLane.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(connection.getRemoteIntersection());
        assertNull(connection.getSignalGroup());
        assertNull(connection.getUserClass());
        assertNull(connection.getConnectionId());
        assertTrue(connectingLane.equals(connection.getConnectingLane()));
    }

    @Test
    public void testDecodeUPERMax() {

        ConnectingLane connectingLane = new ConnectingLane(10);
        IntersectionReferenceID remoteIntersection = new IntersectionReferenceID(20);
        SignalGroupID signalGroup = new SignalGroupID(222);
        RestrictionClassID userClass = new RestrictionClassID(108);
        LaneConnectionID connectionId = new LaneConnectionID(2);

        String connectionOptionals = "1111";

        Connection connection = new Connection();
        String remainingBits = connection
                .decodeUPER(connectionOptionals + connectingLane.encodeUPER() + remoteIntersection.encodeUPER() + signalGroup.encodeUPER() + userClass.encodeUPER() + connectionId.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(connectingLane.equals(connection.getConnectingLane()));
        assertTrue(remoteIntersection.equals(connection.getRemoteIntersection()));
        assertTrue(signalGroup.equals(connection.getSignalGroup()));
        assertTrue(userClass.equals(connection.getUserClass()));
        assertTrue(connectionId.equals(connection.getConnectionId()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        String connectionOptionals = "101";

        Connection connection = new Connection();
        thrown.expect(IllegalArgumentException.class);
        connection.decodeUPER(connectionOptionals);
    }

    @Test
    public void testHashCode() {

        ConnectingLane connectingLane = connection.getConnectingLane();
        IntersectionReferenceID remoteIntersection = connection.getRemoteIntersection();
        int signalGroup = connection.getSignalGroup().getValue();
        int userClass = connection.getUserClass().getValue();
        int connectionId = connection.getConnectionId().getValue();

        ConnectingLane diffConnectingLane = new ConnectingLane(connectingLane.getLane().getValue() + 1);
        IntersectionReferenceID diffRemoteIntersection = new IntersectionReferenceID(remoteIntersection.getId().getValue() + 1);

        Connection connection2 = new Connection(diffConnectingLane);
        connection2.setRemoteIntersection(diffRemoteIntersection);
        connection2.setSignalGroup(signalGroup + 1);
        connection2.setUserClass(userClass + 1);
        connection2.setConnectionId(connectionId + 1);

        assertFalse(connection.hashCode() == connection2.hashCode());
        assertTrue(connection.hashCode() == connection.hashCode());
        assertTrue(connection2.hashCode() == connection2.hashCode());

        Connection connection3 = new Connection(connectingLane);
        connection3.setRemoteIntersection(remoteIntersection);
        connection3.setSignalGroup(signalGroup);
        connection3.setUserClass(userClass);
        connection3.setConnectionId(connectionId);

        assertTrue(connection.hashCode() == connection3.hashCode());
        assertFalse(connection2.hashCode() == connection3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(connection.equals(connection));
        assertFalse(connection.equals(null));
        assertFalse(connection.equals(new String()));

        ConnectingLane connectingLane = connection.getConnectingLane();
        IntersectionReferenceID remoteIntersection = connection.getRemoteIntersection();
        int signalGroup = connection.getSignalGroup().getValue();
        int userClass = connection.getUserClass().getValue();
        int connectionId = connection.getConnectionId().getValue();

        ConnectingLane diffConnectingLane = new ConnectingLane(connectingLane.getLane().getValue() + 1);
        IntersectionReferenceID diffRemoteIntersection = new IntersectionReferenceID(remoteIntersection.getId().getValue() + 1);

        // different
        Connection connection2 = new Connection(diffConnectingLane);
        connection2.setRemoteIntersection(diffRemoteIntersection);
        connection2.setSignalGroup(signalGroup + 1);
        connection2.setUserClass(userClass + 1);
        connection2.setConnectionId(connectionId + 1);

        assertFalse(connection.equals(connection2));

        // different connecting lane
        connection2 = new Connection(diffConnectingLane);
        connection2.setRemoteIntersection(remoteIntersection);
        connection2.setSignalGroup(signalGroup);
        connection2.setUserClass(userClass);
        connection2.setConnectionId(connectionId);

        assertFalse(connection.equals(connection2));

        // different remote intersection
        connection2 = new Connection(connectingLane);
        connection2.setRemoteIntersection(diffRemoteIntersection);
        connection2.setSignalGroup(signalGroup);
        connection2.setUserClass(userClass);
        connection2.setConnectionId(connectionId);

        assertFalse(connection.equals(connection2));

        // different signal group
        connection2 = new Connection(connectingLane);
        connection2.setRemoteIntersection(remoteIntersection);
        connection2.setSignalGroup(signalGroup + 1);
        connection2.setUserClass(userClass);
        connection2.setConnectionId(connectionId);

        assertFalse(connection.equals(connection2));

        // different user class
        connection2 = new Connection(connectingLane);
        connection2.setRemoteIntersection(remoteIntersection);
        connection2.setSignalGroup(signalGroup);
        connection2.setUserClass(userClass + 1);
        connection2.setConnectionId(connectionId);

        assertFalse(connection.equals(connection2));

        // different connection ID
        connection2 = new Connection(connectingLane);
        connection2.setRemoteIntersection(remoteIntersection);
        connection2.setSignalGroup(signalGroup);
        connection2.setUserClass(userClass);
        connection2.setConnectionId(connectionId + 1);

        assertFalse(connection.equals(connection2));

        // same
        connection2 = new Connection(connectingLane);
        connection2.setRemoteIntersection(remoteIntersection);
        connection2.setSignalGroup(signalGroup);
        connection2.setUserClass(userClass);
        connection2.setConnectionId(connectionId);

        assertTrue(connection.equals(connection2));
    }
}
