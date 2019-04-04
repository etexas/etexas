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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the connects to list frame.
 * 
 * @author ttevendale
 */
public class ConnectsToListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        ConnectsToList connections = new ConnectsToList(ConnectsToList.MIN_LIST_SIZE);
        assertTrue(connections.getConnectionArray().length == ConnectsToList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        connections = new ConnectsToList(ConnectsToList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        ConnectsToList connections = new ConnectsToList(ConnectsToList.MAX_LIST_SIZE);
        assertTrue(connections.getConnectionArray().length == ConnectsToList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        connections = new ConnectsToList(ConnectsToList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numConnections = 12;
        ConnectsToList connections = new ConnectsToList(numConnections);
        assertTrue(connections.getConnectionArray().length == numConnections);
    }

    @Test
    public void testEncodeUPERMin() {

        ConnectsToList connections = new ConnectsToList(ConnectsToList.MIN_LIST_SIZE);
        Connection connection = new Connection(new ConnectingLane(1));
        Connection[] connectionArray = connections.getConnectionArray();
        connectionArray[0] = connection;

        String listSize = "0000";
        String remainingBits = connection.encodeUPER();

        assertTrue((listSize + remainingBits).equals(connections.encodeUPER()));

        connections = new ConnectsToList(ConnectsToList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        connections.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        ConnectsToList connections = new ConnectsToList(ConnectsToList.MAX_LIST_SIZE);

        Connection[] connectionArray = connections.getConnectionArray();
        for (int i = 0; i < connectionArray.length; i++) {

            Connection connection = new Connection(new ConnectingLane(i));
            connectionArray[i] = connection;
            remainingBits += connection.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(connections.encodeUPER()));

        connections = new ConnectsToList(ConnectsToList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        connections.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        ConnectsToList connections = new ConnectsToList();
        thrown.expect(IllegalStateException.class);
        connections.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        Connection connection = new Connection(new ConnectingLane(2));
        String listSize = "0000";
        String remainingBits = connection.encodeUPER();

        ConnectsToList connections = new ConnectsToList();
        connections.decodeUPER(listSize + remainingBits);
        Connection[] connectionArray = connections.getConnectionArray();
        assertTrue(ConnectsToList.MIN_LIST_SIZE == connectionArray.length);
        assertTrue(connection.equals(connectionArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        Connection connection = new Connection(new ConnectingLane(3));
        Connection connection2 = new Connection(new ConnectingLane(4));

        String listSize = "1111";
        String remainingBits = connection.encodeUPER();

        for (int i = 0; i < ConnectsToList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += connection2.encodeUPER();
        }

        ConnectsToList connections = new ConnectsToList();
        connections.decodeUPER(listSize + remainingBits);

        Connection[] connectionArray = connections.getConnectionArray();
        assertTrue(ConnectsToList.MAX_LIST_SIZE == connectionArray.length);
        assertTrue(connection.equals(connectionArray[0]));
        for (int i = 1; i < ConnectsToList.MAX_LIST_SIZE; i++) {

            assertTrue(connection2.equals(connectionArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        ConnectsToList connections = new ConnectsToList();
        thrown.expect(IllegalArgumentException.class);
        connections.decodeUPER("001");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        ConnectsToList connections = new ConnectsToList();
        thrown.expect(IllegalArgumentException.class);
        // 1100 = 13 objects, but there's none
        connections.decodeUPER("110011010");
    }

    @Test
    public void testHashCode() {

        ConnectsToList connections = new ConnectsToList(1);
        connections.getConnectionArray()[0] = new Connection(new ConnectingLane(10));

        assertTrue(connections.hashCode() == connections.hashCode());

        ConnectsToList connections2 = new ConnectsToList(2);

        assertFalse(connections.hashCode() == connections2.hashCode());

        connections2 = new ConnectsToList(1);
        connections2.getConnectionArray()[0] = new Connection(new ConnectingLane(20));

        assertFalse(connections.hashCode() == connections2.hashCode());

        connections2.getConnectionArray()[0] = new Connection(new ConnectingLane(10));

        assertTrue(connections.hashCode() == connections2.hashCode());
    }

    @Test
    public void testEquals() {

        ConnectsToList connections = new ConnectsToList(1);
        connections.getConnectionArray()[0] = new Connection(new ConnectingLane(10));

        assertTrue(connections.equals(connections));
        assertFalse(connections.equals(null));
        assertFalse(connections.equals(new String()));

        ConnectsToList connections2 = new ConnectsToList(2);

        assertFalse(connections.equals(connections2));

        connections2 = new ConnectsToList(1);
        connections2.getConnectionArray()[0] = new Connection(new ConnectingLane(20));

        assertFalse(connections.equals(connections2));

        connections2.getConnectionArray()[0] = new Connection(new ConnectingLane(10));

        assertTrue(connections.equals(connections2));
    }
}
