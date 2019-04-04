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
 * Unit tests for the maneuver assist list frame.
 * 
 * @author ttevendale
 */
public class ManeuverAssistListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        ManeuverAssistList assists = new ManeuverAssistList(ManeuverAssistList.MIN_LIST_SIZE);
        assertTrue(assists.getManeuverAssistArray().length == ManeuverAssistList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        assists = new ManeuverAssistList(ManeuverAssistList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        ManeuverAssistList assists = new ManeuverAssistList(ManeuverAssistList.MAX_LIST_SIZE);
        assertTrue(assists.getManeuverAssistArray().length == ManeuverAssistList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        assists = new ManeuverAssistList(ManeuverAssistList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numAssists = 12;
        ManeuverAssistList assists = new ManeuverAssistList(numAssists);
        assertTrue(assists.getManeuverAssistArray().length == numAssists);
    }

    @Test
    public void testEncodeUPERMin() {

        ManeuverAssistList assists = new ManeuverAssistList(ManeuverAssistList.MIN_LIST_SIZE);
        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(15);
        ConnectionManeuverAssist[] assistArray = assists.getManeuverAssistArray();
        assistArray[0] = assist;

        String listSize = "0000";
        String remainingBits = assist.encodeUPER();

        assertTrue((listSize + remainingBits).equals(assists.encodeUPER()));

        assists = new ManeuverAssistList(ManeuverAssistList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        assists.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "1111";
        String remainingBits = "";

        ManeuverAssistList assists = new ManeuverAssistList(ManeuverAssistList.MAX_LIST_SIZE);

        ConnectionManeuverAssist[] assistArray = assists.getManeuverAssistArray();
        for (int i = 0; i < assistArray.length; i++) {

            ConnectionManeuverAssist assist = new ConnectionManeuverAssist(i);
            assistArray[i] = assist;
            remainingBits += assist.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(assists.encodeUPER()));

        assists = new ManeuverAssistList(ManeuverAssistList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        assists.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        ManeuverAssistList assists = new ManeuverAssistList();
        thrown.expect(IllegalStateException.class);
        assists.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        ConnectionManeuverAssist assist = new ConnectionManeuverAssist(80);
        String listSize = "0000";
        String remainingBits = assist.encodeUPER();

        ManeuverAssistList assists = new ManeuverAssistList();
        assists.decodeUPER(listSize + remainingBits);
        ConnectionManeuverAssist[] assistArray = assists.getManeuverAssistArray();
        assertTrue(ManeuverAssistList.MIN_LIST_SIZE == assistArray.length);
        assertTrue(assist.equals(assistArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        ManeuverAssistList assistsExpected = new ManeuverAssistList(ManeuverAssistList.MAX_LIST_SIZE);

        String listSize = "1111";
        String remainingBits = "";

        for (int i = 0; i < ManeuverAssistList.MAX_LIST_SIZE; i++) {

            ConnectionManeuverAssist assist = new ConnectionManeuverAssist(i);
            assist.setAvailableStorageLength(i + 10);

            assistsExpected.getManeuverAssistArray()[i] = assist;
            remainingBits += assist.encodeUPER();
        }

        ManeuverAssistList assists = new ManeuverAssistList();
        assists.decodeUPER(listSize + remainingBits);

        assertTrue(assistsExpected.equals(assists));
    }

    @Test
    public void testDecodeUPERLessBits() {

        ManeuverAssistList assists = new ManeuverAssistList();
        thrown.expect(IllegalArgumentException.class);
        assists.decodeUPER("011");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        ManeuverAssistList assists = new ManeuverAssistList();
        thrown.expect(IllegalArgumentException.class);
        // 1111 = 16 objects, but there's none
        assists.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        ManeuverAssistList assists = new ManeuverAssistList(1);
        assists.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(23);

        assertTrue(assists.hashCode() == assists.hashCode());

        ManeuverAssistList assists2 = new ManeuverAssistList(2);

        assertFalse(assists.hashCode() == assists2.hashCode());

        assists2 = new ManeuverAssistList(1);
        assists2.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(51);

        assertFalse(assists.hashCode() == assists2.hashCode());

        assists2.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(23);

        assertTrue(assists.hashCode() == assists2.hashCode());
    }

    @Test
    public void testEquals() {

        ManeuverAssistList assists = new ManeuverAssistList(1);
        assists.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(23);

        assertTrue(assists.equals(assists));
        assertFalse(assists.equals(null));
        assertFalse(assists.equals(new String()));

        ManeuverAssistList assists2 = new ManeuverAssistList(2);

        assertFalse(assists.equals(assists2));

        assists2 = new ManeuverAssistList(1);
        assists2.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(51);

        assertFalse(assists.equals(assists2));

        assists2.getManeuverAssistArray()[0] = new ConnectionManeuverAssist(23);

        assertTrue(assists.equals(assists2));
    }
}
