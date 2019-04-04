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
package org.etexascode.j2735_2016.messages;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.etexascode.j2735_2016.elements.TemporaryID;
import org.etexascode.j2735_2016.frames.RequestedItemList;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the common safety request message.
 * 
 * @author ttevendale
 */
public class CommonSafetyRequestTest {

    CommonSafetyRequest csr;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void init() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(80);

        MsgCount msgCount = new MsgCount(50);

        TemporaryID id = new TemporaryID("abcdef12");

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.G);

        csr = new CommonSafetyRequest(requestedItems);
        csr.setTimeStamp(timeStamp);
        csr.setMsgCount(msgCount);
        csr.setId(id);
    }

    @Test
    public void testConstructor() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.B);

        CommonSafetyRequest csr = new CommonSafetyRequest(requestedItems);

        assertTrue(requestedItems.equals(csr.getRequests()));

        thrown.expect(NullPointerException.class);
        new CommonSafetyRequest(null);
    }

    @Test
    public void testSetTimeStampPrimitive() {

        CommonSafetyRequest csr = new CommonSafetyRequest();

        assertNull(csr.getTimeStamp());

        int timeStamp = 20;
        csr.setTimeStamp(timeStamp);
        assertTrue(csr.getTimeStamp().getValue() == timeStamp);

        timeStamp = 15;
        csr.setTimeStamp(timeStamp);
        assertTrue(csr.getTimeStamp().getValue() == timeStamp);
    }

    @Test
    public void testSetMsgCountPrimitive() {

        CommonSafetyRequest csr = new CommonSafetyRequest();

        assertNull(csr.getMsgCount());

        int msgCount = 20;
        csr.setMsgCount(msgCount);
        assertTrue(csr.getMsgCount().getValue() == msgCount);

        msgCount = 15;
        csr.setMsgCount(msgCount);
        assertTrue(csr.getMsgCount().getValue() == msgCount);
    }

    @Test
    public void testSetIdPrimitive() {

        CommonSafetyRequest csr = new CommonSafetyRequest();

        assertNull(csr.getId());

        String id = "01010101";
        csr.setId(id);
        assertTrue(csr.getId().getValue().equals(id));

        id = "10101010";
        csr.setId(id);
        assertTrue(csr.getId().getValue().equals(id));
    }

    @Test
    public void testSetRequests() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.B);

        CommonSafetyRequest csr = new CommonSafetyRequest();
        csr.setRequests(requestedItems);

        assertTrue(requestedItems.equals(csr.getRequests()));

        thrown.expect(NullPointerException.class);
        csr.setRequests(null);
    }

    @Test
    public void testEncodeUPERMin() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.G);
        CommonSafetyRequest csr = new CommonSafetyRequest(requestedItems);

        String csrOptionals = "00000";
        String requestList = requestedItems.encodeUPER();
        assertTrue((csrOptionals + requestList).equals(csr.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(80);

        MsgCount msgCount = new MsgCount(50);

        TemporaryID id = new TemporaryID("abcdef12");

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.G);

        CommonSafetyRequest csr = new CommonSafetyRequest(requestedItems);
        csr.setTimeStamp(timeStamp);
        csr.setMsgCount(msgCount);
        csr.setId(id);

        String csrOptionals = "01110";
        String remainingBits = timeStamp.encodeUPER() + msgCount.encodeUPER() + id.encodeUPER() + requestedItems.encodeUPER();

        assertTrue((csrOptionals + remainingBits).equals(csr.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.G);

        String csrOptionals = "00000";

        CommonSafetyRequest csr = new CommonSafetyRequest();
        String remainingBits = csr.decodeUPER(csrOptionals + requestedItems.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(csr.getTimeStamp());
        assertNull(csr.getMsgCount());
        assertNull(csr.getId());

        RequestedItem[] csrItemArray = csr.getRequests().getRequestedItemArray();
        assertTrue(csrItemArray.length == itemArray.length);
        assertTrue(itemArray[0].getEnumeration().equals(csrItemArray[0].getEnumeration()));
    }

    @Test
    public void testDecodeUPERMax() {

        MinuteOfTheYear timeStamp = new MinuteOfTheYear(4040);

        MsgCount msgCount = new MsgCount(120);

        TemporaryID id = new TemporaryID("fbcaef12");

        RequestedItemList requestedItems = new RequestedItemList(1);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = new RequestedItem(Item.A);

        String csrOptionals = "01110";

        CommonSafetyRequest csr = new CommonSafetyRequest();
        String remainingBits = csr.decodeUPER(csrOptionals + timeStamp.encodeUPER() + msgCount.encodeUPER() + id.encodeUPER() + requestedItems.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(csr.getTimeStamp().getValue() == timeStamp.getValue());
        assertTrue(csr.getMsgCount().getValue() == msgCount.getValue());
        assertTrue(csr.getId().getValue().equalsIgnoreCase(id.getValue()));

        RequestedItem[] csrItemArray = csr.getRequests().getRequestedItemArray();
        assertTrue(csrItemArray.length == itemArray.length);
        assertTrue(itemArray[0].getEnumeration().equals(csrItemArray[0].getEnumeration()));
    }

    @Test
    public void testDecodeUPERExtension() {

        String csrOptionals = "11110";

        CommonSafetyRequest csr = new CommonSafetyRequest();
        thrown.expect(IllegalArgumentException.class);
        csr.decodeUPER(csrOptionals);
    }

    @Test
    public void testDecodeUPERRegionalExtension() {

        String csrOptionals = "01111";

        CommonSafetyRequest csr = new CommonSafetyRequest();
        thrown.expect(IllegalArgumentException.class);
        csr.decodeUPER(csrOptionals);
    }

    @Test
    public void testDecodeUPERLessBits() {

        String csrOptionals = "0111";

        CommonSafetyRequest csr = new CommonSafetyRequest();
        thrown.expect(IllegalArgumentException.class);
        csr.decodeUPER(csrOptionals);
    }

    @Test
    public void testHashCode() {

        RequestedItemList requestedItems = csr.getRequests();
        int timeStamp = csr.getTimeStamp().getValue();
        int msgCount = csr.getMsgCount().getValue();
        String id = csr.getId().getValue();

        RequestedItemList requestedItems2 = new RequestedItemList(2);
        RequestedItem[] itemArray2 = requestedItems2.getRequestedItemArray();
        itemArray2[0] = new RequestedItem(Item.G);
        itemArray2[0] = new RequestedItem(Item.A);

        CommonSafetyRequest csr2 = new CommonSafetyRequest(requestedItems2);
        csr2.setTimeStamp(timeStamp + 1);
        csr2.setMsgCount(msgCount + 1);
        csr2.setId("FFFFFFFF");

        assertFalse(csr.hashCode() == csr2.hashCode());
        assertTrue(csr.hashCode() == csr.hashCode());
        assertTrue(csr2.hashCode() == csr2.hashCode());

        CommonSafetyRequest csr3 = new CommonSafetyRequest(requestedItems);
        csr3.setTimeStamp(timeStamp);
        csr3.setMsgCount(msgCount);
        csr3.setId(id);

        assertTrue(csr.hashCode() == csr3.hashCode());
        assertFalse(csr2.hashCode() == csr3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(csr.equals(csr));
        assertFalse(csr.equals(null));
        assertFalse(csr.equals(new String()));

        RequestedItemList requestedItems = csr.getRequests();
        int timeStamp = csr.getTimeStamp().getValue();
        int msgCount = csr.getMsgCount().getValue();
        String id = csr.getId().getValue();

        RequestedItemList requestedItems2 = new RequestedItemList(2);
        RequestedItem[] itemArray2 = requestedItems2.getRequestedItemArray();
        itemArray2[0] = new RequestedItem(Item.G);
        itemArray2[0] = new RequestedItem(Item.A);

        // different
        CommonSafetyRequest csr2 = new CommonSafetyRequest(requestedItems2);
        csr2.setTimeStamp(timeStamp + 1);
        csr2.setMsgCount(msgCount + 1);
        csr2.setId("FFFFFFFF");

        assertFalse(csr.equals(csr2));

        // different requests
        csr2 = new CommonSafetyRequest(requestedItems2);
        csr2.setTimeStamp(timeStamp);
        csr2.setMsgCount(msgCount);
        csr2.setId(id);

        assertFalse(csr.equals(csr2));

        // different timeStamp
        csr2 = new CommonSafetyRequest(requestedItems);
        csr2.setTimeStamp(timeStamp + 1);
        csr2.setMsgCount(msgCount);
        csr2.setId(id);

        assertFalse(csr.equals(csr2));

        // different msgCount
        csr2 = new CommonSafetyRequest(requestedItems);
        csr2.setTimeStamp(timeStamp);
        csr2.setMsgCount(msgCount + 1);
        csr2.setId(id);

        assertFalse(csr.equals(csr2));

        // different id
        csr2 = new CommonSafetyRequest(requestedItems);
        csr2.setTimeStamp(timeStamp);
        csr2.setMsgCount(msgCount);
        csr2.setId("FFFFFFFF");

        assertFalse(csr.equals(csr2));

        // same
        csr2 = new CommonSafetyRequest(requestedItems);
        csr2.setTimeStamp(timeStamp);
        csr2.setMsgCount(msgCount);
        csr2.setId(id);

        assertTrue(csr.equals(csr2));
    }

    @Test
    public void testEqualsNull() {

        RequestedItemList requestedItems = csr.getRequests();
        int timeStamp = csr.getTimeStamp().getValue();
        int msgCount = csr.getMsgCount().getValue();
        String id = csr.getId().getValue();

        CommonSafetyRequest csr2 = new CommonSafetyRequest(requestedItems);

        assertFalse(csr.equals(csr2));

        csr2.setTimeStamp(timeStamp);

        assertFalse(csr.equals(csr2));

        csr2.setMsgCount(msgCount);

        assertFalse(csr.equals(csr2));

        csr2.setId(id);

        assertTrue(csr.equals(csr2));
    }
}
