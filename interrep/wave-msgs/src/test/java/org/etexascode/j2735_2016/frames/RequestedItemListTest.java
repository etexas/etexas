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

import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the requested item list frame.
 * 
 * @author ttevendale
 */
public class RequestedItemListTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructorMin() {

        RequestedItemList requestedItems = new RequestedItemList(RequestedItemList.MIN_LIST_SIZE);
        assertTrue(requestedItems.getRequestedItemArray().length == RequestedItemList.MIN_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        requestedItems = new RequestedItemList(RequestedItemList.MIN_LIST_SIZE - 1);
    }

    @Test
    public void testConstructorMax() {

        RequestedItemList requestedItems = new RequestedItemList(RequestedItemList.MAX_LIST_SIZE);
        assertTrue(requestedItems.getRequestedItemArray().length == RequestedItemList.MAX_LIST_SIZE);

        thrown.expect(IllegalArgumentException.class);
        requestedItems = new RequestedItemList(RequestedItemList.MAX_LIST_SIZE + 1);
    }

    @Test
    public void testConstructor() {

        int numItems = 12;
        RequestedItemList requestedItems = new RequestedItemList(numItems);
        assertTrue(requestedItems.getRequestedItemArray().length == numItems);
    }

    @Test
    public void testEncodeUPERMin() {

        RequestedItemList requestedItems = new RequestedItemList(RequestedItemList.MIN_LIST_SIZE);
        RequestedItem item = new RequestedItem(Item.G);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        itemArray[0] = item;

        String listSize = "00000";
        String remainingBits = item.encodeUPER();

        assertTrue((listSize + remainingBits).equals(requestedItems.encodeUPER()));

        requestedItems = new RequestedItemList(RequestedItemList.MIN_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        requestedItems.encodeUPER();
    }

    @Test
    public void testEncodeUPERMax() {

        String listSize = "11111";
        String remainingBits = "";

        RequestedItemList requestedItems = new RequestedItemList(RequestedItemList.MAX_LIST_SIZE);

        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        for (int i = 0; i < itemArray.length; i++) {

            RequestedItem item = new RequestedItem(Item.G);
            itemArray[i] = item;
            remainingBits += item.encodeUPER();
        }
        assertTrue((listSize + remainingBits).equals(requestedItems.encodeUPER()));

        requestedItems = new RequestedItemList(RequestedItemList.MAX_LIST_SIZE);
        thrown.expect(IllegalStateException.class);
        requestedItems.encodeUPER();
    }

    @Test
    public void testEncodeUPEREmpty() {

        RequestedItemList requestedItems = new RequestedItemList();
        thrown.expect(IllegalStateException.class);
        requestedItems.encodeUPER();
    }

    @Test
    public void testDecodeUPERMin() {

        RequestedItem item = new RequestedItem(Item.A);
        String listSize = "00000";
        String remainingBits = item.encodeUPER();

        RequestedItemList requestedItems = new RequestedItemList();
        requestedItems.decodeUPER(listSize + remainingBits);
        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        assertTrue(RequestedItemList.MIN_LIST_SIZE == itemArray.length);
        assertTrue(item.equals(itemArray[0]));
    }

    @Test
    public void testDecodeUPERMax() {

        RequestedItem item = new RequestedItem(Item.A);
        RequestedItem item2 = new RequestedItem(Item.G);

        String listSize = "11111";
        String remainingBits = item.encodeUPER();

        for (int i = 0; i < RequestedItemList.MAX_LIST_SIZE - 1; i++) {

            remainingBits += item2.encodeUPER();
        }

        RequestedItemList requestedItems = new RequestedItemList();
        requestedItems.decodeUPER(listSize + remainingBits);

        RequestedItem[] itemArray = requestedItems.getRequestedItemArray();
        assertTrue(RequestedItemList.MAX_LIST_SIZE == itemArray.length);
        assertTrue(item.equals(itemArray[0]));
        for (int i = 1; i < RequestedItemList.MAX_LIST_SIZE; i++) {

            assertTrue(item2.equals(itemArray[i]));
        }
    }

    @Test
    public void testDecodeUPERLessBits() {

        RequestedItemList requestedItems = new RequestedItemList();
        thrown.expect(IllegalArgumentException.class);
        requestedItems.decodeUPER("0101");
    }

    @Test
    public void testDecodeUPERNotEnoughObjects() {

        RequestedItemList requestedItems = new RequestedItemList();
        thrown.expect(IllegalArgumentException.class);
        // 11111 = 32 objects, but there's none
        requestedItems.decodeUPER("11111010100");
    }

    @Test
    public void testHashCode() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        requestedItems.getRequestedItemArray()[0] = new RequestedItem(Item.A);

        assertTrue(requestedItems.hashCode() == requestedItems.hashCode());

        RequestedItemList requestedItems2 = new RequestedItemList(2);

        assertFalse(requestedItems.hashCode() == requestedItems2.hashCode());

        requestedItems2 = new RequestedItemList(1);
        requestedItems2.getRequestedItemArray()[0] = new RequestedItem(Item.C);

        assertFalse(requestedItems.hashCode() == requestedItems2.hashCode());

        requestedItems2.getRequestedItemArray()[0] = new RequestedItem(Item.A);

        assertTrue(requestedItems.hashCode() == requestedItems2.hashCode());
    }

    @Test
    public void testEquals() {

        RequestedItemList requestedItems = new RequestedItemList(1);
        requestedItems.getRequestedItemArray()[0] = new RequestedItem(Item.A);

        assertTrue(requestedItems.equals(requestedItems));
        assertFalse(requestedItems.equals(null));
        assertFalse(requestedItems.equals(new String()));

        RequestedItemList requestedItems2 = new RequestedItemList(2);

        assertFalse(requestedItems.equals(requestedItems2));

        requestedItems2 = new RequestedItemList(1);
        requestedItems2.getRequestedItemArray()[0] = new RequestedItem(Item.C);

        assertFalse(requestedItems.equals(requestedItems2));

        requestedItems2.getRequestedItemArray()[0] = new RequestedItem(Item.A);

        assertTrue(requestedItems.equals(requestedItems2));
    }
}
