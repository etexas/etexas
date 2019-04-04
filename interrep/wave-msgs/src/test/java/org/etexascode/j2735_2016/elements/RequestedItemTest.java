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

import org.etexascode.j2735_2016.elements.RequestedItem.Item;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the requested item element.
 * 
 * @author ttevendale
 */
public class RequestedItemTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testConstructor() {

        // min value
        RequestedItem item = new RequestedItem(Item.RESERVED);
        assertTrue(Item.RESERVED.equals(item.getEnumeration()));

        // max value
        item = new RequestedItem(Item.Q);
        assertTrue(Item.Q.equals(item.getEnumeration()));

        item = new RequestedItem(Item.G);
        assertTrue(Item.G.equals(item.getEnumeration()));
    }

    @Test
    public void testEncodeUPER() {

        // test min
        RequestedItem item = new RequestedItem(Item.RESERVED);
        String encodedItem = item.encodeUPER();
        assertTrue("000000".equals(encodedItem));

        // test max
        item = new RequestedItem(Item.Q);
        encodedItem = item.encodeUPER();
        assertTrue("010000".equals(encodedItem));

        item = new RequestedItem(Item.D);
        encodedItem = item.encodeUPER();
        assertTrue("000100".equals(encodedItem));
    }

    @Test
    public void testDecodeUPER() {

        RequestedItem item = new RequestedItem();

        // test min
        String remainingBits = item.decodeUPER("000000");
        assertTrue("".equals(remainingBits));
        assertTrue(Item.RESERVED.equals(item.getEnumeration()));

        // test max
        remainingBits = item.decodeUPER("010000");
        assertTrue("".equals(remainingBits));
        assertTrue(Item.Q.equals(item.getEnumeration()));

        remainingBits = item.decodeUPER("000111");
        assertTrue("".equals(remainingBits));
        assertTrue(Item.G.equals(item.getEnumeration()));

        // one over the known values
        thrown.expect(IllegalArgumentException.class);
        item.decodeUPER("010001");
    }

    @Test
    public void testDecodeUPERExtension() {

        RequestedItem item = new RequestedItem();
        thrown.expect(IllegalArgumentException.class);
        item.decodeUPER("100000");
    }

    @Test
    public void testDecodeUPERLessBits() {

        RequestedItem item = new RequestedItem();
        thrown.expect(IllegalArgumentException.class);
        item.decodeUPER("");
    }

    @Test
    public void testDecodeUPERMoreBits() {

        RequestedItem item = new RequestedItem();
        String remainingBits = item.decodeUPER("0001010"); // 7
        assertTrue("0".equals(remainingBits));
        assertTrue(Item.E.equals(item.getEnumeration()));
    }

    @Test
    public void testHashCode() {

        RequestedItem item = new RequestedItem(Item.A);
        RequestedItem item2 = new RequestedItem(Item.B);

        assertFalse(item.hashCode() == item2.hashCode());
        assertTrue(item.hashCode() == item.hashCode());
        assertTrue(item2.hashCode() == item2.hashCode());

        RequestedItem item3 = new RequestedItem(item.getEnumeration());

        assertTrue(item.hashCode() == item3.hashCode());
        assertFalse(item2.hashCode() == item3.hashCode());
    }

    @Test
    public void testEquals() {

        RequestedItem item = new RequestedItem(Item.A);

        assertFalse(item.equals(null));

        assertTrue(item.equals(item));

        RequestedItem item2 = new RequestedItem(Item.G);

        assertFalse(item.equals(new String()));
        assertFalse(item.equals(item2));

        item2.setEnumeration(item.getEnumeration());
        assertTrue(item.equals(item2));
    }
}
