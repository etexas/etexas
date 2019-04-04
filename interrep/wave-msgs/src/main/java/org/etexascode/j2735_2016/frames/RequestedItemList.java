/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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

import java.util.Arrays;

import org.etexascode.j2735_2016.elements.RequestedItem;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The requested item list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RequestedItemList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the requested item list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 5;

    /**
     * The maximum size of the requested item list.
     */
    public static final int MAX_LIST_SIZE = 32;

    /**
     * The minimum size of the requested item list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The requested item list.
     */
    private RequestedItem[] items;

    /**
     * A constructor setup only for decoding purposes.
     */
    public RequestedItemList() {

        items = new RequestedItem[0];
    }

    /**
     * A constructor for the requested item list frame, where the size will be validated.
     * 
     * @param size The requested item list size to set.
     */
    public RequestedItemList(int size) {

        this.items = new RequestedItem[validate(size)];
    }

    /**
     * A getter for the requested item array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The requested item array.
     */
    public RequestedItem[] getRequestedItemArray() {

        return items;
    }

    /**
     * Validates the requested item list size.
     * 
     * @param size The size to be validated.
     * @return The requested item list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < RequestedItemList.MIN_LIST_SIZE || size > RequestedItemList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the requested item list frame must be in the range of %d to %d", RequestedItemList.MIN_LIST_SIZE, RequestedItemList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (items.length < RequestedItemList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough RequestedItem objects to encode the list. minimum: %d", RequestedItemList.MIN_LIST_SIZE));
        }

        StringBuilder requestedItemListBits = new StringBuilder(UPERInteger.encode(items.length, RequestedItemList.MIN_LIST_SIZE, RequestedItemList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < items.length; i++) {

            if (items[i] == null) {

                throw new IllegalStateException(String.format("The requested item list frame was not filled up to the amount specified. specified: %d, received: %d", items.length, i + 1));
            }
            requestedItemListBits.append(items[i].encodeUPER());
        }
        return requestedItemListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (RequestedItemList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a RequestedItemList frame (%d)", RequestedItemList.NUM_BITS_LIST_SIZE));
        }
        // 00000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, RequestedItemList.NUM_BITS_LIST_SIZE), RequestedItemList.MIN_LIST_SIZE));
        bits = bits.substring(RequestedItemList.NUM_BITS_LIST_SIZE);

        items = new RequestedItem[size];
        for (int i = 0; i < size; i++) {

            RequestedItem item = new RequestedItem();
            bits = item.decodeUPER(bits);
            items[i] = item;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(items);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RequestedItemList)) {

            return false;
        }
        RequestedItemList frame = (RequestedItemList)object;
        return Arrays.equals(this.items, frame.items);
    }
}
