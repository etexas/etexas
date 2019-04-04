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

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;
import org.etexascode.j2735_2016.util.UPERInteger;

/**
 * The movement event list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MovementEventList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the movement event list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the movement event list.
     */
    public static final int MAX_LIST_SIZE = 16;

    /**
     * The minimum size of the movement event list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The movement event list.
     */
    private MovementEvent[] events;

    /**
     * A constructor setup only for decoding purposes.
     */
    public MovementEventList() {

        events = new MovementEvent[0];
    }

    /**
     * A constructor for the movement event list frame, where the size will be validated.
     * 
     * @param size The movement event list size to set.
     */
    public MovementEventList(int size) {

        this.events = new MovementEvent[validate(size)];
    }

    /**
     * A getter for the movement event array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The movement event array.
     */
    public MovementEvent[] getMovementEventArray() {

        return events;
    }

    /**
     * Validates the movement event list size.
     * 
     * @param size The size to be validated.
     * @return The movement event list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < MovementEventList.MIN_LIST_SIZE || size > MovementEventList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the movement event list frame must be in the range of %d to %d", MovementEventList.MIN_LIST_SIZE, MovementEventList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (events.length < MovementEventList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough MovementEvent objects to encode the list. minimum: %d", MovementEventList.MIN_LIST_SIZE));
        }

        StringBuilder movementEventListBits = new StringBuilder(UPERInteger.encode(events.length, MovementEventList.MIN_LIST_SIZE, MovementEventList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < events.length; i++) {

            if (events[i] == null) {

                throw new IllegalStateException(String.format("The movement event list frame was not filled up to the amount specified. specified: %d, received: %d", events.length, i + 1));
            }
            movementEventListBits.append(events[i].encodeUPER());
        }
        return movementEventListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (MovementEventList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a MovementEventList frame (%d)", MovementEventList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, MovementEventList.NUM_BITS_LIST_SIZE), MovementEventList.MIN_LIST_SIZE));
        bits = bits.substring(MovementEventList.NUM_BITS_LIST_SIZE);

        events = new MovementEvent[size];
        for (int i = 0; i < size; i++) {

            MovementEvent event = new MovementEvent();
            bits = event.decodeUPER(bits);
            events[i] = event;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(events);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MovementEventList)) {

            return false;
        }
        MovementEventList frame = (MovementEventList)object;
        return Arrays.equals(this.events, frame.events);
    }
}
