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
 * The movement list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MovementList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the movement list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 8;

    /**
     * The maximum size of the movement list.
     */
    public static final int MAX_LIST_SIZE = 255;

    /**
     * The minimum size of the movement list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The movement list.
     */
    private MovementState[] movements;

    /**
     * A constructor setup only for decoding purposes.
     */
    public MovementList() {

        movements = new MovementState[0];
    }

    /**
     * A constructor for the movement list frame, where the size will be validated.
     * 
     * @param size The movement list size to set.
     */
    public MovementList(int size) {

        this.movements = new MovementState[validate(size)];
    }

    /**
     * A getter for the movement array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The movement array.
     */
    public MovementState[] getMovementArray() {

        return movements;
    }

    /**
     * Validates the movement list size.
     * 
     * @param size The size to be validated.
     * @return The movement list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < MovementList.MIN_LIST_SIZE || size > MovementList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the movement list frame must be in the range of %d to %d", MovementList.MIN_LIST_SIZE, MovementList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (movements.length < MovementList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough MovementState objects to encode the list. minimum: %d", MovementList.MIN_LIST_SIZE));
        }

        StringBuilder movementListBits = new StringBuilder(UPERInteger.encode(movements.length, MovementList.MIN_LIST_SIZE, MovementList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < movements.length; i++) {

            if (movements[i] == null) {

                throw new IllegalStateException(String.format("The movement list frame was not filled up to the amount specified. specified: %d, received: %d", movements.length, i + 1));
            }
            movementListBits.append(movements[i].encodeUPER());
        }
        return movementListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (MovementList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a MovementList frame (%d)", MovementList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, MovementList.NUM_BITS_LIST_SIZE), MovementList.MIN_LIST_SIZE));
        bits = bits.substring(MovementList.NUM_BITS_LIST_SIZE);

        movements = new MovementState[size];
        for (int i = 0; i < size; i++) {

            MovementState movement = new MovementState();
            bits = movement.decodeUPER(bits);
            movements[i] = movement;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(movements);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MovementList)) {

            return false;
        }
        MovementList frame = (MovementList)object;
        return Arrays.equals(this.movements, frame.movements);
    }
}
