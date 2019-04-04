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
 * The connects to list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ConnectsToList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the connects to list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the connects to list.
     */
    public static final int MAX_LIST_SIZE = 16;

    /**
     * The minimum size of the connects to list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The connects to list.
     */
    private Connection[] connections;

    /**
     * A constructor setup only for decoding purposes.
     */
    public ConnectsToList() {

        connections = new Connection[0];
    }

    /**
     * A constructor for the connects to list frame, where the size will be validated.
     * 
     * @param size The connects to list size to set.
     */
    public ConnectsToList(int size) {

        this.connections = new Connection[validate(size)];
    }

    /**
     * A getter for the connection array. NOTE: Changes to the array will be reflected here as well.
     * 
     * @return The connection array.
     */
    public Connection[] getConnectionArray() {

        return connections;
    }

    /**
     * Validates the connects to list size.
     * 
     * @param size The size to be validated.
     * @return The connects to list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < ConnectsToList.MIN_LIST_SIZE || size > ConnectsToList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(String.format("The size of the connects to list frame must be in the range of %d to %d", ConnectsToList.MIN_LIST_SIZE, ConnectsToList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (connections.length < ConnectsToList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough Connection objects to encode the list. minimum: %d", ConnectsToList.MIN_LIST_SIZE));
        }

        StringBuilder connectionListBits = new StringBuilder(UPERInteger.encode(connections.length, ConnectsToList.MIN_LIST_SIZE, ConnectsToList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < connections.length; i++) {

            if (connections[i] == null) {

                throw new IllegalStateException(String.format("The connects to list frame was not filled up to the amount specified. specified: %d, received: %d", connections.length, i + 1));
            }
            connectionListBits.append(connections[i].encodeUPER());
        }
        return connectionListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ConnectsToList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ConnectsToList frame (%d)", ConnectsToList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, ConnectsToList.NUM_BITS_LIST_SIZE), ConnectsToList.MIN_LIST_SIZE));
        bits = bits.substring(ConnectsToList.NUM_BITS_LIST_SIZE);

        connections = new Connection[size];
        for (int i = 0; i < size; i++) {

            Connection connection = new Connection();
            bits = connection.decodeUPER(bits);
            connections[i] = connection;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(connections);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ConnectsToList)) {

            return false;
        }
        ConnectsToList frame = (ConnectsToList)object;
        return Arrays.equals(this.connections, frame.connections);
    }
}
