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
 * The maneuver assist list frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ManeuverAssistList implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the maneuver assist list size.
     */
    public static final int NUM_BITS_LIST_SIZE = 4;

    /**
     * The maximum size of the maneuver assist list.
     */
    public static final int MAX_LIST_SIZE = 16;

    /**
     * The minimum size of the maneuver assist list.
     */
    public static final int MIN_LIST_SIZE = 1;

    /**
     * The maneuver assist list.
     */
    private ConnectionManeuverAssist[] assists;

    /**
     * A constructor setup only for decoding purposes.
     */
    public ManeuverAssistList() {

        assists = new ConnectionManeuverAssist[0];
    }

    /**
     * A constructor for the maneuver assist list frame, where the size will be validated.
     * 
     * @param size The maneuver assist list size to set.
     */
    public ManeuverAssistList(int size) {

        this.assists = new ConnectionManeuverAssist[validate(size)];
    }

    /**
     * A getter for the maneuver assist array. NOTE: Changes to the array will be reflected here as
     * well.
     * 
     * @return The maneuver assist array.
     */
    public ConnectionManeuverAssist[] getManeuverAssistArray() {

        return assists;
    }

    /**
     * Validates the maneuver assist list size.
     * 
     * @param size The size to be validated.
     * @return The maneuver assist list size that was successfully validated.
     */
    private int validate(int size) {

        if (size < ManeuverAssistList.MIN_LIST_SIZE || size > ManeuverAssistList.MAX_LIST_SIZE) {

            throw new IllegalArgumentException(
                    String.format("The size of the maneuver assist list frame must be in the range of %d to %d", ManeuverAssistList.MIN_LIST_SIZE, ManeuverAssistList.MAX_LIST_SIZE));
        }

        return size;
    }

    @Override
    public String encodeUPER() {

        if (assists.length < ManeuverAssistList.MIN_LIST_SIZE) {

            throw new IllegalStateException(String.format("There are not enough ConnectionManeuverAssist objects to encode the list. minimum: %d", ManeuverAssistList.MIN_LIST_SIZE));
        }

        StringBuilder maneuverAssistListBits = new StringBuilder(UPERInteger.encode(assists.length, ManeuverAssistList.MIN_LIST_SIZE, ManeuverAssistList.NUM_BITS_LIST_SIZE));

        for (int i = 0; i < assists.length; i++) {

            if (assists[i] == null) {

                throw new IllegalStateException(String.format("The maneuver assist list frame was not filled up to the amount specified. specified: %d, received: %d", assists.length, i + 1));
            }
            maneuverAssistListBits.append(assists[i].encodeUPER());
        }
        return maneuverAssistListBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ManeuverAssistList.NUM_BITS_LIST_SIZE > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ManeuverAssistList frame (%d)", ManeuverAssistList.NUM_BITS_LIST_SIZE));
        }
        // 0000 = 1
        int size = validate(UPERInteger.decode(bits.substring(0, ManeuverAssistList.NUM_BITS_LIST_SIZE), ManeuverAssistList.MIN_LIST_SIZE));
        bits = bits.substring(ManeuverAssistList.NUM_BITS_LIST_SIZE);

        assists = new ConnectionManeuverAssist[size];
        for (int i = 0; i < size; i++) {

            ConnectionManeuverAssist maneuverAssist = new ConnectionManeuverAssist();
            bits = maneuverAssist.decodeUPER(bits);
            assists[i] = maneuverAssist;
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Arrays.hashCode(assists);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ManeuverAssistList)) {

            return false;
        }
        ManeuverAssistList frame = (ManeuverAssistList)object;
        return Arrays.equals(this.assists, frame.assists);
    }
}
