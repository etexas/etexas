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

import java.util.Objects;

import org.etexascode.j2735_2016.elements.AllowedManeuvers;
import org.etexascode.j2735_2016.elements.LaneID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The connecting lane frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class ConnectingLane implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the connecting lane.
     */
    public static final int NUM_BITS = 1;

    /**
     * The lane ID element.
     */
    private LaneID lane;

    /**
     * The allowed maneuvers element. (OPTIONAL)
     */
    private AllowedManeuvers maneuver;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public ConnectingLane() {

        lane = new LaneID();
    }

    /**
     * A constructor for the connecting lane frame for all required fields.
     * 
     * @param lane The lane ID element.
     */
    public ConnectingLane(LaneID lane) {

        this.lane = Objects.requireNonNull(lane);
    }

    /**
     * A constructor for the connecting lane frame for all required fields (primitive).
     * 
     * @param lane The lane ID value.
     */
    public ConnectingLane(int lane) {

        this.lane = new LaneID(lane);
    }

    /**
     * A getter for the lane ID element.
     * 
     * @return The lane ID element.
     */
    public LaneID getLane() {

        return lane;
    }

    /**
     * A setter for the lane ID element.
     * 
     * @param lane The lane ID element to set.
     */
    public void setLane(LaneID lane) {

        this.lane = Objects.requireNonNull(lane);
    }

    /**
     * A setter for the lane ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param lane The lane ID value to be set in the element.
     */
    public void setLane(int lane) {

        this.lane.setValue(lane);
    }

    /**
     * A getter for the allowed maneuvers element.
     * 
     * @return The allowed maneuvers element.
     */
    public AllowedManeuvers getManeuver() {

        return maneuver;
    }

    /**
     * A setter for the allowed maneuvers element.
     * 
     * @param maneuver The allowed maneuvers element to set.
     */
    public void setManeuver(AllowedManeuvers maneuver) {

        this.maneuver = maneuver;
    }

    @Override
    public String encodeUPER() {

        StringBuilder connectingLaneBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        connectingLaneBits.append(lane.encodeUPER());

        if (maneuver != null) {

            optionalBits.append('1');
            connectingLaneBits.append(maneuver.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        connectingLaneBits.insert(0, optionalBits);

        return connectingLaneBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (ConnectingLane.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a ConnectingLane frame (%d)", ConnectingLane.NUM_BITS));
        }

        String connectingLaneOptionalBits = bits.substring(0, ConnectingLane.NUM_BITS);
        bits = bits.substring(ConnectingLane.NUM_BITS);

        bits = lane.decodeUPER(bits);

        if (connectingLaneOptionalBits.charAt(0) == '1') {

            maneuver = new AllowedManeuvers();
            bits = maneuver.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(lane, maneuver);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof ConnectingLane)) {

            return false;
        }
        ConnectingLane frame = (ConnectingLane)object;
        return this.lane.equals(frame.lane)
                && Objects.equals(this.maneuver, frame.maneuver);
    }
}
