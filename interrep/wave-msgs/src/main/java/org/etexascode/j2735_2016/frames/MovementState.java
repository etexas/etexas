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

import org.etexascode.j2735_2016.elements.SignalGroupID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The movement state frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MovementState implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the movement state.
     */
    public static final int NUM_BITS = 4;

    /**
     * The signal group ID element.
     */
    private SignalGroupID signalGroup;

    /**
     * The movement event list frame.
     */
    private MovementEventList stateTimeSpeed;

    /**
     * The maneuver assist list frame. (OPTIONAL)
     */
    private ManeuverAssistList maneuverAssistList;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public MovementState() {

        signalGroup = new SignalGroupID();
        stateTimeSpeed = new MovementEventList();
    }

    /**
     * A constructor for the movement state frame for all required fields.
     * 
     * @param signalGroup The signal group ID element.
     * @param stateTimeSpeed The movement event list frame.
     */
    public MovementState(SignalGroupID signalGroup, MovementEventList stateTimeSpeed) {

        this.signalGroup = Objects.requireNonNull(signalGroup);
        this.stateTimeSpeed = Objects.requireNonNull(stateTimeSpeed);
    }

    /**
     * A constructor for the movement state frame for all required fields (primitive).
     * 
     * @param signalGroup The signal group ID value.
     * @param stateTimeSpeed The movement event list frame.
     */
    public MovementState(int signalGroup, MovementEventList stateTimeSpeed) {

        this.signalGroup = new SignalGroupID(signalGroup);
        this.stateTimeSpeed = Objects.requireNonNull(stateTimeSpeed);
    }

    /**
     * A getter for the signal group ID element.
     * 
     * @return The signal group ID element.
     */
    public SignalGroupID getSignalGroup() {

        return signalGroup;
    }

    /**
     * A setter for the signal group ID element.
     * 
     * @param signalGroup The signal group ID element to set.
     */
    public void setSignalGroup(SignalGroupID signalGroup) {

        this.signalGroup = Objects.requireNonNull(signalGroup);
    }

    /**
     * A setter for the signal group ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param signalGroup The signal group ID value to be set in the element.
     */
    public void setSignalGroup(int signalGroup) {

        this.signalGroup.setValue(signalGroup);
    }

    /**
     * A getter for the movement event list frame.
     * 
     * @return The movement event list frame.
     */
    public MovementEventList getStateTimeSpeed() {

        return stateTimeSpeed;
    }

    /**
     * A setter for the movement event list frame.
     * 
     * @param stateTimeSpeed The movement event list frame to set.
     */
    public void setStateTimeSpeed(MovementEventList stateTimeSpeed) {

        this.stateTimeSpeed = Objects.requireNonNull(stateTimeSpeed);
    }

    /**
     * A getter for the maneuver assist list frame.
     * 
     * @return The maneuver assist list frame.
     */
    public ManeuverAssistList getManeuverAssistList() {

        return maneuverAssistList;
    }

    /**
     * A setter for the maneuver assist list frame.
     * 
     * @param maneuverAssistList The maneuver assist list frame to set.
     */
    public void setManeuverAssistList(ManeuverAssistList maneuverAssistList) {

        this.maneuverAssistList = maneuverAssistList;
    }

    @Override
    public String encodeUPER() {

        StringBuilder movementStateBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having either the extension or descriptive name usable so setting
        // both to off.
        optionalBits.append("00");

        movementStateBits.append(signalGroup.encodeUPER());
        movementStateBits.append(stateTimeSpeed.encodeUPER());

        if (maneuverAssistList != null) {

            optionalBits.append('1');
            movementStateBits.append(maneuverAssistList.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        movementStateBits.insert(0, optionalBits);

        return movementStateBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (MovementState.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a MovementState frame (%d)", MovementState.NUM_BITS));
        }

        String movementStateOptionalBits = bits.substring(0, MovementState.NUM_BITS);
        bits = bits.substring(MovementState.NUM_BITS);

        if (movementStateOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The MovementState extension is not supported");
        }

        if (movementStateOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The MovementState descriptive name is not supported");
        }

        if (movementStateOptionalBits.charAt(3) != '0') {

            throw new IllegalArgumentException("The MovementState regional extension is not supported");
        }

        bits = signalGroup.decodeUPER(bits);
        bits = stateTimeSpeed.decodeUPER(bits);

        if (movementStateOptionalBits.charAt(2) == '1') {

            maneuverAssistList = new ManeuverAssistList();
            bits = maneuverAssistList.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(signalGroup, stateTimeSpeed, maneuverAssistList);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MovementState)) {

            return false;
        }
        MovementState frame = (MovementState)object;
        return this.signalGroup.equals(frame.signalGroup)
                && this.stateTimeSpeed.equals(frame.stateTimeSpeed)
                && Objects.equals(this.maneuverAssistList, frame.maneuverAssistList);
    }
}
