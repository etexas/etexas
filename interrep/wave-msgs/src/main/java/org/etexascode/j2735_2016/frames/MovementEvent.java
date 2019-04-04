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

import org.etexascode.j2735_2016.elements.MovementPhaseState;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The movement event frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class MovementEvent implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the movement event.
     */
    public static final int NUM_BITS = 4;

    /**
     * The movement phase state element.
     */
    private MovementPhaseState eventState;

    /**
     * The time change details frame. (OPTIONAL)
     */
    private TimeChangeDetails timing;

    /**
     * The advisory speed list frame. (OPTIONAL)
     */
    private AdvisorySpeedList speeds;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public MovementEvent() {

        eventState = new MovementPhaseState();
    }

    /**
     * A constructor for the movement event frame for all required fields.
     * 
     * @param eventState The movement phase state element.
     */
    public MovementEvent(MovementPhaseState eventState) {

        this.eventState = Objects.requireNonNull(eventState);
    }

    /**
     * A constructor for the movement event frame for all required fields (primitive).
     * 
     * @param eventState The movement phase state element.
     */
    public MovementEvent(MovementPhase eventState) {

        this.eventState = new MovementPhaseState(Objects.requireNonNull(eventState));
    }

    /**
     * A getter for the movement phase state element.
     * 
     * @return The movement phase state element.
     */
    public MovementPhaseState getEventState() {

        return eventState;
    }

    /**
     * A setter for the movement phase state element.
     * 
     * @param eventState The movement phase state element to set.
     */
    public void setEventState(MovementPhaseState eventState) {

        this.eventState = Objects.requireNonNull(eventState);
    }

    /**
     * A setter for the movement phase state element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param eventState The movement phase enumeration to be set in the element.
     */
    public void setEventState(MovementPhase eventState) {

        this.eventState.setEnumeration(Objects.requireNonNull(eventState));
    }

    /**
     * A getter for the time change details frame.
     * 
     * @return The time change details frame.
     */
    public TimeChangeDetails getTiming() {

        return timing;
    }

    /**
     * A setter for the time change details frame.
     * 
     * @param timing The time change details frame to set.
     */
    public void setTiming(TimeChangeDetails timing) {

        this.timing = timing;
    }

    /**
     * A getter for the advisory speed list frame.
     * 
     * @return The advisory speed list frame.
     */
    public AdvisorySpeedList getSpeeds() {

        return speeds;
    }

    /**
     * A setter for the advisory speed list frame.
     * 
     * @param speeds The advisory speed list frame to set.
     */
    public void setSpeeds(AdvisorySpeedList speeds) {

        this.speeds = speeds;
    }

    @Override
    public String encodeUPER() {

        StringBuilder movementEventBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having the extension usable so setting to off.
        optionalBits.append('0');

        movementEventBits.append(eventState.encodeUPER());

        if (timing != null) {

            optionalBits.append('1');
            movementEventBits.append(timing.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (speeds != null) {

            optionalBits.append('1');
            movementEventBits.append(speeds.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        movementEventBits.insert(0, optionalBits);

        return movementEventBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (MovementEvent.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a MovementEvent frame (%d)", MovementEvent.NUM_BITS));
        }

        String movementEventOptionalBits = bits.substring(0, MovementEvent.NUM_BITS);
        bits = bits.substring(MovementEvent.NUM_BITS);

        if (movementEventOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The MovementEvent extension is not supported");
        }

        if (movementEventOptionalBits.charAt(3) != '0') {

            throw new IllegalArgumentException("The MovementEvent regional extension is not supported");
        }

        bits = eventState.decodeUPER(bits);

        if (movementEventOptionalBits.charAt(1) == '1') {

            timing = new TimeChangeDetails();
            bits = timing.decodeUPER(bits);
        }

        if (movementEventOptionalBits.charAt(2) == '1') {

            speeds = new AdvisorySpeedList();
            bits = speeds.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(eventState, timing, speeds);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof MovementEvent)) {

            return false;
        }
        MovementEvent frame = (MovementEvent)object;
        return this.eventState.equals(frame.eventState)
                && Objects.equals(this.timing, frame.timing)
                && Objects.equals(this.speeds, frame.speeds);
    }
}
