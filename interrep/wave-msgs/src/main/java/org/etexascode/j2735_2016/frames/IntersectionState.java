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

import org.etexascode.j2735_2016.elements.DSecond;
import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The intersection state frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class IntersectionState implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the intersection state.
     */
    public static final int NUM_BITS = 7;

    /**
     * The intersection reference ID frame.
     */
    private IntersectionReferenceID id;

    /**
     * The message count element.
     */
    private MsgCount revision;

    /**
     * The intersection status object element.
     */
    private IntersectionStatusObject status;

    /**
     * The minute of the year element. (OPTIONAL)
     */
    private MinuteOfTheYear moy;

    /**
     * The second element. (OPTIONAL)
     */
    private DSecond timeStamp;

    /**
     * The enabled lane list frame. (OPTIONAL)
     */
    private EnabledLaneList enabledLanes;

    /**
     * The movement list frame.
     */
    private MovementList states;

    /**
     * The maneuver assist list frame. (OPTIONAL)
     */
    private ManeuverAssistList maneuverAssistList;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public IntersectionState() {

        id = new IntersectionReferenceID();
        revision = new MsgCount();
        status = new IntersectionStatusObject();
        states = new MovementList();
    }

    /**
     * A constructor for the intersection state frame for all required fields.
     * 
     * @param id The intersection reference ID frame.
     * @param revision The message count element.
     * @param status The intersection status object element.
     * @param states The movement list frame.
     */
    public IntersectionState(IntersectionReferenceID id, MsgCount revision, IntersectionStatusObject status, MovementList states) {

        this.id = Objects.requireNonNull(id);
        this.revision = Objects.requireNonNull(revision);
        this.status = Objects.requireNonNull(status);
        this.states = Objects.requireNonNull(states);
    }

    /**
     * A constructor for the intersection state frame for all required fields (primitive).
     * 
     * @param id The intersection reference ID frame.
     * @param revision The message count element.
     * @param status The intersection status object element.
     * @param states The movement list frame.
     */
    public IntersectionState(IntersectionReferenceID id, int revision, IntersectionStatusObject status, MovementList states) {

        this.id = Objects.requireNonNull(id);
        this.revision = new MsgCount(revision);
        this.status = Objects.requireNonNull(status);
        this.states = Objects.requireNonNull(states);
    }

    /**
     * A getter for the intersection reference ID frame.
     * 
     * @return The intersection reference ID frame.
     */
    public IntersectionReferenceID getId() {

        return id;
    }

    /**
     * A setter for the intersection reference ID frame.
     * 
     * @param id The intersection reference ID frame to set.
     */
    public void setId(IntersectionReferenceID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A getter for the message count element.
     * 
     * @return The message count element.
     */
    public MsgCount getRevision() {

        return revision;
    }

    /**
     * A setter for the message count element.
     * 
     * @param revision The message count element to set.
     */
    public void setRevision(MsgCount revision) {

        this.revision = Objects.requireNonNull(revision);
    }

    /**
     * A setter for the message count element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param revision The message count value to be set in the element.
     */
    public void setRevision(int revision) {

        this.revision.setValue(revision);
    }

    /**
     * A getter for the intersection status object element.
     * 
     * @return The intersection status object element.
     */
    public IntersectionStatusObject getStatus() {

        return status;
    }

    /**
     * A setter for the intersection status object element.
     * 
     * @param status The intersection status object element to set.
     */
    public void setStatus(IntersectionStatusObject status) {

        this.status = Objects.requireNonNull(status);
    }

    /**
     * A getter for the minute of the year element.
     * 
     * @return The minute of the year element.
     */
    public MinuteOfTheYear getMoy() {

        return moy;
    }

    /**
     * A setter for the minute of the year element.
     * 
     * @param moy The minute of the year element to set.
     */
    public void setMoy(MinuteOfTheYear moy) {

        this.moy = moy;
    }

    /**
     * A setter for the minute of the year element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param moy The minute of the year value to be set in the element.
     */
    public void setMoy(int moy) {

        if (this.moy == null) {

            this.moy = new MinuteOfTheYear();
        }
        this.moy.setValue(moy);
    }

    /**
     * A getter for the second element.
     * 
     * @return The second element.
     */
    public DSecond getTimeStamp() {

        return timeStamp;
    }

    /**
     * A setter for the second element.
     * 
     * @param timeStamp The second element to set.
     */
    public void setTimeStamp(DSecond timeStamp) {

        this.timeStamp = timeStamp;
    }

    /**
     * A setter for the second element. Allows primitive data to be passed to the primitive element.
     * 
     * @param timeStamp The second value to be set in the element.
     */
    public void setTimeStamp(int timeStamp) {

        if (this.timeStamp == null) {

            this.timeStamp = new DSecond();
        }
        this.timeStamp.setValue(timeStamp);
    }

    /**
     * A getter for the enabled lane list frame.
     * 
     * @return The enabled lane list frame.
     */
    public EnabledLaneList getEnabledLanes() {

        return enabledLanes;
    }

    /**
     * A setter for the enabled lane list frame.
     * 
     * @param enabledLanes The enabled lane list frame to set.
     */
    public void setEnabledLanes(EnabledLaneList enabledLanes) {

        this.enabledLanes = enabledLanes;
    }

    /**
     * A getter for the movement list frame.
     * 
     * @return The movement list frame.
     */
    public MovementList getStates() {

        return states;
    }

    /**
     * A setter for the movement list frame.
     * 
     * @param states The movement list frame to set.
     */
    public void setStates(MovementList states) {

        this.states = Objects.requireNonNull(states);
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

        StringBuilder intersectionStateBits = new StringBuilder();
        StringBuilder optionalBits = new StringBuilder();

        // we aren't planning on having either the extension or descriptive name usable so setting
        // both to off.
        optionalBits.append("00");

        intersectionStateBits.append(id.encodeUPER());
        intersectionStateBits.append(revision.encodeUPER());
        intersectionStateBits.append(status.encodeUPER());

        if (moy != null) {

            optionalBits.append('1');
            intersectionStateBits.append(moy.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (timeStamp != null) {

            optionalBits.append('1');
            intersectionStateBits.append(timeStamp.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        if (enabledLanes != null) {

            optionalBits.append('1');
            intersectionStateBits.append(enabledLanes.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        intersectionStateBits.append(states.encodeUPER());

        if (maneuverAssistList != null) {

            optionalBits.append('1');
            intersectionStateBits.append(maneuverAssistList.encodeUPER());
        }
        else {

            optionalBits.append('0');
        }

        // we aren't planning on having the regional extension usable so setting to off.
        optionalBits.append('0');
        intersectionStateBits.insert(0, optionalBits);

        return intersectionStateBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (IntersectionState.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for an IntersectionState frame (%d)", IntersectionState.NUM_BITS));
        }

        String intersectionStateOptionalBits = bits.substring(0, IntersectionState.NUM_BITS);
        bits = bits.substring(IntersectionState.NUM_BITS);

        if (intersectionStateOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The IntersectionState extension is not supported");
        }

        if (intersectionStateOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The IntersectionState descriptive name is not supported");
        }

        if (intersectionStateOptionalBits.charAt(6) != '0') {

            throw new IllegalArgumentException("The IntersectionState regional extension is not supported");
        }

        bits = id.decodeUPER(bits);
        bits = revision.decodeUPER(bits);
        bits = status.decodeUPER(bits);

        if (intersectionStateOptionalBits.charAt(2) == '1') {

            moy = new MinuteOfTheYear();
            bits = moy.decodeUPER(bits);
        }

        if (intersectionStateOptionalBits.charAt(3) == '1') {

            timeStamp = new DSecond();
            bits = timeStamp.decodeUPER(bits);
        }

        if (intersectionStateOptionalBits.charAt(4) == '1') {

            enabledLanes = new EnabledLaneList();
            bits = enabledLanes.decodeUPER(bits);
        }

        bits = states.decodeUPER(bits);

        if (intersectionStateOptionalBits.charAt(5) == '1') {

            maneuverAssistList = new ManeuverAssistList();
            bits = maneuverAssistList.decodeUPER(bits);
        }

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, revision, status, moy, timeStamp, enabledLanes, states, maneuverAssistList);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof IntersectionState)) {

            return false;
        }
        IntersectionState frame = (IntersectionState)object;
        return this.id.equals(frame.id)
                && this.revision.equals(frame.revision)
                && this.status.equals(frame.status)
                && Objects.equals(this.moy, frame.moy)
                && Objects.equals(this.timeStamp, frame.timeStamp)
                && Objects.equals(this.enabledLanes, frame.enabledLanes)
                && this.states.equals(frame.states)
                && Objects.equals(this.maneuverAssistList, frame.maneuverAssistList);
    }
}
