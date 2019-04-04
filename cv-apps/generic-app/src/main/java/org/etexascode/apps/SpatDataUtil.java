/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.apps;

import java.util.LinkedList;
import java.util.List;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.IntersectionState;
import org.etexascode.j2735.IntersectionState.States;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.util.SignalLightState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A utility class for binding SPAT data to Intellidrive API data
 * 
 * @author bbadillo
 * @author ablatt
 */
public class SpatDataUtil {

    /**
     * private final static logger
     */
    private final static Logger LOGGER = LoggerFactory.getLogger(SpatDataUtil.class);

    /**
     * Take a long integer value that represents a signal light state from SAE J2735 and create a
     * Signal Indication representation.
     * 
     * @param movementId A unique numerical identifier for the lane represented.
     * @param timeToChange The time remaining (in seconds) for the current signal.
     * @param signalLightState A long integer value that represents a signal light state from SAE
     *        J2735.
     * @return A signal indication with the correct values.
     */
    static public SignalIndication convertLongToSignalIndication(int movementId, double timeToChange, Long signalLightState) {
        SignalIndication.Color color;
        SignalIndication.Type type;
        SignalIndication.State state;

        color = SignalIndication.Color.NONE;
        type = SignalIndication.Type.BALL;
        state = SignalIndication.State.STEADY;

        // Checks for powers of 2. See SignalLightState.BALL_GREEN = 1L, BALL_YELLOW = 2L, ....
        // 1024L ....
        if ((signalLightState & SignalLightState.BALL_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.BALL;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.BALL_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.BALL;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.BALL_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.BALL;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.BALL_FLASHING) != 0) {
            type = SignalIndication.Type.BALL;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.LEFT_ARROW_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.LEFT_ARROW_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.LEFT_ARROW_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.LEFT_ARROW_FLASHING) != 0) {
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.RIGHT_ARROW_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.RIGHT_ARROW_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.RIGHT_ARROW_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.RIGHT_ARROW_FLASHING) != 0) {
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.STRAIGHT_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.STRAIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.STRAIGHT_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.STRAIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.STRAIGHT_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.STRAIGHT_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.STRAIGHT_FLASHING) != 0) {
            type = SignalIndication.Type.STRAIGHT_ARROW;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.SOFT_LEFT_ARROW_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_LEFT_ARROW_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_LEFT_ARROW_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_LEFT_ARROW_FLASHING) != 0) {
            type = SignalIndication.Type.LEFT_ARROW;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.SOFT_RIGHT_ARROW_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_RIGHT_ARROW_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_RIGHT_ARROW_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.SOFT;
        }
        if ((signalLightState & SignalLightState.SOFT_RIGHT_ARROW_FLASHING) != 0) {
            type = SignalIndication.Type.RIGHT_ARROW;
            state = SignalIndication.State.FLASHING;
        }
        if ((signalLightState & SignalLightState.UTURN_ARROW_GREEN) != 0) {
            color = SignalIndication.Color.GREEN;
            type = SignalIndication.Type.UTURN_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.UTURN_ARROW_YELLOW) != 0) {
            color = SignalIndication.Color.YELLOW;
            type = SignalIndication.Type.UTURN_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.UTURN_ARROW_RED) != 0) {
            color = SignalIndication.Color.RED;
            type = SignalIndication.Type.UTURN_ARROW;
            state = SignalIndication.State.STEADY;
        }
        if ((signalLightState & SignalLightState.UTURN_ARROW_FLASHING) != 0) {
            type = SignalIndication.Type.UTURN_ARROW;
            state = SignalIndication.State.FLASHING;
        }

        SignalIndication ret = new SignalIndication();

        ret.setLaneId(movementId);
        ret.setTimeToChange(timeToChange);
        ret.setColorIndication(color);
        ret.setTypeIndication(type);
        ret.setStateIndication(state);

        return ret;
    }

    /**
     * Extract data about traffic lanes (such as the current signal and the time to signal change)
     * from a SAE J2735 SPAT message java object.
     * 
     * @param spatData A SAE J2735 SPAT message java object.
     * @return A map of traffic lane signal and movement information keyed by the movement
     *         identifier.
     */
    public static List<SignalIndication> getSignalInfoFromSpat(SPAT spatData) {
        // Initialize the return map.
        List<SignalIndication> signalInfoFromSpat = new LinkedList<SignalIndication>();

        // Drill down into the message object.
        SPAT.IntersectionStates intersections = spatData.getIntersectionStates();
        for (IntersectionState state : intersections.getIntersectionState()) {
            States states = state.getStates();

            for (MovementState currMovementState : states.getMovementState()) {
                // Iterate over the lane identifiers represented by this lane signal and movement
                // information.
                byte[] laneSet = currMovementState.getLaneSet();
                for (byte currLaneId : laneSet) {
                    /**
                     * Spat messages use unsigned bytes (max 255), so convert signed java byte to
                     * unsigned value
                     */
                    int laneId = currLaneId & 0xFF;
                    LOGGER.debug("The lane id: {}", laneId);
                    // Create an Intellidrive API object with the data and put in the return map.
                    SignalIndication movement = convertLongToSignalIndication(laneId, UtilsUnitConversion.convertOneTenthSecondsToSeconds(currMovementState.getTimeToChange()),
                            currMovementState.getCurrState());
                    signalInfoFromSpat.add(movement);
                }
            }
        }

        // After everything is said and done, return the obtained information.
        return signalInfoFromSpat;
    }
}
