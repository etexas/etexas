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

package org.etexascode.j2735.util;

/**
 * A set of bit masks to represent a signal light state as a long value
 * 
 * @author bbadillo
 */
public class SignalLightState {

    static public final long DARK = 0;

    static public final long BALL_GREEN = 1L;

    static public final long BALL_YELLOW = 2L;

    static public final long BALL_RED = 4L;

    static public final long BALL_FLASHING = 8L;

    static public final long LEFT_ARROW_GREEN = 16L;

    static public final long LEFT_ARROW_YELLOW = 32L;

    static public final long LEFT_ARROW_RED = 64L;

    static public final long LEFT_ARROW_FLASHING = 128L;

    static public final long RIGHT_ARROW_GREEN = 256L;

    static public final long RIGHT_ARROW_YELLOW = 512L;

    static public final long RIGHT_ARROW_RED = 1024L;

    static public final long RIGHT_ARROW_FLASHING = 2048L;

    static public final long STRAIGHT_GREEN = 4096L;

    static public final long STRAIGHT_YELLOW = 8192L;

    static public final long STRAIGHT_RED = 16384L;

    static public final long STRAIGHT_FLASHING = 32768L;

    static public final long SOFT_LEFT_ARROW_GREEN = 65536L;

    static public final long SOFT_LEFT_ARROW_YELLOW = 131072L;

    static public final long SOFT_LEFT_ARROW_RED = 262144L;

    static public final long SOFT_LEFT_ARROW_FLASHING = 524288L;

    static public final long SOFT_RIGHT_ARROW_GREEN = 1048576L;

    static public final long SOFT_RIGHT_ARROW_YELLOW = 2097152L;

    static public final long SOFT_RIGHT_ARROW_RED = 4194304L;

    static public final long SOFT_RIGHT_ARROW_FLASHING = 8388608L;

    static public final long UTURN_ARROW_GREEN = 16777216L;

    static public final long UTURN_ARROW_YELLOW = 33554432L;

    static public final long UTURN_ARROW_RED = 67108864L;

    static public final long UTURN_ARROW_FLASHING = 134217728L;

    /**
     * Take a long integer value that represents a signal light state from SAE J2735 and create a
     * textual representation.
     * 
     * @param signalLightState A long integer value that represents a signal light state from SAE
     *        J2735.
     * @return A textual representation of the signal light state.
     */
    static public String convertSignalLightStateToString(Long signalLightState) {
        StringBuilder sb = new StringBuilder();
        if (signalLightState == DARK) {
            return "No Signal";
        }
        if ((signalLightState & BALL_GREEN) != 0) {
            sb.append("BALL_GREEN, ");
        }
        if ((signalLightState & BALL_YELLOW) != 0) {
            sb.append("BALL_YELLOW, ");
        }
        if ((signalLightState & BALL_RED) != 0) {
            sb.append("BALL_RED, ");
        }
        if ((signalLightState & BALL_FLASHING) != 0) {
            sb.append("BALL_FLASHING, ");
        }
        if ((signalLightState & LEFT_ARROW_GREEN) != 0) {
            sb.append("LEFT_ARROW_GREEN, ");
        }
        if ((signalLightState & LEFT_ARROW_YELLOW) != 0) {
            sb.append("LEFT_ARROW_YELLOW, ");
        }
        if ((signalLightState & LEFT_ARROW_RED) != 0) {
            sb.append("LEFT_ARROW_RED, ");
        }
        if ((signalLightState & LEFT_ARROW_FLASHING) != 0) {
            sb.append("LEFT_ARROW_FLASHING, ");
        }
        if ((signalLightState & RIGHT_ARROW_GREEN) != 0) {
            sb.append("RIGHT_ARROW_GREEN, ");
        }
        if ((signalLightState & RIGHT_ARROW_YELLOW) != 0) {
            sb.append("RIGHT_ARROW_YELLOW, ");
        }
        if ((signalLightState & RIGHT_ARROW_RED) != 0) {
            sb.append("RIGHT_ARROW_RED, ");
        }
        if ((signalLightState & RIGHT_ARROW_FLASHING) != 0) {
            sb.append("RIGHT_ARROW_FLASHING, ");
        }
        if ((signalLightState & STRAIGHT_GREEN) != 0) {
            sb.append("STRAIGHT_GREEN, ");
        }
        if ((signalLightState & STRAIGHT_YELLOW) != 0) {
            sb.append("STRAIGHT_YELLOW, ");
        }
        if ((signalLightState & STRAIGHT_RED) != 0) {
            sb.append("STRAIGHT_RED, ");
        }
        if ((signalLightState & STRAIGHT_FLASHING) != 0) {
            sb.append("STRAIGHT_FLASHING, ");
        }
        if ((signalLightState & SOFT_LEFT_ARROW_GREEN) != 0) {
            sb.append("SOFT_LEFT_ARROW_GREEN, ");
        }
        if ((signalLightState & SOFT_LEFT_ARROW_YELLOW) != 0) {
            sb.append("SOFT_LEFT_ARROW_YELLOW, ");
        }
        if ((signalLightState & SOFT_LEFT_ARROW_RED) != 0) {
            sb.append("SOFT_LEFT_ARROW_RED, ");
        }
        if ((signalLightState & SOFT_LEFT_ARROW_FLASHING) != 0) {
            sb.append("SOFT_LEFT_ARROW_FLASHING, ");
        }
        if ((signalLightState & SOFT_RIGHT_ARROW_GREEN) != 0) {
            sb.append("SOFT_RIGHT_ARROW_GREEN, ");
        }
        if ((signalLightState & SOFT_RIGHT_ARROW_YELLOW) != 0) {
            sb.append("SOFT_RIGHT_ARROW_YELLOW, ");
        }
        if ((signalLightState & SOFT_RIGHT_ARROW_RED) != 0) {
            sb.append("SOFT_RIGHT_ARROW_RED, ");
        }
        if ((signalLightState & SOFT_RIGHT_ARROW_FLASHING) != 0) {
            sb.append("SOFT_RIGHT_ARROW_FLASHING, ");
        }
        if ((signalLightState & UTURN_ARROW_GREEN) != 0) {
            sb.append("UTURN_ARROW_GREEN, ");
        }
        if ((signalLightState & UTURN_ARROW_YELLOW) != 0) {
            sb.append("UTURN_ARROW_YELLOW, ");
        }
        if ((signalLightState & UTURN_ARROW_RED) != 0) {
            sb.append("UTURN_ARROW_RED, ");
        }
        if ((signalLightState & UTURN_ARROW_FLASHING) != 0) {
            sb.append("UTURN_ARROW_FLASHING, ");
        }

        String signalString = sb.substring(0, sb.lastIndexOf(","));
        return signalString;
    }
}
