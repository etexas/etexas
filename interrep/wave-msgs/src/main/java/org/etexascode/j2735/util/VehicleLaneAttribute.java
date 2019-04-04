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
 * A set of bit masks to represent a vehicle lane attribute as an integer value
 * 
 * @author bbadillo
 */
public class VehicleLaneAttribute {

    static public final int NO_LANE_DATA = 0;

    static public final int EGRESS_PATH = 1;

    static public final int MANEUVER_STRAIGHT_ALLOWED = 2;

    static public final int MANEUVER_LEFT_ALLOWED = 4;

    static public final int MANEUVER_RIGHT_ALLOWED = 8;

    static public final int YIELD = 16;

    static public final int MANEUVER_NO_UTURN = 32;

    static public final int MANEUVER_NO_TURN_ON_RED = 64;

    static public final int MANEUVER_NO_STOP = 128;

    static public final int NO_STOP = 256;

    static public final int NO_TURN_ON_RED = 512;

    static public final int HOV_LANE = 1024;

    static public final int BUS_ONLY = 2048;

    static public final int BUS_AND_TAXI_ONLY = 4096;

    static public final int MANEUVER_HOV_LANE = 8192;

    static public final int MANEUVER_SHARED_LANE = 16384;

    static public final int MANEUVER_BIKE_LANE = 32768;

}
