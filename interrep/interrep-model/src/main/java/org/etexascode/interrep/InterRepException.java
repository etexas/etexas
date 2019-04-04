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

package org.etexascode.interrep;

/**
 * Named exception for the intersection model.
 * 
 * @author dranker
 * @author bbadillo
 */
@SuppressWarnings("serial")
public class InterRepException extends Exception {

    /**
     * A message for exceptions expressing that a lane does not have associated geometry
     * information.
     */
    final static public String EX_NO_LANE_GEOMETRY = "Lane does not contain geometry.";

    /**
     * A message for exceptions expressing that map data information has not yet been received.
     */
    final static public String EX_NO_MAP_DATA = "MapData has not yet been received.";

    /**
     * A message for exceptions expressing that a calculator for a Geographical Coordinate Reference
     * System has not been set.
     */
    final static public String EX_NO_GEO_CALC = "Geographical Coordinate Reference System calculator has not been set.";

    /**
     * Constructor
     * 
     * @param message The message to use for the exception. This should be one of the public static
     *        members of this class.
     */
    public InterRepException(String message) {
        super(message);
    }
}
