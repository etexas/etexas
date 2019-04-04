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

package org.etexascode.interrep.datamodel.interfaces;

import java.util.Map;
import java.util.Set;

/**
 * Interface for merging the mutable and immutable models.
 * 
 * @author jrutherford
 */
public interface ILaneManager extends IDable, Iterable<ILane> {

    /**
     * Gets the elevation.
     * 
     * @return The elevation.
     */
    public double getElevation();

    /**
     * Gets the lane by ID.
     * 
     * @param <T> The type that extends the ILane interface.
     * @param id The ID.
     * @return The lane.
     */
    public <T extends ILane> T getLaneById(int id);

    /**
     * Gets the map of lanes.
     * 
     * @param <T> The type that extends the ILane interface.
     * @return The map of lanes.
     */
    public <T extends ILane> Map<Integer, T> getLanes();

    /**
     * Gets the latitude.
     * 
     * @return The latitude.
     */
    public double getLatitude();

    /**
     * Gets the longitude.
     * 
     * @return The longitude.
     */
    public double getLongitude();

    /**
     * Get the type of Calculator being used by the simulation.
     * 
     * @return the type of Calculator.
     */
    public int getGeoCalculatorType();

    /**
     * Getter.
     * 
     * @return Get the id of the intersection.
     */
    public int getIntersectionId();

    /**
     * Gets the ids of all the lanes in this lane manager. Note: This might need to be turned into a
     * set of Strings as we move to multi-intersection...
     * 
     * @return The ids of the lane.
     */
    public Set<Integer> getLaneIds();
}