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

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Interface for merging the mutable and immutable models.
 * 
 * @author jrutherford
 */
public interface ILane extends IDistanceable, IDable, Serializable, Iterable<ILaneNode> {

    /**
     * Gets the approach ID.
     * 
     * @return The approach ID.
     */
    public int getApproachId();

    /**
     * Gets the lane geometry list.
     * 
     * @return The lane geometry list.
     */
    public List<? extends ILaneNode> getLaneGeomList();

    /**
     * Gets the lane ID.
     * 
     * @return The lane ID.
     */
    public int getLaneId();

    /**
     * Gets the intersection ID.
     * 
     * @return The intersection ID.
     */
    public int getIntersectionId();

    /**
     * Gets the lane movements.
     * 
     * @return The map of lane movements.
     */
    public Map<Integer, ? extends ILaneMovement> getLaneMovements();

    /**
     * Gets the speed limit of the lane (m/s).
     * 
     * @return The speed limit in m/s.
     */
    public double getSpeedLimitInMetersPerSecond();

    /**
     * Gets the type of lane.
     * 
     * @return The type of lane.
     */
    public String getType();

    /**
     * Special iterator to get the lane movements
     * 
     * @return Lane movement iterator
     */
    public Iterable<? extends ILaneMovement> lanMovIterator();
}