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

import java.util.Set;

/**
 * Interface which marks that an MOE calculator tracks metrics per lane.
 * 
 * @author bbadillo
 */
public interface ILaneMOECalc {

    /**
     * Register a lane for MOE calculation.
     * 
     * @param laneId The ID of the lane to register.
     */
    public void registerLane(int laneId);

    /**
     * Get the lanes that have been registered for MOE calculation.
     * 
     * @return A set of lane IDs that have been registered for MOE calculation.
     */
    public Set<Integer> getLanes();

    /**
     * Get all possible MOE outputs for lanes.
     * 
     * @return A set of all possible MOE outputs for lanes.
     */
    public Set<String> getPossibleLaneOutputs();

    /**
     * Get the enabled MOE outputs for each lane.
     * 
     * @return A set of enabled MOE outputs for each lane.
     */
    public Set<String> getEnabledLaneOutputs();
}
