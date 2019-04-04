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
package org.etexascode.datalayer.interfaces;

import java.awt.Polygon;
import java.util.Collection;

import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;

/**
 * Data Layer for the Wave Sim
 * 
 * @author ablatt
 */
public interface IWaveSimDataLayer {

    /**
     * Get the Tx Iterable (the inputs to the Wave Sim)
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param segment The polygon of space which the Wave Sim is looking at
     * @return The inputs to the Wave Sim
     */
    public Iterable<Tx> getTxs(int stepNum, Polygon segment);

    /**
     * Add the messages to the Data Layer (this is the outputs of the Wave Sim)
     * 
     * @param stepNum The step number which the Data Layer should use
     * @param nodes The nodes.
     */
    public void putMessages(int stepNum, Collection<Rx> nodes);

    /**
     * Get the current time of the simulation
     * 
     * @param stepNum The step number which the Data Layer should use
     * @return The time in the simulation (in seconds)
     */
    public double getSimTime(int stepNum);

    /**
     * The step size being used by this simulation
     * 
     * @return The step size being used by this simulation
     */
    public double getStepSize();
}
