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
package org.etexascode.wavesim;

import java.util.Collections;
import java.util.List;

/**
 * Layer for executing all the wave simulations.
 * 
 * @author ablatt
 * @author ttevendale
 */
public class WaveSimLayer implements IMultiWaveSim {

    /**
     * The maximum DSRC message distance for an open environment (cm).
     */
    public static final int MAX_DSRC_MESSAGE_DISTANCE_OPEN = 200000;

    /**
     * The maximum DSRC message distance for an suburban environment (cm).
     */
    public static final int MAX_DSRC_MESSAGE_DISTANCE_SUBURBAN = 17500;

    /**
     * The maximum DSRC message distance for an urban environment (cm).
     */
    public static final int MAX_DSRC_MESSAGE_DISTANCE_URBAN = 15000;

    /**
     * The wave simulations to execute.
     */
    public final List<IWaveSim> sims;

    /**
     * Constructor
     * 
     * @param sims Simulations to execute
     */
    public WaveSimLayer(List<IWaveSim> sims) {
        this.sims = Collections.unmodifiableList(sims);
    }

    /**
     * Transmit messages in the wave sim for the given step number
     * 
     * @param stepNum the step number of the simulation
     */
    @Override
    public void waveSim(int stepNum) {
        for (IWaveSim ws : sims) {
            ws.transmitMessages(stepNum);
        }
    }
}
