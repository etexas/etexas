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

package org.etexascode.webapp.ra;

import java.io.Serializable;

/**
 * An object used to pass data back from the simulation.
 * 
 * @author bbadillo
 */

public class SimDriverData implements Serializable {

    /** The simulation time. */
    private double simTime;

    /** The simulation messages. */
    private Object[] messages;

    /** Variable to identify if the data if finished */
    private boolean finished;

    /**
     * Gets the simulation time.
     * 
     * @return The simulation time.
     */
    public double getSimTime() {
        return simTime;
    }

    /**
     * Sets the simulation time.
     * 
     * @param simTime The simulation time to set.
     */
    public void setSimTime(double simTime) {
        this.simTime = simTime;
    }

    /**
     * Gets the simulation messages.
     * 
     * @return The messages from the simulation.
     */
    public Object[] getMessages() {
        return messages == null ? null : messages.clone();
    }

    /**
     * Sets messages for the simulation.
     * 
     * @param messages The messages to set for the simulation.
     */
    public void setMessages(Object[] messages) {
        this.messages = messages == null ? null : messages;
    }

    /**
     * Checks to see if the simulation data is finished.
     * 
     * @return True/False the simulation data is finished.
     */
    public boolean isFinished() {
        return finished;
    }

    /**
     * Sets whether the simulation data is finished.
     * 
     * @param finished True/False the simulation data is finished.
     */
    public void setFinished(boolean finished) {
        this.finished = finished;
    }
}
