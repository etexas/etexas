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
package org.etexascode.test;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a signal change in the NGSim data.
 * 
 * @author janway
 */
public class NGSimSignals {

    /**
     * The timestep the signal changed at.
     */
    private int timestep;

    /**
     * The lanes controlled by this signal.
     */
    private List<Integer> laneIds = new ArrayList<Integer>();

    /**
     * The state the signal changed to.
     */
    private State state;

    /**
     * The previous signal change time.
     */
    private int prevTimeStep;

    /**
     * The possible states a signal can be in - color green, yellow, or red; movement straight or
     * left. NOTE: The data table for the signal data allows for right movements as well, but the
     * data has none.
     */
    public enum State {
        G_THRU,
        Y_THRU,
        R_THRU,
        G_LEFT,
        Y_LEFT,
        R_LEFT
    }

    /**
     * Constructor.
     * 
     * @param timestep The value for the timestep.
     * @param laneIds The list of laneIds for the signal.
     * @param state The state the signal changed to.
     * @param prevTimeStep The value for the previous timestep.
     */
    public NGSimSignals(int timestep, List<Integer> laneIds, State state, int prevTimeStep) {
        this.timestep = timestep;
        this.laneIds = laneIds;
        this.state = state;
        this.prevTimeStep = prevTimeStep;
    }

    /**
     * Getter
     * 
     * @return The time step.
     */
    public int getTimestep() {
        return timestep;
    }

    /**
     * Setter
     * 
     * @param timestep The new time step.
     */
    public void setTimestep(int timestep) {
        this.timestep = timestep;
    }

    /**
     * Getter
     * 
     * @return The lane ID's.
     */
    public List<Integer> getLaneIds() {
        return laneIds;
    }

    /**
     * Setter
     * 
     * @param laneIds The new lane ID's.
     */
    public void setLaneIds(List<Integer> laneIds) {
        this.laneIds = laneIds;
    }

    /**
     * Getter
     * 
     * @return The state.
     */
    public State getState() {
        return state;
    }

    /**
     * Setter
     * 
     * @param state The new state.
     */
    public void setState(State state) {
        this.state = state;
    }

    /**
     * Getter
     * 
     * @return The previous time step.
     */
    public int getPrevTimeStep() {
        return this.prevTimeStep;
    }

    /**
     * Setter
     * 
     * @param prevTimeStep The new time step.
     */
    public void setPrevTimeStep(int prevTimeStep) {
        this.prevTimeStep = prevTimeStep;
    }
}
