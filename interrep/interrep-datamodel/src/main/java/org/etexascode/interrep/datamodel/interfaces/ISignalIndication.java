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

import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;

/**
 * Interface for merging the mutable and immutable models.
 * 
 * @author jrutherford
 */
public interface ISignalIndication extends Serializable {

    /**
     * Gets the lane ID.
     * 
     * @return The lane ID.
     */
    public int getLaneId();

    /**
     * Gets the color indication.
     * 
     * @return The color indication.
     */
    public Color getColorIndication();

    /**
     * Gets the type indication.
     * 
     * @return The type indication.
     */
    public Type getTypeIndication();

    /**
     * Gets the state indication.
     * 
     * @return The state indication.
     */
    public State getStateIndication();

    /**
     * Gets the time to change.
     * 
     * @return The time to change.
     */
    public double getTimeToChange();
}