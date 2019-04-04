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

package org.etexascode.devicedata;

import java.util.Collection;

/**
 * Interface to Connected Vehicle applications.
 * 
 * @author bbadillo
 */
public interface IConnectedVehicleApp<T> {

    /**
     * The update function for the device. This function is meant to take in all relevant
     * information and return the list of messages the app should emit.
     * 
     * @param device The device the app is executing on.
     * @param messages The messages which the app is due to receive.
     * @param receive The list of message indications.
     * @param simTime The time in the simulation in seconds since the start of the simulation.
     * @param logger The logger the user can use to get data out of the app.
     */
    public abstract void performUpdate(T device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger);

}
