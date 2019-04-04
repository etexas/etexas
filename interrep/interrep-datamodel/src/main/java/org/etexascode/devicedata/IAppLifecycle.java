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

/**
 * Interface for Connected Vehicle Applications that need to perform actions on startup and/or
 * shutdown.
 * 
 * @author cdeisher
 */
public interface IAppLifecycle {

    /**
     * An initializer method for this app. Override this method if you want to perform actions prior
     * to the first update.
     * 
     * @param appConfigs the app configurations.
     */
    public void init(String[] appConfigs);

    /**
     * Performs actions before shutting down the app.
     * 
     * @param logger An app logger which can be used to store additional data.
     */
    public void appShutdown(AppLogger logger);

}
