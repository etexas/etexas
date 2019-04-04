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
package org.etexascode.appslayer;

import java.util.Collection;
import java.util.List;

import org.etexascode.appslayerdata.RemoteProxyApp;

/**
 * Interface for the remote manager.
 * 
 * @author jrutherford
 */
public interface IRemoteAppManager {

    /**
     * Adds the started apps to the list.
     * 
     * @param newApps The list of started apps.
     */
    public void putStartedApps(List<RemoteProxyApp<?>> newApps);

    /**
     * Removes the shutdown apps from the list.
     * 
     * @param shutdownApps The list of shutdown apps.
     */
    public void putFinishedApps(List<RemoteProxyApp<?>> shutdownApps);

    /**
     * Gets the remote application.
     * 
     * @param properId The proper ID.
     * @return The AbstractRemoteApp.
     */
    public RemoteProxyApp<?> getRemoteApp(String properId);

    /**
     * Gets the list of started applications.
     * 
     * @return List of started application IDs.
     */
    public Collection<String> getStartedApps();

    /**
     * Gets the list of applications currently active.
     * 
     * @return List of active application IDs.
     */
    public Collection<String> getActiveApps();

    /**
     * Gets the list of stopped applications.
     * 
     * @return List of stopped application IDs.
     */
    public Collection<String> getStoppedApps();
}
