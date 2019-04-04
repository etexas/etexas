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
package org.etexascode.appslayer.remote;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.appslayerdata.RemoteProxyApp;

/**
 * Remote Application Manager. TODO ttevendale 1/2/2018 Add unit tests when we have remote
 * applications working again
 * 
 * @author jrutherford
 */
public class RemoteAppManager implements IRemoteAppManager {

    /** Map of all applications. */
    Map<String, RemoteProxyApp<?>> applications = new HashMap<String, RemoteProxyApp<?>>();

    /** Started applications for the current timestep. */
    List<String> startedApps;

    /** Stopped applications for the current timestep. */
    List<String> stoppedApps;

    /**
     * Adds the started apps to the list.
     * 
     * @param newApps The list of started apps.
     */
    @Override
    public void putStartedApps(List<RemoteProxyApp<?>> newApps) {
        startedApps = new ArrayList<String>();
        for (RemoteProxyApp<?> app : newApps) {
            startedApps.add(app.getProperId());
            applications.put(app.getProperId(), app);
        }
    }

    /**
     * Removes the shutdown apps from the list.
     * 
     * @param shutdownApps The list of shutdown apps.
     */
    @Override
    public void putFinishedApps(List<RemoteProxyApp<?>> shutdownApps) {
        stoppedApps = new ArrayList<String>();
        for (RemoteProxyApp<?> app : shutdownApps) {
            stoppedApps.add(app.getProperId());
            applications.remove(app.getProperId());
        }
    }

    /**
     * Gets the remote application.
     * 
     * @param properId The proper ID.
     * @return The AbstractRemoteApp.
     */
    @Override
    public RemoteProxyApp<?> getRemoteApp(String properId) {
        return applications.get(properId);
    }

    /**
     * Gets the list of started applications.
     * 
     * @return List of started application IDs.
     */
    @Override
    public Collection<String> getStartedApps() {
        return startedApps;
    }

    /**
     * Gets the list of applications currently active.
     * 
     * @return List of active application IDs.
     */
    @Override
    public Collection<String> getActiveApps() {
        return applications.keySet();
    }

    /**
     * Gets the list of stopped applications.
     * 
     * @return List of stopped application IDs.
     */
    @Override
    public Collection<String> getStoppedApps() {
        return stoppedApps;
    }
}
