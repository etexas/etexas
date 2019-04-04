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
package org.etexascode.driver;

import org.etexascode.appshutdownlayer.IAppShutdown;
import org.etexascode.appslayer.IAppLayer;
import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.appslayer.IRemoteAppLayer;
import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.interrep.IInterRepCoordinator;
import org.etexascode.vehiclelocationmanager.IVehicleLocationManagerCoordinator;
import org.etexascode.wavesim.IMultiWaveSim;

/**
 * Container for the components used by the driver
 * 
 * @author ablatt
 */
public class ComponentContainer {

    /**
     * The coordinator for the intersection simulations
     */
    public final IInterRepCoordinator interRepCoordinator;

    /**
     * The data layer to be used in this execution
     */
    public final IDataLayer data;

    /**
     * The vehicle location manager
     */
    public final IVehicleLocationManagerCoordinator locationManager;

    /**
     * The message simulation layer to use
     */
    public final IMultiWaveSim waveSim;

    /**
     * The app layer to be used in this simulation
     */
    public final IAppLayer appLayer;

    /**
     * The remote app layer to be used in this simulation.
     */
    public final IRemoteAppLayer remoteAppLayer;

    /**
     * The remote app manager.
     */
    public final IRemoteAppManager remoteAppManager;

    /**
     * The native app manager.
     */
    public final INativeAppManager nativeAppManager;

    /**
     * The app shutdown layer to be used
     */
    public final IAppShutdown shutdownLayer;

    /**
     * Constructor
     * 
     * @param data The dataLayer.
     * @param coord The inter representation coordinator.
     * @param locMan The location manager.
     * @param messLayer The wave simulation.
     * @param apps The app layer.
     * @param remApp The remote app layer.
     * @param remMan The remote app manager.
     * @param nativeAppManager The native app manager.
     * @param shutdownLayer The shutdown layer.
     */
    public ComponentContainer(IDataLayer data, IInterRepCoordinator coord, IVehicleLocationManagerCoordinator locMan, IMultiWaveSim messLayer, IAppLayer apps, IRemoteAppLayer remApp,
            IRemoteAppManager remMan, INativeAppManager nativeAppManager, IAppShutdown shutdownLayer) {
        this.data = data;
        interRepCoordinator = coord;
        locationManager = locMan;
        waveSim = messLayer;
        appLayer = apps;
        remoteAppLayer = remApp;
        remoteAppManager = remMan;
        this.nativeAppManager = nativeAppManager;
        this.shutdownLayer = shutdownLayer;
    }
}
