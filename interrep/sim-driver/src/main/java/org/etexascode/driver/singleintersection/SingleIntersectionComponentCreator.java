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
package org.etexascode.driver.singleintersection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.appshutdownlayer.IAppShutdown;
import org.etexascode.appshutdownlayer.SimpleAppShutdown;
import org.etexascode.appslayer.IAppLayer;
import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.appslayer.IRemoteAppLayer;
import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.appslayer.remote.RemoteAppManager;
import org.etexascode.appslayer.singleintersection.SingleIntersectionAppLayer;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.datalayer.inmemory.DefaultVehicleIdComponent;
import org.etexascode.datalayer.inmemory.SingleIntersectionDataLayer;
import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.driver.ComponentContainer;
import org.etexascode.driver.IComponentCreator;
import org.etexascode.interrep.IInterRep;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.InterRepCoordinator;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.vehiclelocationmanager.IVehicleLocationManager;
import org.etexascode.vehiclelocationmanager.IVehicleLocationManagerCoordinator;
import org.etexascode.vehiclelocationmanager.singleintersection.InMemParallelLayerLocationManager;
import org.etexascode.vehiclelocationmanager.singleintersection.VehicleLocationManagerImpl;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.IMultiWaveSim;
import org.etexascode.wavesim.IWaveSim;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.WaveSimLayer;
import org.etexascode.wavesim.WaveSimType;
import org.etexascode.wavesim.idealized.IdealizedWaveSim;
import org.etexascode.wavesim.idealized.PacketAlwaysMakesIt;
import org.etexascode.wavesim.idealized.PacketThereNextTimeStep;
import org.etexascode.wavesim.ns3.WaveSimNS3Impl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Creates the components necessary to execute a single intersection with an memory data model.
 * 
 * @author ablatt
 */
public class SingleIntersectionComponentCreator implements IComponentCreator {

    /**
     * Static logger
     */
    @SuppressWarnings("unused")
    private static final Logger LOGGER = LoggerFactory.getLogger(SingleIntersectionComponentCreator.class);

    @Override
    public ComponentContainer build(Map<Long, SimulatorInterface> simulatorMap, Map<Long, ExecMetaData> metaDataMap, List<IDeviceData> deviceData, List<AppInitConfig> dsrcConfs,
            List<AppInitConfig> cellConfs, CellularConfig cellularConfiguration, List<CellTower> cellTowers, ITopography topography, WaveSimType waveSimType, INativeAppManager nativeAppManager,
            IPersistenceLayer persistenceLayer, Map<String, String> jndiParams) {

        int randomSeed = 42;
        IDataLayer data = new SingleIntersectionDataLayer(deviceData, persistenceLayer, randomSeed, new DefaultVehicleIdComponent());
        List<IInterRep> interReps = new ArrayList<IInterRep>(simulatorMap.size());

        for (Entry<Long, SimulatorInterface> simulatorEntry : simulatorMap.entrySet()) {

            interReps.add(new InterRep(simulatorEntry.getKey().intValue(), simulatorEntry.getValue(), metaDataMap.get(simulatorEntry.getKey()), data));
        }

        InterRepCoordinator irc = new InterRepCoordinator(interReps, data);
        ((SingleIntersectionDataLayer)data).setStepSize(irc.getMinStepSize());

        IVehicleLocationManager ivlm = new VehicleLocationManagerImpl(0, data, dsrcConfs, cellConfs, new HashMap<DualIntIdentifier, DualIntIdentifier>());
        List<IVehicleLocationManager> locMans = new ArrayList<IVehicleLocationManager>(1);
        locMans.add(ivlm);
        IVehicleLocationManagerCoordinator vlm = new InMemParallelLayerLocationManager(locMans);

        List<IWaveSim> waveSims = new ArrayList<IWaveSim>(1);
        if (waveSimType == WaveSimType.IDEALIZED) {

            waveSims.add(new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), data, topography, PropagationLossModel.URBAN));
        }
        else if (waveSimType == WaveSimType.NS3) {

            waveSims.add(new WaveSimNS3Impl(null, data, jndiParams, topography, cellTowers, cellularConfiguration, PropagationLossModel.URBAN));
        }
        else {

            throw new AssertionError(waveSimType.getName());
        }

        IRemoteAppManager ram = new RemoteAppManager();
        IMultiWaveSim wave = new WaveSimLayer(waveSims);
        SingleIntersectionAppLayer sial = new SingleIntersectionAppLayer(data, ram, nativeAppManager);
        IAppLayer apps = sial;
        IRemoteAppLayer ral = sial;
        IAppShutdown shutdownLayer = new SimpleAppShutdown(data);

        ComponentContainer cc = new ComponentContainer(data, irc, vlm, wave, apps, ral, ram, nativeAppManager, shutdownLayer);

        return cc;
    }
}
