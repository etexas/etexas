/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.driver.singleintersection;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.appshutdownlayer.SimpleAppShutdown;
import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.appslayer.remote.RemoteAppManager;
import org.etexascode.appslayer.singleintersection.SingleIntersectionAppLayer;
import org.etexascode.datalayer.IDataLayer;
import org.etexascode.datalayer.inmemory.DefaultDataLayer;
import org.etexascode.devicedata.AppInitConfig;
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
import org.etexascode.vehiclelocationmanager.singleintersection.InMemParallelLayerLocationManager;
import org.etexascode.vehiclelocationmanager.singleintersection.VehicleLocationManagerImpl;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.IWaveSim;
import org.etexascode.wavesim.PropagationLossModel;
import org.etexascode.wavesim.WaveSimLayer;
import org.etexascode.wavesim.WaveSimType;
import org.etexascode.wavesim.idealized.IdealizedWaveSim;
import org.etexascode.wavesim.idealized.PacketAlwaysMakesIt;
import org.etexascode.wavesim.idealized.PacketThereNextTimeStep;
import org.etexascode.wavesim.ns3.WaveSimNS3Impl;

/**
 * The default component creator for a simulation.
 * 
 * @author ablatt
 * @author emyers
 * @author ttevendale
 */
public class DefaultComponentCreator implements IComponentCreator {

    @Override
    public ComponentContainer build(Map<Long, SimulatorInterface> simulatorMap, Map<Long, ExecMetaData> metaDataMap, List<IDeviceData> deviceData, List<AppInitConfig> dsrcConfs,
            List<AppInitConfig> cellConfs, CellularConfig cellularConfiguration, List<CellTower> cellTowers, ITopography topography, WaveSimType waveSimType, INativeAppManager nativeAppManager,
            IPersistenceLayer persistenceLayer, Map<String, String> jndiParams) {

        IDataLayer data = new DefaultDataLayer(deviceData, persistenceLayer, 42);
        List<IInterRep> interReps = new ArrayList<IInterRep>(simulatorMap.size());
        List<IVehicleLocationManager> locationManagers = new ArrayList<IVehicleLocationManager>(simulatorMap.size());

        PropagationLossModel propagationLossModel = null;

        for (Entry<Long, SimulatorInterface> simulatorEntry : simulatorMap.entrySet()) {

            interReps.add(new InterRep(simulatorEntry.getKey().intValue(), simulatorEntry.getValue(), metaDataMap.get(simulatorEntry.getKey()), data));
            locationManagers.add(new VehicleLocationManagerImpl(simulatorEntry.getKey().intValue(), data, dsrcConfs, cellConfs, metaDataMap.get(simulatorEntry.getKey()).getLaneMap()));
            propagationLossModel = metaDataMap.get(simulatorEntry.getKey()).getPropagationLossModel();
        }

        InterRepCoordinator irc = new InterRepCoordinator(interReps, data);
        ((DefaultDataLayer)data).setStepSize(irc.getMinStepSize());

        List<IWaveSim> waveSims = new ArrayList<IWaveSim>(1);

        if (waveSimType == WaveSimType.IDEALIZED) {

            waveSims.add(new IdealizedWaveSim(null, new PacketAlwaysMakesIt(), new PacketThereNextTimeStep(), data, topography, propagationLossModel));
        }
        else if (waveSimType == WaveSimType.NS3) {

            waveSims.add(new WaveSimNS3Impl(null, data, jndiParams, topography, cellTowers, cellularConfiguration, propagationLossModel));
        }
        else {

            throw new AssertionError(waveSimType.getName());
        }

        IRemoteAppManager ram = new RemoteAppManager();
        SingleIntersectionAppLayer sial = new SingleIntersectionAppLayer(data, ram, nativeAppManager);
        return new ComponentContainer(data, irc, new InMemParallelLayerLocationManager(locationManagers), new WaveSimLayer(waveSims), sial, sial, ram, nativeAppManager, new SimpleAppShutdown(data));
    }
}
