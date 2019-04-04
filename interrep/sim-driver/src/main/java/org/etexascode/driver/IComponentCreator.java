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

import java.util.List;
import java.util.Map;

import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.persistencelayer.IPersistenceLayer;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.WaveSimType;

/**
 * Interface defining the creation of components for a specific simulation type.
 * 
 * @author ablatt
 * @author bbadillo
 */
public interface IComponentCreator {

    /**
     * Builds a component container using the specified simulation components.
     * 
     * @param simulatorMap The map of simulators for this simulation.
     * @param metaDataMap The map of meta data for this simulation.
     * @param deviceData The device data for this simulation.
     * @param dsrcConfs The DSRC application configurations for this simulation.
     * @param cellConfs The cellular application configurations for this simulation.
     * @param cellularConfiguration The cellular communication configuration for this simulation.
     * @param cellTowers The cellular communication towers for this simulation.
     * @param topography The topography for this simulation.
     * @param waveSimType The WAVE simulation type for this simulation.
     * @param nativeAppManager The native application manager for this simulation.
     * @param persistenceLayer The persistence layer for this simulation.
     * @param jndiParams The JNDI naming parameters for this simulation.
     * @return A component container for the specified simulation components.
     */
    public ComponentContainer build(Map<Long, SimulatorInterface> simulatorMap, Map<Long, ExecMetaData> metaDataMap, List<IDeviceData> deviceData, List<AppInitConfig> dsrcConfs,
            List<AppInitConfig> cellConfs, CellularConfig cellularConfiguration, List<CellTower> cellTowers, ITopography topography, WaveSimType waveSimType, INativeAppManager nativeAppManager,
            IPersistenceLayer persistenceLayer, Map<String, String> jndiParams);
}
