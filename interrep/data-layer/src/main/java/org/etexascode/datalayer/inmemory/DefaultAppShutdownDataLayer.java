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
package org.etexascode.datalayer.inmemory;

import java.util.LinkedList;
import java.util.List;

import org.etexascode.appslayerdata.AppShutdownLayerInput;
import org.etexascode.datalayer.interfaces.IAppShutdownDataLayer;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.LogData;
import org.etexascode.persistencelayer.IPersistenceLayer;

/**
 * The default application shutdown data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultAppShutdownDataLayer implements IAppShutdownDataLayer {

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /** The MAC manager component. */
    private IMacManagerComponent macManagerComponent;

    /** The persistence layer. */
    private IPersistenceLayer persistenceLayer;

    /**
     * Creates a new <code>DefaultAppShtudownDataLayer</code> with the specified components.
     * 
     * @param devicesComponent The devices component for this data layer.
     * @param macManagerComponent The MAC manager component for this data layer.
     * @param persistenceLayer The persistence layer for this data layer.
     */
    public DefaultAppShutdownDataLayer(IDevicesComponent devicesComponent, IMacManagerComponent macManagerComponent, IPersistenceLayer persistenceLayer) {

        this.devicesComponent = devicesComponent;
        this.macManagerComponent = macManagerComponent;
        this.persistenceLayer = persistenceLayer;
    }

    @Override
    public void putAppLogs(int stepNum, List<LogData> logs) {

        persistenceLayer.persistAppLogs(logs);
    }

    @Override
    public Iterable<AppShutdownLayerInput> getAppsToShutdown(int stepNum) {

        LinkedList<AppShutdownLayerInput> ret = new LinkedList<AppShutdownLayerInput>();

        for (IDeviceData idd : devicesComponent.getStoppedDevices(stepNum)) {

            for (IConnectedVehicleApp<?> ica : idd.getApps()) {

                ret.add(new AppShutdownLayerInput(macManagerComponent.getMac(0, idd.getProperId()), ica));
            }
        }

        return ret;
    }

    @Override
    public Iterable<AppShutdownLayerInput> getAllAppsForShutdown() {

        LinkedList<AppShutdownLayerInput> ret = new LinkedList<AppShutdownLayerInput>();

        for (IDeviceData idd : devicesComponent.stopAllDevices()) {

            for (IConnectedVehicleApp<?> ica : idd.getApps()) {

                ret.add(new AppShutdownLayerInput(macManagerComponent.getMac(0, idd.getProperId()), ica));
            }
        }

        return ret;
    }
}
