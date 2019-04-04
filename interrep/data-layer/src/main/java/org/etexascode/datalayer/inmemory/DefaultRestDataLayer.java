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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.etexascode.appslayerdata.RemoteProxyApp;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IRestDataLayer;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;

/**
 * The default REST data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultRestDataLayer implements IRestDataLayer {

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /**
     * Creates a new <code>DefaultRestDataLayer</code> with the specified components.
     * 
     * @param devicesComponent The devices component for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     */
    public DefaultRestDataLayer(IDevicesComponent devicesComponent, ITemporalComponent temporalComponent) {

        this.devicesComponent = devicesComponent;
        this.temporalComponent = temporalComponent;
    }

    @Override
    public Iterable<IDeviceData> getStartedAppDevices(int stepNum) {

        return devicesComponent.getNewDevices(stepNum);
    }

    @Override
    public List<RemoteProxyApp<?>> getStartedRemoteApps(int timestep) {

        Iterable<IDeviceData> start = getStartedAppDevices(timestep);
        List<RemoteProxyApp<?>> started = new ArrayList<RemoteProxyApp<?>>();
        Iterator<IDeviceData> iter = start.iterator();

        while (iter.hasNext()) {

            IDeviceData idd = iter.next();
            List<IConnectedVehicleApp<?>> apps = idd.getApps();

            for (IConnectedVehicleApp<?> app : apps) {

                if (app instanceof RemoteProxyApp) {

                    started.add((RemoteProxyApp<?>)app);
                }
            }
        }

        return started;
    }

    @Override
    public Iterable<IDeviceData> getStoppedAppDevices(int stepNum) {

        return devicesComponent.getStoppedDevices(stepNum);
    }

    @Override
    public List<RemoteProxyApp<?>> getStoppedRemoteApps(int timestep) {

        Iterable<IDeviceData> stop = getStoppedAppDevices(timestep);
        List<RemoteProxyApp<?>> stopped = new ArrayList<RemoteProxyApp<?>>();
        Iterator<IDeviceData> iter = stop.iterator();

        while (iter.hasNext()) {

            IDeviceData idd = iter.next();
            List<IConnectedVehicleApp<?>> apps = idd.getApps();

            for (IConnectedVehicleApp<?> app : apps) {

                if (app instanceof RemoteProxyApp) {

                    stopped.add((RemoteProxyApp<?>)app);
                }
            }
        }

        return stopped;
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }
}
