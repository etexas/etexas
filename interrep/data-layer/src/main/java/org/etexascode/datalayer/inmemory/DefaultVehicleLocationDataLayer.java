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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.datalayer.interfaces.ICommandsComponent;
import org.etexascode.datalayer.interfaces.IDevicesComponent;
import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.datalayer.interfaces.IMacManagerComponent;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IVehicleLocationDataLayer;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.persistencelayer.IPersistenceLayer;

/**
 * The default vehicle location data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultVehicleLocationDataLayer implements IVehicleLocationDataLayer {

    /** The dual integer identifier. */
    private DualIntIdentifier dualIdentifier;

    /** The commands component. */
    private ICommandsComponent commandsComponent;

    /** The devices component. */
    private IDevicesComponent devicesComponent;

    /** The intersection models component. */
    private IIntersectionModelsComponent modelsComponent;

    /** The random seed number. */
    private int randomSeed;

    /** The MAC manager component. */
    private IMacManagerComponent macManagerComponent;

    /** The persistence layer. */
    private IPersistenceLayer persistenceLayer;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /** The vehicle ID component. */
    private IVehicleIdComponent vehicleIdComponent;

    /**
     * Creates a new <code>DefaultVehicleLocationDataLayer</code> with the specified components.
     * 
     * @param commandsComponent The commands component for this data layer.
     * @param devicesComponent The devices component for this data layer.
     * @param modelsComponent The intersection models component for this data layer.
     * @param macManagerComponent The MAC manager component for this data layer.
     * @param persistenceLayer The persistence layer for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     * @param seedNumber The integer random seed number.
     * @param vehicleIdComponent The vehicle ID component for this data layer.
     */
    public DefaultVehicleLocationDataLayer(ICommandsComponent commandsComponent, IDevicesComponent devicesComponent, IIntersectionModelsComponent modelsComponent,
            IMacManagerComponent macManagerComponent, IPersistenceLayer persistenceLayer, ITemporalComponent temporalComponent, int seedNumber, IVehicleIdComponent vehicleIdComponent) {

        dualIdentifier = new DualIntIdentifier(0, 0, true);
        this.commandsComponent = commandsComponent;
        this.devicesComponent = devicesComponent;
        this.modelsComponent = modelsComponent;
        this.macManagerComponent = macManagerComponent;
        this.persistenceLayer = persistenceLayer;
        this.temporalComponent = temporalComponent;
        this.randomSeed = seedNumber;
        this.vehicleIdComponent = vehicleIdComponent;
    }

    @Override
    public long addVehicleId(IVehicle vehicle) {

        return vehicleIdComponent.addVehicleId(vehicle);
    }

    @Override
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId) {

        commandsComponent.putVehicleInjection(stepNum, interRepId, request);
        persistenceLayer.persistVehicleInjectionRequests(source, interRepId, getSimTime(stepNum), Arrays.asList(request));
    }

    @Override
    public Long getGlobalVehicleId(String properId) {

        return vehicleIdComponent.getGlobalVehicleId(properId);
    }

    @Override
    public Set<String> getInjectedVehicles(int stepNum, int interRepId) {

        return new HashSet<String>();
    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleIdComponent.getProperVehicleId(globalId);
    }

    @Override
    public int getRandomSeed(int stepNum, int interRepId) {

        return randomSeed;
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }

    @Override
    public IVehicleManager getVehicleManagerInfo(int stepNum, int interRepId) {

        return modelsComponent.getVehicles(stepNum, interRepId);
    }

    @Override
    public boolean isStalledVehicle(IDable vehicle) {

        return vehicleIdComponent.isStalledVehicle(vehicle);
    }

    @Override
    public int nextInjectedVehicleId() {

        return vehicleIdComponent.nextInjectedVehicleId();
    }

    @Override
    public void putAppDeviceData(int stepNum, List<? extends IDeviceData> deviceData) {

        macManagerComponent.putNewDevices(stepNum, deviceData);
        devicesComponent.putNewDevices(stepNum, deviceData);
    }

    @Override
    public void putLoggedOutVehicles(int stepNum, Map<DualIntIdentifier, List<IVehicle>> logoutVehicles) {

        devicesComponent.putLogoutVehicles(stepNum, logoutVehicles.get(dualIdentifier));
    }

    @Override
    public void putRandomSeed(int stepNum, int interRepId, int randomSeed) {

        this.randomSeed = randomSeed;
    }

    @Override
    public void putReturnedVehicle(IDable vehicle) {

        vehicleIdComponent.putReturnedVehicle(vehicle);
        devicesComponent.putReturnedVehicle(vehicle);
    }

    @Override
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        vehicleIdComponent.putStalledVehicle(oldVehicle, newVehicle);
        List<String> oldDeviceIds = devicesComponent.getDeviceProperIds(oldVehicle);
        List<String> newDeviceIds = devicesComponent.putStalledVehicle(oldVehicle, newVehicle);
        for (int i = 0; i < oldDeviceIds.size(); i++) {

            String oldDeviceId = oldDeviceIds.get(i);
            String newDeviceId = newDeviceIds.get(i);
            macManagerComponent.updateMacAddress(oldDeviceId, newDeviceId);
        }
    }
}
