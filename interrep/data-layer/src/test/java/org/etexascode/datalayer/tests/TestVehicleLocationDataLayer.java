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
package org.etexascode.datalayer.tests;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.datalayer.inmemory.DefaultVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IVehicleLocationDataLayer;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

public class TestVehicleLocationDataLayer implements IVehicleLocationDataLayer {

    // Key: step num, Value: HashMap -> Key: intersection id, Value: Info model
    public Map<Integer, InterRepInfoModel> inputs = new HashMap<Integer, InterRepInfoModel>();

    public double simTime = 0.0;

    public IVehicleIdComponent vehicleIdComponent = new DefaultVehicleIdComponent();

    @Override
    public double getSimTime(int stepNum) {
        return simTime;
    }

    @Override
    public IVehicleManager getVehicleManagerInfo(int stepNum, int interRepId) {
        return null;
    }

    @Override
    public Set<String> getInjectedVehicles(int stepNum, int interRepId) {
        return null;
    }

    @Override
    public int getRandomSeed(int stepNum, int interRepId) {
        return 0;
    }

    @Override
    public void putRandomSeed(int stepNum, int interRepId, int randomSeed) {}

    @Override
    public void putAppDeviceData(int stepNum, List<? extends IDeviceData> deviceData) {}

    @Override
    public void putLoggedOutVehicles(int stepNum, Map<DualIntIdentifier, List<IVehicle>> logoutVehicles) {}

    @Override
    public void addVehicleInjectionRequest(String source, int stepNum, VehicleInjectionRequest request, int interRepId) {}

    @Override
    public int nextInjectedVehicleId() {

        return vehicleIdComponent.nextInjectedVehicleId();
    }

    @Override
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        vehicleIdComponent.putStalledVehicle(oldVehicle, newVehicle);
    }

    @Override
    public void putReturnedVehicle(IDable returnedVehicle) {

        vehicleIdComponent.putReturnedVehicle(returnedVehicle);
    }

    @Override
    public boolean isStalledVehicle(IDable vehicle) {

        return vehicleIdComponent.isStalledVehicle(vehicle);
    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleIdComponent.getProperVehicleId(globalId);
    }

    @Override
    public Long getGlobalVehicleId(String properId) {

        return vehicleIdComponent.getGlobalVehicleId(properId);
    }

    @Override
    public long addVehicleId(IVehicle vehicle) {

        return vehicleIdComponent.addVehicleId(vehicle);
    }
}
