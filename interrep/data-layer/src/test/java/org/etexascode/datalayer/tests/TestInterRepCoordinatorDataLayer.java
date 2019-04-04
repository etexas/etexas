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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.inmemory.DefaultVehicleIdComponent;
import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;
import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;

public class TestInterRepCoordinatorDataLayer implements IInterRepCoordinationDataLayer {

    // Key: step num, Value: HashMap -> Key: intersection id, Value: Info model
    public Map<Integer, Map<Integer, InterRepInfoModel>> outputs = new HashMap<Integer, Map<Integer, InterRepInfoModel>>();

    public List<SignalCommand> sigCommands = new ArrayList<SignalCommand>();

    public List<VehicleCommand> vehCommands = new ArrayList<VehicleCommand>();

    public List<VehicleInjectionRequest> requests = new ArrayList<VehicleInjectionRequest>();

    public IVehicleIdComponent vehicleIdComponent = new DefaultVehicleIdComponent();

    public double simTime = 0.0;

    @Override
    public void putInterRepModel(int stepNum, int intersectionId, InterRepInfoModel interRep) {
        if (!outputs.containsKey(stepNum)) {
            outputs.put(stepNum, new HashMap<Integer, InterRepInfoModel>());
        }

        outputs.get(stepNum).put(intersectionId, interRep);
    }

    @Override
    public double getSimTime(int stepNum) {
        return simTime;
    }

    @Override
    public List<SignalCommand> getSignalCommands(int stepNum, int interRepId) {
        return sigCommands;
    }

    @Override
    public List<VehicleCommand> getVehicleCommands(int stepNum, int interRepId) {
        return vehCommands;
    }

    @Override
    public List<VehicleInjectionRequest> getVehicleInjectionRequests(int stepNum, int interRepId) {
        return requests;
    }
}
