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

import java.util.HashMap;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IVehicleIdComponent;
import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;

/**
 * The default vehicle ID component.
 * 
 * @author ttevendale
 */
public class DefaultVehicleIdComponent implements IVehicleIdComponent {

    /** A map of global vehicle IDs mapped by vehicle proper IDs. */
    private Map<String, Long> vehicleGlobalIdMap = new HashMap<String, Long>();

    /** A map of vehicle proper IDs mapped by global vehicle IDs. */
    private Map<Long, String> vehicleProperIdMap = new HashMap<Long, String>();

    /** A map of global vehicle IDs mapped by stalled vehicle proper IDs. */
    private Map<String, Long> stalledVehicleMap = new HashMap<String, Long>();

    /** A counter to keep global vehicle IDs unique. */
    private long globalVehicleIdCounter = 1;

    /** A counter to keep global injected vehicle IDs unique. */
    private long globalInjectedVehicleIdCounter = -1;

    /** A counter to keep local injected vehicle IDs unique. */
    private int injectedVehicleIdCounter = 1;

    /**
     * This implementation only adds a new global ID if the vehicle's proper ID isn't already added
     * to this component otherwise returns the global ID already attached to the vehicle's proper
     * ID.
     */
    @Override
    public long addVehicleId(IVehicle vehicle) {

        String vehicleProperId = vehicle.getProperId();
        Long globalVehicleId = vehicleGlobalIdMap.get(vehicleProperId);

        if (globalVehicleId == null) {

            if (vehicle.getVehicleID() > 0) {

                globalVehicleId = globalVehicleIdCounter++;
            }
            else {

                globalVehicleId = globalInjectedVehicleIdCounter--;
            }
            vehicleGlobalIdMap.put(vehicleProperId, globalVehicleId);
            vehicleProperIdMap.put(globalVehicleId, vehicleProperId);
        }
        return globalVehicleId;
    }

    @Override
    public Long getGlobalVehicleId(String properId) {

        return vehicleGlobalIdMap.get(properId);
    }

    @Override
    public String getProperVehicleId(long globalId) {

        return vehicleProperIdMap.get(globalId);
    }

    @Override
    public boolean isStalledVehicle(IDable vehicle) {

        return stalledVehicleMap.get(vehicle.getProperId()) != null;
    }

    @Override
    public int nextInjectedVehicleId() {

        // TODO janway - the 65535 is based on TEXAS, but should be simulator-independent
        int id = injectedVehicleIdCounter;
        injectedVehicleIdCounter = (injectedVehicleIdCounter % 65535) + 1;
        return id;
    }

    @Override
    public void putReturnedVehicle(IDable returnedVehicle) {

        long globalId = stalledVehicleMap.remove(returnedVehicle.getProperId());
        vehicleProperIdMap.put(globalId, returnedVehicle.getProperId());
        vehicleGlobalIdMap.put(returnedVehicle.getProperId(), globalId);
    }

    @Override
    public void putStalledVehicle(IDable oldVehicle, IDable newVehicle) {

        long globalId = vehicleGlobalIdMap.remove(oldVehicle.getProperId());
        vehicleProperIdMap.remove(globalId);
        stalledVehicleMap.put(newVehicle.getProperId(), globalId);
    }
}
