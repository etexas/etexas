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
package org.etexascode.vehiclelocationmanager.singleintersection;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.datalayer.interfaces.IVehicleLocationDataLayer;
import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.CellDeviceData;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.interrep.datamodel.CommandSource;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.vehiclelocationmanager.IVehicleLocationManager;
import org.etexascode.vehiclelocationmanager.shared.InitializedAppData;
import org.etexascode.vehiclelocationmanager.shared.SortedVehicles;
import org.etexascode.vehiclelocationmanager.shared.VehicleAppInitializer;
import org.etexascode.vehiclelocationmanager.shared.VehicleSorter;

/**
 * Vehicle Location Manager built around a single intersection.
 * 
 * @author ablatt
 */
public class VehicleLocationManagerImpl implements IVehicleLocationManager {

    /**
     * App initialization configurations for dsrc
     */
    final List<AppInitConfig> dsrcConfs;

    /**
     * App initialization configurations for cells
     */
    final List<AppInitConfig> cellConfs;

    /**
     * The intersection id for this Location Manager
     */
    final int intersectionId;

    /**
     * The data layer this location manager will be getting data from and putting data into
     */
    final IVehicleLocationDataLayer data;

    /**
     * Contains the function for vehicle sorting
     */
    final VehicleSorter sorter = new VehicleSorter();

    /**
     * Contains the function for app initialization
     */
    final VehicleAppInitializer appInit = new VehicleAppInitializer();

    /**
     * Maps lanes in one intersection to lanes in another intersection
     */
    final Map<DualIntIdentifier, DualIntIdentifier> laneToLaneMap;

    /**
     * The vehicle manager from the previous time
     */
    IVehicleManager vmi;

    /**
     * Constructor
     * 
     * @param intersectionId The id of the intersection this location manager is governing
     * @param data The data layer
     * @param dsrcConfs The app configurations provided by the user for OBUs (dsrc)
     * @param cellConfs The app configurations provided by the user for cells
     * @param laneToLaneMap A mapping of lanes between intersections.
     */
    public VehicleLocationManagerImpl(int intersectionId, IVehicleLocationDataLayer data, List<AppInitConfig> dsrcConfs, List<AppInitConfig> cellConfs,
            Map<DualIntIdentifier, DualIntIdentifier> laneToLaneMap) {
        this.intersectionId = intersectionId;
        this.data = data;
        this.dsrcConfs = dsrcConfs;
        this.cellConfs = cellConfs;
        this.laneToLaneMap = laneToLaneMap;
        vmi = new VehicleManager();
    }

    /**
     * Manages vehicle manager information for vehicles in the specified intersection and puts it
     * into the data layer then sets current vehicle manager to previous vehicle manager for next
     * time step
     * 
     * @param stepNum time step of the simulation for actions to be performed
     * @throws InstantiationException If the instantiation exception occurs.
     * @throws IllegalAccessException If the illegal access exception occurs.
     */
    @Override
    public void manageLocations(int stepNum) throws InstantiationException, IllegalAccessException {

        IVehicleManager irim = data.getVehicleManagerInfo(stepNum, intersectionId);
        Set<String> injected = data.getInjectedVehicles(stepNum, intersectionId);
        SortedVehicles sv = sorter.sortVehicles(vmi, irim, injected, intersectionId, laneToLaneMap);
        List<IVehicle> newVehicles = assignGlobalIds(irim);

        InitializedAppData iad = appInit.dsrcInitApps(newVehicles, dsrcConfs, data.getRandomSeed(stepNum, intersectionId));
        List<CellDeviceData> cells = appInit.cellInitApps(newVehicles, cellConfs, data.getRandomSeed(stepNum, intersectionId));

        // Request simulators that are receiving vehicles from other intersections to inject them
        // into their simulations
        for (Entry<DualIntIdentifier, List<IVehicle>> entry : sv.logoutVehicles.entrySet()) {
            if (!entry.getKey().isDefault) { // vehicles is moving to another intersection
                for (IVehicle veh : entry.getValue()) {
                    VehicleInjectionRequest req = new VehicleInjectionRequest();

                    Vehicle newVeh = new Vehicle(veh);
                    newVeh.setVehicleID(data.nextInjectedVehicleId());
                    newVeh.setLaneID(entry.getKey().id2);

                    Vehicle properIdVehicle = new Vehicle(newVeh);
                    if (properIdVehicle.getVehicleID() > 0) {

                        properIdVehicle.setVehicleID(properIdVehicle.getVehicleID() * -1);
                    }
                    else {

                        properIdVehicle.setVehicleID(properIdVehicle.getVehicleID());
                    }
                    properIdVehicle.setSimulationId(entry.getKey().id1);
                    data.putStalledVehicle(veh, properIdVehicle);

                    req.setVehicle(newVeh);
                    req.setIntersectionId(entry.getKey().id1);
                    req.setInjectionTime(data.getSimTime(stepNum));
                    data.addVehicleInjectionRequest(CommandSource.SYSTEM, stepNum, req, entry.getKey().id1);
                }
            }
        }

        data.putAppDeviceData(stepNum, iad.initedApps);
        data.putAppDeviceData(stepNum, cells);
        data.putRandomSeed(stepNum, intersectionId, iad.randSeed);
        data.putLoggedOutVehicles(stepNum, sv.logoutVehicles);

        vmi = irim;
    }

    /**
     * Assigns the global IDs to the vehicles this time step. Also figures out which vehicles are
     * new to the composite.
     * 
     * @param vehicleManager The vehicle manager that's been passed in this time step
     * @return The new vehicles in the composite.
     */
    private List<IVehicle> assignGlobalIds(IVehicleManager vehicleManager) {

        List<IVehicle> newVehicles = new ArrayList<IVehicle>();

        for (IVehicle iVehicle : vehicleManager) {

            Vehicle vehicle = (Vehicle)iVehicle;
            String properId = vehicle.getProperId();
            Long globalId = data.getGlobalVehicleId(properId);
            if (globalId != null) {

                vehicle.setGlobalId(globalId);
            }
            else {

                if (data.isStalledVehicle(vehicle)) {

                    data.putReturnedVehicle(vehicle);
                }
                else {

                    newVehicles.add(vehicle);
                    data.addVehicleId(vehicle);
                }
                vehicle.setGlobalId(data.getGlobalVehicleId(properId));
            }

            data.addVehicleId(vehicle);
            vehicle.setGlobalId(data.getGlobalVehicleId(vehicle.getProperId()));
        }
        return newVehicles;
    }
}
