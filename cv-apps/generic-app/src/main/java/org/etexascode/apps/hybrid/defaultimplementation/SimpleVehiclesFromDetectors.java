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
package org.etexascode.apps.hybrid.defaultimplementation;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.etexascode.apps.hybrid.interfaces.IVehiclesFromDetectors;
import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorEvent;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;

/**
 * Basic implementation of the function to get vehicles from detectors This version assumes that the
 * lane manager has the speed limit inside of it...
 * 
 * @author ablatt
 */
public class SimpleVehiclesFromDetectors implements IVehiclesFromDetectors {

    int genVehId = Integer.MIN_VALUE; // Note: ablatt - It is possible that this could cause an id
                                      // conflict at some point -- I do not believe it is possible
                                      // to avoid that

    @Override
    public List<Vehicle> getVehiclesFromDetectors(VehicleManager prevMan, VehicleManager currMan, IDetectorManager detMan, LaneManager laneMan) {
        Set<Integer> detKeys = detMan.getKeys();
        List<IDetector> active = new ArrayList<IDetector>();

        for (Integer i : detKeys) {
            IDetector di = detMan.getDetector(i);
            IDetectorEvent dei = di.getDetEvent();
            if ((dei != null) && dei.isPresence()) {
                active.add(detMan.getDetector(i));
            }
        }
        // Check for vehicles in motion that passed over a presence detector during the last time
        // step.
        // Find the detectors that do not have a vehicle over it in the current time step, then
        // check that list for
        // detectors that do have a vehicle over the detector in the previous time step.
        List<IDetector> noVehDets = pruneDetectorList(pruneDetectorList(active, currMan, false), prevMan, true);
        List<Vehicle> ret = new ArrayList<Vehicle>(noVehDets.size());

        // we can ignore width and height, length is specified elsewhere...
        for (IDetector di : noVehDets) {
            // Changed to new vehicle constructor, length and width are not in the IDetector so set
            // to 0.0 here
            Vehicle v = new Vehicle(genVehId, 0.0, 0.0, di.getX(), di.getY(), di.getZ());
            v.setLaneID(di.getLaneIDs().get(0));
            v.setSpeed(laneMan.getLanes().get(v.getLaneID()).getSpeedLimitInMetersPerSecond()); // should
                                                                                                // make
                                                                                                // this
                                                                                                // the
                                                                                                // speed
                                                                                                // limit
            UtilsInterRep.genVehicleHeading(v, null, laneMan); // set heading
            v.setType(Vehicle.VEHICLE_TYPE.CAR);

            ret.add(v);

            genVehId++;
        }

        return ret;
    }

    /**
     * Remove detectors which meet the stated criterion
     * 
     * @param active The detectors which we want to
     * @param vm The vehicle manager which we want to match against the detectors
     * @param wantVehOverDet true/false - we want to remove the detector if a vehicle is over the
     *        detector
     * @return The list of the detectors in active which meet the criterion
     */
    List<IDetector> pruneDetectorList(List<IDetector> active, VehicleManager vm, boolean wantVehOverDet) {
        List<IDetector> ret = new ArrayList<IDetector>(active.size());
        for (IDetector di : active) {
            if ((vehicleOver(di, vm) != null) == wantVehOverDet) {
                ret.add(di);
            }
        }

        return ret;
    }

    /**
     * determine if a vehicle is over top of the detector
     * 
     * @param di the detector to check against
     * @param vm the vehicle manager containing the vehicles to check against
     * @return the vehicle which is over the detector (null if there is no vehicle over the
     *         detector)
     */
    Vehicle vehicleOver(IDetector di, VehicleManager vm) {
        for (Vehicle v : vm.getIterable()) {
            double len = v.getLength();
            v.setLength(len * 1.05); // increase the length to compensate for the slight difference
                                     // between bsm and the
            if (UtilsCalculations.areaIsOverDetector(UtilsCalculations.genAreaFromVehicle(v, v.genRunRiseHeading()), di)) {
                v.setLength(len);
                return v;
            }
            else {
                v.setLength(len);
            }
        }

        return null;
    }
}
