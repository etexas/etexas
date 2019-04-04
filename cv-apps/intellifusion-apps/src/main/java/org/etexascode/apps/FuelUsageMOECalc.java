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
package org.etexascode.apps;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.devicedata.AppLogger;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;

/**
 * A MOE Calculator designed to. TODO ttevendale 1/4/2018 look into creating unit tests and/or
 * updating this calc to use the new spec
 * 
 * @author ablatt
 */
public class FuelUsageMOECalc {

    /**
     * Key: Vehicle Id Value: Fuel Consumed by this Vehicle
     */
    Map<Integer, Double> fuelConsumed;

    /**
     * Key: Vehicle Id Value: Multiplier
     */
    Map<Integer, Integer> vehicleMultiplier;

    /**
     * Key: Lane Id Value: Vehicle Id
     */
    Map<Integer, Integer> currVehicleForLane;

    /**
     * Key: Detector Id Value: Lane Id
     */
    Map<Integer, Integer> detectorForLane;

    /**
     * Key: Lane Id Value: Multiplier
     */
    Map<Integer, Integer> preVehMultiplier;

    /**
     * VehicleManager from the previous time step
     */
    IVehicleManager prevVehMan = null;

    /**
     * Fuel calculator
     */
    IFuelCalculator fuelCalculator;

    /**
     * Constructor which determines which detectors to use.
     * 
     * @param lanMan The lane manager which is to be used to initialize the lanes to watch.
     * @param detMan The detector manager to use for determining which detectors belong to which
     *        lanes.
     */
    public FuelUsageMOECalc(ILaneManager lanMan, IDetectorManager detMan) {
        this(lanMan, buildDetLaneMap(detMan));
    }

    /**
     * Constructor which initializes all class variables.
     * 
     * @param lanMan The lane manager which is to be used to initialize the lanes to watch.
     * @param detsForLanes Key: Detector Id Value: Lane Id
     */
    public FuelUsageMOECalc(ILaneManager lanMan, Map<Integer, Integer> detsForLanes) {
        fuelConsumed = new HashMap<Integer, Double>();
        vehicleMultiplier = new HashMap<Integer, Integer>();
        currVehicleForLane = new HashMap<Integer, Integer>();
        preVehMultiplier = new HashMap<Integer, Integer>();
        fuelCalculator = new FuelCalcImpl();

        for (Integer i : lanMan.getLaneIds()) {
            ILane li = lanMan.getLaneById(i);
            currVehicleForLane.put(li.getLaneId(), null);
            preVehMultiplier.put(i, 0);
        }

        detectorForLane = detsForLanes;
    }

    /**
     * Setter {@link #fuelCalculator}
     * 
     * @param newCalc The new fuel calculator to set.
     */
    public void setFuelCalculator(IFuelCalculator newCalc) {
        fuelCalculator = newCalc;
    }

    /**
     * Logs the total fuel consumed during this simulation.
     * 
     * @param logger The logger.
     */
    public void onDestroy(AppLogger logger) {
        BigDecimal totalFuelConsumed = new BigDecimal(0.0);

        for (Entry<Integer, Double> entry : fuelConsumed.entrySet()) {

            Integer vehicleId = entry.getKey();
            totalFuelConsumed = totalFuelConsumed.add(new BigDecimal(entry.getValue() * vehicleMultiplier.get(vehicleId)));
        }

        logger.log("Fuel Consumption MOE", "total fuel consumed: " + totalFuelConsumed.toString() + " (need to add units here)");
    }

    /**
     * Updates the amount of fuel consumed during this simulation.
     * 
     * @param updatedVehicles The BSM vehicles which were observed during this time step.
     * @param updatedDetectors The detectors as observed during this time step. (assumed to be taken
     *        from the underlying model).
     * @param vehMan The vehicle manager reconstructed from BSM messages.
     * @param simTime The sim time for this time step.
     */
    public void update(Collection<IVehicle> updatedVehicles, Collection<IDetector> updatedDetectors, IVehicleManager vehMan, double simTime) {
        for (String i : vehMan.getAllVehicleIds()) {
            IVehicle v = vehMan.getVehicle(i);

            if ((prevVehMan == null) || (prevVehMan.getVehicle(i) == null)) {
                fuelConsumed.put(v.getVehicleID(), 0.0);
                vehicleMultiplier.put(v.getVehicleID(), 1);
            }
            else {
                double fuelUsed = fuelCalculator.calculateFuel();
                double currFuelUsed = fuelConsumed.get(v.getVehicleID());
                fuelConsumed.put(v.getVehicleID(), fuelUsed + currFuelUsed);
            }
        }

        for (IDetector detInf : updatedDetectors) {
            if ((detInf.getDetEvent() != null) && detInf.getDetEvent().isPresence()) {
                Integer currLaneId = detectorForLane.get(detInf.getDetectorID());
                IVehicle vInf = isOverDetector(detInf, updatedVehicles);

                if (vInf == null) {
                    Integer vehInf = currVehicleForLane.get(currLaneId);
                    if (vehInf == null) {
                        int multiplier = preVehMultiplier.get(currLaneId);
                        multiplier++;
                        preVehMultiplier.put(currLaneId, multiplier);
                    }
                    else {
                        int multiplier = vehicleMultiplier.get(vehInf);
                        multiplier++;
                        vehicleMultiplier.put(vehInf, multiplier);
                    }
                }
                else {
                    Integer vTmp = currVehicleForLane.get(detectorForLane.get(detInf.getDetectorID()));

                    if (vTmp == null) {
                        int multiplier = vehicleMultiplier.get(vInf.getVehicleID());
                        multiplier += preVehMultiplier.get(currLaneId);
                        vehicleMultiplier.put(vInf.getVehicleID(), multiplier);
                    }

                    currVehicleForLane.put(currLaneId, vInf.getVehicleID());
                }
            }
        }

        prevVehMan = vehMan;
    }

    /**
     * Determines if a Vehicle which was updated this time step is over a detector.
     * 
     * @param detInf
     * @param vehs
     * @return The vehicle which was over detInf. Null if no vehicle was over detInf.
     */
    private static IVehicle isOverDetector(IDetector detInf, Collection<IVehicle> vehs) {
        for (IVehicle v : vehs) {
            if (UtilsCalculations.vehicleIsOverDetector(v, detInf)) {
                return v;
            }
        }

        return null;
    }

    /**
     * Utility method for assigning detectors to lanes. Note: Likely unstable if there is more than
     * 1 detector for a lane.
     * 
     * @param detMan The detector manager to convert.
     * @return Key: Detector Id Value: Lane Id
     */
    private static Map<Integer, Integer> buildDetLaneMap(IDetectorManager detMan) {
        Map<Integer, Integer> detByLane = new HashMap<Integer, Integer>();
        List<Integer> usedLaneIds = new LinkedList<Integer>();

        for (Integer i : detMan.getKeys()) {
            IDetector det = detMan.getDetector(i);
            Integer detLane = det.getLaneIDs().get(0);

            if (!usedLaneIds.contains(detLane)) {
                detByLane.put(det.getDetectorID(), detLane);
                usedLaneIds.add(detLane);
            }
        }

        return detByLane;
    }
}