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

package org.etexascode.apps.dcs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.dcs.model.VehicleDilemmaZoneData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * A component used to check the status of vehicles and provide dilemma zone information.
 * 
 * @author jrutherford
 * @author janway
 */
public class VehicleStatusComponent {

    /**
     * The estimated time of arrival to to the stop bar for a vehicle (in seconds) at which the
     * dilemma zone begins.
     */
    private static final double dilemmaZoneBeginTime = 5.5;

    /**
     * A collection of dilemma zone data for each vehicle detected by the vehicle detectors. The map
     * is keyed by unique vehicle identifiers.
     */
    private Map<Integer, VehicleDilemmaZoneData> dilemmaZoneMatrix = new HashMap<Integer, VehicleDilemmaZoneData>();

    /** Reference to the lane manager. */
    private ILaneManager laneManager;

    /** Reference to the vehicle manager. */
    private IVehicleManager vehicleManager;

    /**
     * The set of lanes the algorithm is governing.
     */
    private final List<Integer> managedLaneIds;

    /**
     * Constructor.
     * 
     * @param laneManager The lane manager.
     * @param vehicleManager The vehicle manager.
     * @param managedLaneIds The managed lane IDs.
     */
    public VehicleStatusComponent(ILaneManager laneManager, IVehicleManager vehicleManager, int[] managedLaneIds) {
        this.laneManager = laneManager;
        this.vehicleManager = vehicleManager;
        this.managedLaneIds = new ArrayList<Integer>(managedLaneIds.length);
        for (int i : managedLaneIds) {
            this.managedLaneIds.add(i);
        }
    }

    /**
     * Update the managers.
     * 
     * @param interrep The interrep model.
     */
    public void updateManagers(InterRepInfoModel interrep) {
        this.laneManager = interrep.lmi;
        this.vehicleManager = interrep.vmi;
    }

    /**
     * Obtain the current collection of dilemma zone data.
     * 
     * @return The current collection of dilemma zone data.
     */
    public Collection<VehicleDilemmaZoneData> getDilemmaZoneMatrix() {
        return dilemmaZoneMatrix.values();
    }

    /**
     * Detect vehicles and calculate a list of dilemma zone information.
     * 
     * @param currentTime The current time (in seconds) since the start of the algorithm loop.
     */
    public void performDetection(double currentTime) {
        for (ILane lane : laneManager.getLanes().values()) {
            if (lane.getType().equals(Lane.OUTBOUND) || !managedLaneIds.contains(lane.getLaneId())) {
                continue; // No dilemma zone for vehicles in outbound lanes.
            }

            List<? extends IVehicle> detectedVehicles = vehicleManager.getVehiclesInLane(lane.getLaneId());
            if (!detectedVehicles.isEmpty()) {
                for (IVehicle vehicle : detectedVehicles) {
                    double vehicleSpeed = vehicle.getSpeed();
                    if (vehicleSpeed != 0.0) {
                        VehicleDilemmaZoneData newData = new VehicleDilemmaZoneData();
                        newData.setLaneId(lane.getLaneId());
                        newData.setVehicleSpeed(UtilsUnitConversion.convertMetersToFeet(vehicleSpeed));

                        double timeToZone = computeTimeOfArrivalToDilemmaZone(currentTime, UtilsCalculations.calcDistToStopLine(vehicle, lane), vehicleSpeed, detectedVehicles);
                        newData.setTimeOfArrivalToDilemmaZone(timeToZone);

                        newData.setVehicleLength(UtilsUnitConversion.convertMetersToFeet(vehicle.getLength()));
                        dilemmaZoneMatrix.put(vehicle.getVehicleID(), newData);
                    }
                }
            }
        }
    }

    /**
     * Calculate the start time of the dilemma zone based on where a a vehicle was detected, the
     * vehicles current speed, and the current time into the algorithm. Also takes slow leading
     * vehicles into account.
     * 
     * @param currentTime The current time (in seconds) since the start of the algorithm loop.
     * @param cmFromStopLine The distance (in cm) between the vehicle and the stop bar.
     * @param vehicleSpeed The velocity (in meters per second) of the vehicle.
     * @return The start time of the dilemma zone in relation to the current time.
     */
    private double computeTimeOfArrivalToDilemmaZone(double currentTime, double cmFromStopLine, double vehicleSpeed, List<? extends IVehicle> otherVehiclesInLane) {
        if (vehicleSpeed == 0.0) {
            throw new IllegalStateException("Cannot compute a dilemma zone with a vehicle speed of 0.0");
        }

        double secondsFromStopLine = UtilsUnitConversion.convertCentimetersToMeters(cmFromStopLine) / vehicleSpeed;
        double secondsFromBeginDilemmaZone = secondsFromStopLine - dilemmaZoneBeginTime;
        IVehicle closestVehicle = null;
        double closestVehDistToStopLine = 0.0;
        // Finds the closest vehicle in front of the one being checked.
        for (int i = 0; i < otherVehiclesInLane.size(); i++) {
            IVehicle currentVehicle = otherVehiclesInLane.get(i);
            double currentDistToStopLine = UtilsCalculations.calcDistToStopLine(currentVehicle, laneManager.getLaneById(currentVehicle.getLaneID()));
            if (currentDistToStopLine < cmFromStopLine) {
                if (closestVehicle == null || currentDistToStopLine > closestVehDistToStopLine) {
                    closestVehicle = currentVehicle;
                    closestVehDistToStopLine = currentDistToStopLine;
                }
            }
        }
        double timeOfArrival = currentTime + secondsFromBeginDilemmaZone;
        if (closestVehicle != null) {
            double closestTimeOfArrival = currentTime + ((UtilsUnitConversion.convertCentimetersToMeters(closestVehDistToStopLine) / closestVehicle.getSpeed()) - dilemmaZoneBeginTime);
            if (timeOfArrival < closestTimeOfArrival + 1.5) {
                // Vehicle will overtake closest vehicle and needs its time adjusted, change its
                // arrival time to just after the closest vehicle's
                timeOfArrival = closestTimeOfArrival + 1.5;
            }
        }

        return timeOfArrival;
    }

    /**
     * Clears the dilemma zone matrix to remove out-dated information once the phase ends.
     */
    public void resetDilemmaZoneMatrix() {
        dilemmaZoneMatrix.clear();
    }
}