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
package org.etexascode.vehiclelocationmanager.shared;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * Sorts vehicles into logged out and logged in vehicles.
 * 
 * @author ablatt
 */
public class VehicleSorter {

    /**
     * Constructor
     */
    public VehicleSorter() {}

    /**
     * Function for sorting vehicles into logging out and logging in vehicles
     * 
     * @param prev The vehicle manager from the previous time step
     * @param curr The vehicle manager from the current time step
     * @param injected The vehicles injected into the vehicle manager this time step
     * @param intersectionId The Id of the Intersection the IVehicleManagers belong to (used as key
     *        in laneToLaneMap)
     * @param laneToLaneMap Key: Id of the intersection with the outgoing lane, Value: Id of the
     *        lane the key feeds into
     * @return The vehicles sorted into logged in and logged out vehicles
     */
    public SortedVehicles sortVehicles(IVehicleManager prev, IVehicleManager curr, Set<String> injected, int intersectionId, Map<DualIntIdentifier, DualIntIdentifier> laneToLaneMap) {
        Set<String> prevIds = prev.getAllVehicleIds();
        Set<String> currIds = curr.getAllVehicleIds();

        return new SortedVehicles(getLoginVehicles(prevIds, currIds, injected, curr), getLogoutVehicles(prevIds, currIds, prev, intersectionId, laneToLaneMap));
    }

    /**
     * Function which gets all the vehicles which logged out this time step
     * 
     * @param prev The ids of the vehicles from the previous time step
     * @param curr The ids of the vehicles from the current time step
     * @param prevVehs The vehicle manager from the previous time step
     * @param intersectionId The Id of the Intersection the IVehicleManagers belong to (used as key
     *        in laneToLaneMap)
     * @param laneToLaneMap Key: Id of the intersection with the outgoing lane, Value: Id of the
     *        lane the key feeds into
     * @return The logged out vehicles (previous time step's vehicle)
     */
    Map<DualIntIdentifier, List<IVehicle>> getLogoutVehicles(Set<String> prev, Set<String> curr, IVehicleManager prevVehs, int intersectionId,
            Map<DualIntIdentifier, DualIntIdentifier> laneToLaneMap) {
        Map<DualIntIdentifier, List<IVehicle>> ret = new HashMap<DualIntIdentifier, List<IVehicle>>();
        for (String i : prev) {
            if (!curr.contains(i)) {
                IVehicle vi = prevVehs.getVehicle(i);
                DualIntIdentifier dii = getPairing(intersectionId, vi, laneToLaneMap);
                List<IVehicle> l = ret.get(dii);
                if (l == null) {
                    l = new LinkedList<IVehicle>();
                    l.add(vi);
                    ret.put(dii, l);
                }
                else {
                    l.add(vi);
                }
            }
        }
        return ret;
    }

    /**
     * The vehicles which logged in this time step
     * 
     * @param prev The ids of the vehicles from the previous time step
     * @param curr The ids of the vehicles from the current time step
     * @param injected The vehicles injected into the vehicle manager this time step
     * @param currVehs The vehicle manager from the current time step
     * @return The vehicles which logged in this time step (does not include injected vehicles)
     */
    List<IVehicle> getLoginVehicles(Set<String> prev, Set<String> curr, Set<String> injected, IVehicleManager currVehs) {
        List<IVehicle> logIn = new LinkedList<IVehicle>();
        for (String i : curr) {
            if ((!prev.contains(i)) && (!injected.contains(i))) {
                logIn.add(currVehs.getVehicle(i));
            }
        }
        return logIn;
    }

    /**
     * Function for finding the lane a specific vehicle is supposed to go into
     * 
     * @param intersectionId The intersection the vehicle is in
     * @param vi The vehicle which is leaving the intersection
     * @param laneToLaneMap The mappings of which lanes in which intersection go to which other
     *        lanes in which other intersection
     * @return The identifier for the new lane the vehicle is in (default if there is no lane the
     *         vehicle is supposed to go into)
     */
    DualIntIdentifier getPairing(int intersectionId, IVehicle vi, Map<DualIntIdentifier, DualIntIdentifier> laneToLaneMap) {
        // build the identifier vi would show up in if said identifier is in the laneToLaneMap
        DualIntIdentifier dii = new DualIntIdentifier(intersectionId, vi.getLaneID(), false);
        DualIntIdentifier dii2 = laneToLaneMap.get(dii);

        if (dii2 != null) {
            return dii2; // The identifier is there, meaning this lane connects to a different lane
        }

        return new DualIntIdentifier(0, 0, true); // vi is not in a lane that connects to a
                                                  // different lane
    }
}
