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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.apps.hybrid.interfaces.IVehiclesByLane;
import org.etexascode.interrep.datamodel.Vehicle;

/**
 * Basic implementation of the vehicle by lane filter
 * 
 * @author ablatt
 */
public class SimpleVehiclesByLane implements IVehiclesByLane {

    @Override
    public Map<Integer, List<Vehicle>> filterVehiclesByLane(List<Vehicle> vehs) {
        Map<Integer, List<Vehicle>> ret = new HashMap<Integer, List<Vehicle>>();

        for (Vehicle v : vehs) {
            if (ret.containsKey(v.getLaneID())) {
                ret.get(v.getLaneID()).add(v);
            }
            else {
                List<Vehicle> lv = new ArrayList<Vehicle>();
                lv.add(v);
                ret.put(v.getLaneID(), lv);
            }
        }

        return ret;
    }
}
