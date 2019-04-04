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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;

public class UtilsTestPhaseFailure {

    public static List<ISignalIndication> genSignalIndications() {
        List<ISignalIndication> ret = new LinkedList<ISignalIndication>();

        SignalIndication si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(20);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setTimeToChange(5);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.RED);
        si.setTimeToChange(10);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setTimeToChange(40);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.GREEN);
        si.setTimeToChange(80);
        ret.add(si);

        si = new SignalIndication();
        si.setColorIndication(SignalIndication.Color.YELLOW);
        si.setTimeToChange(7);
        ret.add(si);

        return ret;
    }

    // public static List<EstimatedDataModel> genModelFromInterRep(InterRep interRep) {
    // List<EstimatedDataModel> ret = new ArrayList<EstimatedDataModel>();
    //
    // while(interRep.update()) {
    // ret.add(genEstimatedModel(interRep));
    // }
    //
    // return ret;
    // }
    //
    // public static EstimatedDataModel genEstimatedModel(InterRep interRep) {
    // EstimatedDataModel ret = new EstimatedDataModel();
    //
    // ret.detectorManager = new DetectorManagerInfo(interRep.getDetectorManager());
    // ret.laneManager = new LaneManagerInfo(interRep.getIntersectionManager());
    // ret.signalManager = new SignalManagerInfo(interRep.getSignalManager());
    // ret.vehicleManager = new VehicleManagerInfo(interRep.getVehicleManager(),
    // interRep.getIntersectionManager());
    //
    // return ret;
    // }

    public static Map<Integer, Double> genQueueLens() {
        Map<Integer, Double> ret = new HashMap<Integer, Double>();
        ret.put(0, 1.0);
        ret.put(18, 862 * 4.0);

        return ret;
    }
}
