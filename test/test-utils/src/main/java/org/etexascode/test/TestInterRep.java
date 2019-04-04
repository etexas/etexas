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
package org.etexascode.test;

import org.etexascode.datalayer.IDataLayer;
import org.etexascode.interrep.InterRep;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.VehicleManager;

/**
 * TestInterRep wraps InterRep. Adds sets for LaneManager, SignalManager, VehicleManager. We may add
 * other sets later
 * 
 * @author bmauldon
 */
public class TestInterRep extends InterRep {

    /**
     * Manages all of the signals in the model.
     * 
     * @param sim The simulator.
     * @param metadata The meta data.
     * @param idl The data layer.
     */
    public TestInterRep(SimulatorInterface sim, ExecMetaData metadata, IDataLayer idl) {
        super(0, sim, metadata, idl);
    }

    public LaneManager getLaneManager() {
        return this.laneManager;
    }

    public void setLaneManager(LaneManager lm) {
        laneManager = lm;
    }

    public DetectorManager getDetectorManager() {
        return this.detectorManager;
    }

    public void setDetectorManager(DetectorManager dm) {
        detectorManager = dm;
    }

    public VehicleManager getVehicleManager() {
        return vehicleManager;
    }

    public SignalManager getSignalManager() {
        return signalManager;
    }

    public int getId() {

        return intersectionId;
    }
}
