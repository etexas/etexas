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

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.interrep.UtilsInterRep;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.SPAT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A container class provided by any --PopulatedApp class to any class that extends it. List of
 * variable usages: detectorManager: this is set with the actual detectors in the underlying model.
 * vehicleManager: this is set with the vehicles parsed from the BasicSafetyMessages and
 * BasicSafetyMessageVerbose messages provided. signalManager: a signal manager built from the last
 * received SPAT message. laneManager: a lane manager built from the last received MapData message.
 * bsms: a list of all the BasicSafetyMessages messages received this time step. bsmvs: a list of
 * all the BasicSafetyMessageVerbose messages received this time step. spats: a list of all the SPAT
 * messages received this time step. map: a list of all the MapData messages received this time
 * step. updatedVehicles: a list of all the Vehicles which changed this time step (that is, all the
 * vehicles we received BasicSafetyMessage or BasicSafetyMessageVerbose messages for).
 * updatedSignals: a list of the signals updated via SPAT message this time step. updatedDetectors:
 * a list of detectors updated this time step. referencePointSet: is true when the reference point
 * of the intersection is set. Currently used to check that a MapData message was received/there is
 * a LaneManager.
 * 
 * @author ablatt
 * @author janway
 */
public class EstimatedDataModel {

    /**
     * Static logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(EstimatedDataModel.class);

    /**
     * This is set with the actual detectors in the underlying model.
     */
    private IDetectorManager detectorManager = null;

    /**
     * This is set with the vehicles parsed from the BasicSafetyMessages and
     * BasicSafetyMessageVerbose messages provided.
     */
    private IVehicleManager vehicleManager = null;

    /**
     * A signal manager built from the last received SPAT message.
     */
    private ISignalManager signalManager = null;

    /**
     * A lane manager built from the last received MapData message.
     */
    private ILaneManager laneManager = null;

    /**
     * The underlying VehicleManager that we populate but do not expose.
     */
    private VehicleManager vm = null;

    /**
     * The underlying SignalManager that we populate but do not expose.
     */
    private SignalManager sm = null;

    /**
     * The underlying LaneManager that we populate but do not expose.
     */
    private LaneManager lm = null;

    /**
     * The previous detectors, used to determine which ones need updating.
     */
    private List<IDetector> prevDets = null;

    /**
     * A list of all the BasicSafetyMessages messages received this time step.
     */
    private Collection<BasicSafetyMessage> bsms;

    /**
     * A list of all the BasicSafetyMessageVerbose messages received this time step.
     */
    private Collection<BasicSafetyMessageVerbose> bsmvs;

    /**
     * A list of all the SPAT messages received this time step.
     */
    private Collection<SPAT> spats;

    /**
     * A list of all the MapData messages received this time step.
     */
    private Collection<MapData> map;

    /**
     * A list of all the Vehicles which changed this time step (that is, all the vehicles we
     * received BasicSafetyMessage or BasicSafetyMessageVerbose messages for).
     */
    private Collection<IVehicle> updatedVehicles;

    /**
     * A list of the signals updated via SPAT message this time step.
     */
    private Collection<ISignalIndication> updatedSignals;

    /**
     * A list of detectors updated this time step.
     */
    private Collection<IDetector> updatedDetectors;

    /**
     * A list of all the vehicles being logged out this time step.
     */
    private Collection<IVehicle> logoutVehicles;

    /**
     * Is true when the reference point of the intersection is set.
     */
    private boolean referencePointSet = false;

    EstimatedDataModel(Object[] messages, IDetectorManager dmi, int geoCalcType, int logoutZone) {
        updateEDM(messages, dmi, geoCalcType, logoutZone);
    }

    // TODO ttevendale 1/2/2018 update this class to handle messages using byte[] and add unit tests
    // to ensure the logic is working as expected, also add proper javadocs
    public void updateEDM(Object[] messages, IDetectorManager dmi, int geoCalcType, int logoutZone) {
        bsms = UtilsFilterMessages.filterForBSM(messages);
        bsmvs = UtilsFilterMessages.filterForBSMV(messages);
        spats = UtilsFilterMessages.filterForSPAT(messages);
        map = UtilsFilterMessages.filterForMap(messages);

        if (map.size() > 0) {
            referencePointSet = true;
            lm = UtilsMessageImports.parseMapDataMessage(map.iterator().next(), geoCalcType);
        }
        if (!referencePointSet) {
            return;
        }

        LOGGER.debug("The lane manager :" + lm.toString());

        sm = new SignalManager();
        vm = new VehicleManager();

        updatedDetectors = new LinkedList<IDetector>();
        updatedSignals = new LinkedList<ISignalIndication>();
        updatedVehicles = new LinkedList<IVehicle>();

        detectorManager = dmi;
        vehicleManager = vm;
        signalManager = sm;
        laneManager = lm;

        LOGGER.debug("The BSMs at RSE: " + bsms.toString());

        for (SPAT spat : spats) {
            List<SignalIndication> signals = UtilsMessageImports.importSignal(spat);
            for (SignalIndication s : signals) {
                sm.addSignal(s);
            }
            updatedSignals.addAll(signals);
        }

        for (BasicSafetyMessage bsm : bsms) {
            Vehicle v = UtilsMessageImports.importBSM(bsm, lm, geoCalcType);
            vm.addVehicle(v);
            updatedVehicles.add(vehicleManager.getVehicle(v.getProperId()));
        }

        for (BasicSafetyMessageVerbose bsmv : bsmvs) {
            Vehicle v = UtilsMessageImports.importBSMV(bsmv, lm, geoCalcType);
            vm.addVehicle(v);
            updatedVehicles.add(vehicleManager.getVehicle(v.getProperId()));
        }

        if (prevDets == null) {
            for (Integer key : detectorManager.getKeys()) {
                updatedDetectors.add(detectorManager.getDetector(key));
            }
            prevDets = new LinkedList<IDetector>();
        }
        else {
            for (IDetector prevInf : prevDets) {
                IDetector nextInf = detectorManager.getDetector(prevInf.getDetectorID());

                if (!prevInf.equals(nextInf)) {
                    updatedDetectors.add(nextInf);
                }
            }
        }

        logoutVehicles(logoutZone);

        prevDets.clear();
        for (Integer key : detectorManager.getKeys()) {
            prevDets.add(detectorManager.getDetector(key));
        }
    }

    public void logoutVehicles(int logoutZone) {
        Map<Integer, Lane> lanes = lm.getLanes();
        logoutVehicles = new LinkedList<IVehicle>();

        for (Entry<Integer, Lane> entry : lanes.entrySet()) {
            Integer i = entry.getKey();
            if (Lane.OUTBOUND.equals(entry.getValue().getType())) {
                List<Vehicle> toCheck = vm.getVehiclesInLane(i);
                ILaneNode last = UtilsInterRep.getLastNode(lm.getLaneById(i));

                for (Vehicle v : toCheck) {
                    if (logoutZone < UtilsCalculations.getDistance(v, last)) {
                        vm.removeVehicle(v.getProperId());
                        logoutVehicles.add(v);
                    }
                }
            }
        }
    }

    public IDetectorManager getDetectorManager() {
        return detectorManager;
    }

    public IVehicleManager getVehicleManager() {
        return vehicleManager;
    }

    public ISignalManager getSignalManager() {
        return signalManager;
    }

    public ILaneManager getLaneManager() {
        return laneManager;
    }

    public Collection<BasicSafetyMessage> getBsms() {
        return bsms;
    }

    public Collection<BasicSafetyMessageVerbose> getBsmvs() {
        return bsmvs;
    }

    public Collection<SPAT> getSpats() {
        return spats;
    }

    public Collection<MapData> getMap() {
        return map;
    }

    public Collection<IVehicle> getUpdatedVehicles() {
        return updatedVehicles;
    }

    public Collection<ISignalIndication> getUpdatedSignals() {
        return updatedSignals;
    }

    public Collection<IDetector> getUpdatedDetectors() {
        return updatedDetectors;
    }

    public Collection<IVehicle> getLogoutVehicles() {
        return logoutVehicles;
    }

    public boolean isReferencePointSet() {
        return referencePointSet;
    }
}
