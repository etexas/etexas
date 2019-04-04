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

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.apps.hybrid.defaultimplementation.SimpleCarMovementCalculator;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleDetermineVehicleLength;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleMapParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleNotDSRCVehicles;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleSpatParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehicleParser;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehiclesFromDetectors;
import org.etexascode.apps.hybrid.defaultimplementation.SimpleVehiclesFromDriveway;
import org.etexascode.apps.hybrid.interfaces.ICarMovementCalculator;
import org.etexascode.apps.hybrid.interfaces.IDetermineVehicleLength;
import org.etexascode.apps.hybrid.interfaces.IMapParser;
import org.etexascode.apps.hybrid.interfaces.INotDSRCVehicles;
import org.etexascode.apps.hybrid.interfaces.ISpatParser;
import org.etexascode.apps.hybrid.interfaces.IVehicleParser;
import org.etexascode.apps.hybrid.interfaces.IVehiclesFromDetectors;
import org.etexascode.apps.hybrid.interfaces.IVehiclesFromDriveway;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A driver class for producing new interrep info models based on incoming messages.
 * 
 * @author ablatt
 */
public class MicroscopicIntellifusionDriver {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(MicroscopicIntellifusionDriver.class.getName());

    /**
     * For finding vehicle lengths.
     */
    IDetermineVehicleLength vehLens = new SimpleDetermineVehicleLength();

    /**
     * For finding new vehicles which came from driveways.
     */
    IVehiclesFromDriveway vehsFromDriveway = new SimpleVehiclesFromDriveway();

    /**
     * For getting lane managers
     */
    IMapParser mapParse = new SimpleMapParser();

    /**
     * For getting signal managers
     */
    ISpatParser spatParse = new SimpleSpatParser();

    /**
     * For getting a base vehicle manager
     */
    IVehicleParser vehParse = new SimpleVehicleParser();

    /**
     * For finding new vehicles which are passing over detectors
     */
    IVehiclesFromDetectors vehsFromDets = new SimpleVehiclesFromDetectors();

    /**
     * For finding which vehicles were not DSRC vehicles
     */
    INotDSRCVehicles notDsrcVehs = new SimpleNotDSRCVehicles();

    /**
     * For moving non DSRC vehicles
     */
    ICarMovementCalculator icmc = new SimpleCarMovementCalculator();

    /**
     * For piecing together the state of the signal manager without the aid of the spat message
     */
    LightChangeCalculator lcc = null;

    /**
     * The first lane manager obtained from a mapdata message
     */
    LaneManager laneMan = null;

    /**
     * A signal manager stored until a mapdata message is received
     */
    SignalManager sigMan = null;

    /**
     * The vehicle manager from the previous time step
     */
    VehicleManager prevVehMan = new VehicleManager();

    /**
     * Constructor
     */
    public MicroscopicIntellifusionDriver() {}

    /**
     * Given the inputs and the previous outputs, parse the messages into a model of the
     * intersection.
     * 
     * @param messages the messages received by the app this time step
     * @param dmi The detector manager for this time step.
     * @param lmi The lane manager for this time step.
     * @param simTime The simTime for this time step
     * @param geoCalcType The calc type to be used when parsing vehicles
     * @param timeStepInterval The time step interval for the simulation.
     * @return A model of the intersection
     */
    public InterRepInfoModel parseModel(Object[] messages, IDetectorManager dmi, ILaneManager lmi, Double simTime, int geoCalcType, Double timeStepInterval) {

        laneMan = mapParse.parseLaneManager(messages, laneMan);
        SignalManager sm = spatParse.parseSignalManager(messages);

        if ((sigMan == null) && sm.iterator().hasNext()) {
            sigMan = sm;
        }

        if (laneMan == null) {
            return null;
        }

        // this section only to get speed limits...
        Map<Integer, Lane> lanes = laneMan.getLanes();

        for (Entry<Integer, Lane> entry : lanes.entrySet()) {
            Integer i = entry.getKey();
            ILane li = lmi.getLaneById(i);
            entry.getValue().setSpeedLimitInMetersPerSecond(li.getSpeedLimitInMetersPerSecond());
        }

        if (lcc == null) {
            Set<Integer> si = new HashSet<Integer>();
            for (ILane l : laneMan) {
                if (l.getType().equals(Lane.INBOUND)) {
                    si.add(l.getLaneId());
                }
            }

            lcc = new LightChangeCalculator(si);

            if (sigMan != null) {
                lcc.update(sigMan, simTime);
            }
        }

        lcc.update(sm, simTime);
        VehicleManager currVehMan = vehParse.parseVehicleManager(messages, laneMan, geoCalcType);

        InterRepInfoModel irim = new InterRepInfoModel(laneMan, currVehMan, lcc.getSignalManager(), dmi, new ReferencePoint[0], simTime, timeStepInterval);

        List<Vehicle> vehsDrive = vehLens.getVehicleLengths(vehsFromDriveway.getVehiclesFromDriveway(irim));
        List<Vehicle> vehsDet = vehLens.getVehicleLengths(vehsFromDets.getVehiclesFromDetectors(prevVehMan, currVehMan, irim.dmi, laneMan));
        List<Vehicle> nonDsrc = notDsrcVehs.getVehiclesNotDSRC(prevVehMan, currVehMan, vehsDrive, vehsDet, laneMan, timeStepInterval);

        for (Vehicle v : nonDsrc) {
            currVehMan.addVehicle(v.clone());
        }
        List<Vehicle> movedVehs = icmc.getCarMovements(nonDsrc, irim);
        for (Vehicle v : movedVehs) {
            currVehMan.addVehicle(v);
        }
        prevVehMan = currVehMan;
        irim = new InterRepInfoModel(laneMan, currVehMan, lcc.getSignalManager(), dmi, new ReferencePoint[0], simTime, timeStepInterval);

        return irim;
    }

    /**
     * Getter.
     * 
     * @return Function for finding vehicle lengths.
     */
    public IDetermineVehicleLength getVehLens() {
        return vehLens;
    }

    /**
     * Setter
     * 
     * @param vehLens Function for finding vehicle lengths.
     */
    public void setVehLens(IDetermineVehicleLength vehLens) {
        this.vehLens = vehLens;
    }

    /**
     * Getter.
     * 
     * @return Function for finding new vehicles which came from driveways.
     */
    public IVehiclesFromDriveway getVehsFromDriveway() {
        return vehsFromDriveway;
    }

    /**
     * Setter
     * 
     * @param vehsFromDriveway Function for finding new vehicles which came from driveways.
     */
    public void setVehsFromDriveway(IVehiclesFromDriveway vehsFromDriveway) {
        this.vehsFromDriveway = vehsFromDriveway;
    }

    /**
     * Getter
     * 
     * @return Function for finding new vehicles which are passing over detectors
     */
    public IVehiclesFromDetectors getVehsFromDets() {
        return vehsFromDets;
    }

    /**
     * Setter
     * 
     * @param vehsFromDets Function for finding new vehicles which are passing over detectors
     */
    public void setVehsFromDets(IVehiclesFromDetectors vehsFromDets) {
        this.vehsFromDets = vehsFromDets;
    }

    /**
     * Getter
     * 
     * @return Function for finding which vehicles were not DSRC vehicles
     */
    public INotDSRCVehicles getNotDsrcVehs() {
        return notDsrcVehs;
    }

    /**
     * Setter
     * 
     * @param notDsrcVehs Function for finding which vehicles were not DSRC vehicles
     */
    public void setNotDsrcVehs(INotDSRCVehicles notDsrcVehs) {
        this.notDsrcVehs = notDsrcVehs;
    }

    /**
     * Getter
     * 
     * @return Function for getting lane managers
     */
    public IMapParser getMapParse() {
        return mapParse;
    }

    /**
     * Setter
     * 
     * @param mapParse Function for getting lane managers
     */
    public void setMapParse(IMapParser mapParse) {
        this.mapParse = mapParse;
    }

    /**
     * Getter
     * 
     * @return Function for getting signal managers
     */
    public ISpatParser getSpatParse() {
        return spatParse;
    }

    /**
     * Setter
     * 
     * @param spatParse Function for getting signal managers
     */
    public void setSpatParse(ISpatParser spatParse) {
        this.spatParse = spatParse;
    }

    /**
     * Getter
     * 
     * @return Function for getting a base vehicle manager
     */
    public IVehicleParser getVehParse() {
        return vehParse;
    }

    /**
     * Setter
     * 
     * @param vehParse Function for getting a base vehicle manager
     */
    public void setVehParse(IVehicleParser vehParse) {
        this.vehParse = vehParse;
    }

    /**
     * Getter
     * 
     * @return Function for moving non DSRC vehicles
     */
    public ICarMovementCalculator getIcmc() {
        return icmc;
    }

    /**
     * Setter
     * 
     * @param icmc Function for moving non DSRC vehicles
     */
    public void setIcmc(ICarMovementCalculator icmc) {
        this.icmc = icmc;
    }
}
