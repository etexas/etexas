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

package org.etexascode.interrep;

import java.rmi.RemoteException;
import java.util.List;

import org.etexascode.datalayer.interfaces.IInterRepCoordinationDataLayer;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.DetectorManager;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages all of the vehicles, loop detectors, lanes, and signals.
 * 
 * @author bbadillo
 * @author egaebel
 * @author ablatt
 * @author janway
 */
public class InterRep implements IInterRep {

    // ~Data
    // Fields--------------------------------------------------------------
    /** Static logger */
    private static final Logger LOGGER = LoggerFactory.getLogger(InterRep.class);

    /** Manages the intersection's lanes. */
    protected LaneManager laneManager;

    /** Manages all of the detectors in the model. */
    protected DetectorManager detectorManager;

    /** Holds the current signal manager. */
    protected SignalManager signalManager;

    /** Holds the current vehicle manager. */
    protected VehicleManager vehicleManager = new VehicleManager();

    /** The current time in the simulation (in seconds). */
    private Double simTime = 0.0;

    /** The size of the time step interval (in seconds). */
    private double timeStepInterval;

    /** The current time step in the simulation. */
    private int currentTimeStep = 0;

    /** The maximum number of steps in the simulation. */
    private long maxTimeStep = 0;

    /** The underlying simulation interface. */
    protected SimulatorInterface simulator;

    /** The last retrieved step data. */
    protected StepData stepData;

    /** The reference points used in MapData messages. */
    private ReferencePoint[] referencePoints;

    /** Data Layer */
    protected IInterRepCoordinationDataLayer dataLayer;

    /** The id of the current intersection */
    protected int intersectionId;

    /** The ID of the simulation */
    long simulationId;

    /** The x coordinate of the model */
    private double x;

    /** The y coordinate of the model */
    private double y;

    /**
     * Creates a new <code>InterRep</code> instance.
     * 
     * @param intersectionId The integer ID of the intersection.
     * @param simulator The simulator for this intersection.
     * @param metadata The meta data for this intersection.
     * @param dataLayer The data layer for this intersection.
     */
    public InterRep(int intersectionId, SimulatorInterface simulator, ExecMetaData metadata, IInterRepCoordinationDataLayer dataLayer) {

        this.simulator = simulator;
        this.dataLayer = dataLayer;
        this.intersectionId = intersectionId;

        StaticData sd;

        try {

            sd = simulator.getStaticData();
            timeStepInterval = sd.getMetaData().getStepSize();

            detectorManager = sd.getDetectorManager();
            laneManager = sd.getLaneManager();

            referencePoints = new ReferencePoint[1];
            referencePoints[0] = new ReferencePoint(laneManager.getLatitude(), laneManager.getLongitude());

            currentTimeStep = (int)sd.getMetaData().getFirstStep();
            maxTimeStep = sd.getMetaData().getMaxSteps();

            x = 0;
            y = 0;
        }
        catch (RemoteException e) {

            LOGGER.error("Exception", e);
        }

        if (metadata != null) {

            simulationId = metadata.getSimulationId();

            referencePoints = metadata.getReferencePoints();

            laneManager.setLatitude(referencePoints[0].getLatitude());
            laneManager.setLongitude(referencePoints[0].getLongitude());
            laneManager.setGeoCalculatorType(metadata.getGeoCalculatorType());
            x = metadata.getX();
            y = metadata.getY();

            List<Detector> dets = metadata.getExecDetectors();

            detectorManager.clearDetectors();
            for (Detector d : dets) {
                detectorManager.addDetector(d.getDetectorID(), d);
            }
        }
    }

    /**
     * Stop and close down all resources.
     */
    @Override
    public void close() {
        try {
            simulator.close();
        }
        catch (RemoteException e) {
            LOGGER.error("Exception", e);
        }
    }

    // ~Methods------------------------------------------------------------------
    /**
     * Move the simulation to the next step and update the data model
     */
    @Override
    public boolean update() {
        stepData = null;

        if (currentTimeStep >= maxTimeStep) {
            return false;
        }

        // TODO: ablatt - should these be throwing exceptions if adding the
        // command fails?
        for (SignalCommand command : dataLayer.getSignalCommands(currentTimeStep, intersectionId)) {
            try {
                simulator.addSignalCommand(command);
            }
            catch (RemoteException e) {
                LOGGER.debug("Signal Command Exception", e);
            }
        }

        for (VehicleCommand command : dataLayer.getVehicleCommands(currentTimeStep, intersectionId)) {
            try {
                simulator.addVehicleCommand(command);
            }
            catch (RemoteException e) {
                LOGGER.debug("Vehicle Command Exception", e);
            }
        }

        for (VehicleInjectionRequest request : dataLayer.getVehicleInjectionRequests(currentTimeStep, intersectionId)) {
            try {
                simulator.addVehicleInjectionRequest(request);
            }
            catch (RemoteException e) {
                LOGGER.debug("Vehicle Injection Request Exception", e);
            }
        }

        try {
            stepData = simulator.getStepData(currentTimeStep);
        }
        catch (RemoteException e) {
            LOGGER.info("Our Exception Here", e);
            throw new RuntimeException(e);
        }

        List<String> errs = stepData.getErrorMessages();
        if ((errs == null) || (errs.size() != 0)) {
            StringBuilder excep = new StringBuilder();
            UtilsStringOnModel.addList(excep, errs, "");
            throw new RuntimeException(excep.toString());
        }

        // Create new detector manager and clear out detector events
        DetectorManager prevDetectorManager = detectorManager;
        detectorManager = new DetectorManager();
        for (Detector d : prevDetectorManager.getDetectorCollection()) {
            Detector newDetector = Detector.cloneDetector(d);
            newDetector.setDetEvent(null);
            detectorManager.addDetector(newDetector.getDetectorID(), newDetector);
        }

        // Simulate Detectors
        for (Vehicle v : stepData.getVehicles()) {
            v.setSimulationId(simulationId);
            // using the vehicle manager from the last time step along with the
            // new vehicle to calculate heading
            // Note: ablatt - possible optimization - 1) assign vehicle to lane,
            // 2) map detectors by lane, 3) only check the vehicle against the
            // detectors in its lane
            // This checks to see if the vehicle has a speed and if not
            // calculates it from the current and previous locations
            UtilsInterRep.getLaneAssignment(v, laneManager);

            Vehicle prev = vehicleManager.getVehicle(v.getProperId());
            boolean vSpeedSet = v.isSpeedSet();
            if (prev != null) {
                boolean prevSpeedSet = prev.isSpeedSet();
                double calcSpeed = UtilsCalculations.genSpeed(v, prev, timeStepInterval);
                if (vSpeedSet == false && prevSpeedSet == false) {
                    v.setSpeed(0.0);
                }
                // Checks to see if the vehicle has a speed set while in the
                // previous step it had a speed if not calculate the speed
                else if (vSpeedSet == true && prevSpeedSet == false) {
                    v.setSpeed(calcSpeed);
                }
                // Checks to see if the acceleration is set while the current and
                // previous positions of the vehicle had a speed if not calculate it
                if (v.isAccelerationSet() == false) {
                    if (vSpeedSet == true && prevSpeedSet == true) {
                        v.setAcceleration(UtilsCalculations.getAcceleration(v, prev, timeStepInterval));
                    }
                }
            }
            else if (vSpeedSet == false) {
                v.setSpeed(0.0);

            }
            if (v.isAccelerationSet() == false) {
                v.setAcceleration(0.0);
            }
            double[] latLon = UtilsLatLongConversion.convertCentimeterOffsetToLatLong(v.getX(), v.getY(), laneManager.getLatitude(), laneManager.getLongitude(), laneManager.getGeoCalculatorType());
            v.setLatitude(latLon[0]);
            v.setLongitude(latLon[1]);
            v.setElev(0);
            v.setHeading(UtilsInterRep.genVehicleHeading(v, prev, laneManager));
            v.setSteeringAngle(UtilsInterRep.calculateSteeringAngle(v, prev));
            UtilsInterRep.genDetEvent(v, prev, detectorManager, prevDetectorManager, laneManager);

        }

        DetectorManager dmClone = new DetectorManager();

        for (Detector d : detectorManager.getDetectorCollection()) {
            Detector d2 = Detector.cloneDetector(d);
            dmClone.addDetector(d2.getDetectorID(), d2);
        }

        // create new signal manager
        signalManager = new SignalManager();

        // add in new signal indications
        signalManager.addSignals(stepData.getSignalIndication());

        // Now create a new vehicle manager
        vehicleManager = new VehicleManager();
        for (Vehicle v : stepData.getVehicles()) {

            v.setSimulationId(simulationId);
            vehicleManager.addVehicle(v);
        }

        VehicleManager shiftedVehicleManager = new VehicleManager(vehicleManager);
        shiftedVehicleManager.shift(x, y);
        LaneManager shiftedLaneManager = new LaneManager(laneManager);
        shiftedLaneManager.shift(x, y);

        InterRepInfoModel irim = new InterRepInfoModel(shiftedLaneManager, shiftedVehicleManager, signalManager, dmClone, referencePoints, simTime, timeStepInterval);

        dataLayer.putInterRepModel(currentTimeStep, intersectionId, irim);

        currentTimeStep++;
        simTime = simTime + timeStepInterval;

        return true;
    }

    /**
     * Get the current time in the simulation.
     * 
     * @return The current time in the simulation or null if no time has been reported.
     */
    @Override
    public Double getSimTime() {
        return simTime;
    }

    /**
     * Gets the time step interval.
     * 
     * @return {@link #timeStepInterval}
     */
    @Override
    public double getTimeStepInterval() {
        return timeStepInterval;
    }

    /**
     * Gets the current time step.
     * 
     * @return {@link #currentTimeStep}
     */
    @Override
    public int getCurrentTimeStep() {
        return currentTimeStep;
    }
}