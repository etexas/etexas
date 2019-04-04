/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** *  COPYRIGHT (C) 2003 by The University of Texas at Austin   * ** *
 * * ** *                                                            * ** *
 * * ** * Permission is hereby granted to use, modify, copy, and     * ** *
 * * ** * distribute this software and its documentation for any     * ** *
 * * ** * purpose only without profit, provided that the above       * ** *
 * * ** * Copyright Notice appears in all copies and that both the   * ** *
 * * ** * Copyright Notice and this Permission Notice appears in     * ** *
 * * ** * every copy of supporting documentation.  No title to nor   * ** *
 * * ** * ownership of the software is transferred hereby.  The name * ** *
 * * ** * of The University of Texas at Austin shall not be used in  * ** *
 * * ** * advertising or publicity related to the distribution of    * ** *
 * * ** * the software without specific, written, prior permission.  * ** *
 * * ** * This software is provided as-delivered without expressed   * ** *
 * * ** * or implied warranty.  The University of Texas at Austin    * ** *
 * * ** * makes no representation about the suitability of this      * ** *
 * * ** * software for any purpose and accepts no responsibility for * ** *
 * * ** * its use.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * * ** *                                                            * ** *
 * * ** * This program is free software; you can redistribute it     * ** *
 * * ** * and/or modify it under the terms of the GNU General Public * ** *
 * * ** * License as published by the Free Software Foundation;      * ** *
 * * ** * either version 2 of the License, or (at your option) any   * ** *
 * * ** * later version.                                             * ** *
 * * ** *                                                            * ** *
 * * ** * This program is distributed in the hope that it will be    * ** *
 * * ** * useful, but WITHOUT ANY WARRANTY; without even the implied * ** *
 * * ** * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    * ** *
 * * ** * PURPOSE.  See the GNU General Public License for more      * ** *
 * * ** * details.                                                   * ** *
 * * ** *                                                            * ** *
 * * ** * You should have received a copy of the GNU General Public  * ** *
 * * ** * License along with this program; if not, write to the Free * ** *
 * * ** * Software Foundation, Inc., 51 Franklin Street, Fifth       * ** *
 * * ** * Floor, Boston, MA 02110-1301, USA.                         * ** *
 * * ** *                                                            * ** *
 * * ** * For more information: http://www.gnu.org/licenses/gpl.html * ** *
 * * ** *                                                            * ** *
 * * ** ************************************************************** ** *
 * #L%
 */

package org.etexascode.api;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.etexascode.CoberturaIgnore;
import org.etexascode.api.SimproJNA.SIMPRO_VehicleInjectionData;
import org.etexascode.api.SimproJNA.SIMPRO_VehicleInjectionError;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.StaticData;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.interrep.datamodel.VehicleDestinationCommand;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.VehicleLaneChangeCommand;
import org.etexascode.interrep.datamodel.VehicleSpeedCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jna.ptr.DoubleByReference;

/**
 * A wrapper class to hide data management logic between the eTEXAS C library and the Java
 * SimproInterface. This class provides a mapping between data obtained from the eTEXAS C library
 * and the J2735 Message Set. Part of the Public TEXAS Model Java API
 * 
 * @author bbadillo
 * @author ablatt
 */
public class eTEXAS {

    /**
     * Logger for convenience.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(eTEXAS.class);

    /** M/S to MPH conversion ratio. */
    private static final double MS_TO_MPH = 2.23694;

    /**
     * The current time step in the simulation in seconds
     */
    private double currTime = 0.0;

    /**
     * The current time step in the simulation
     */
    private long currStep = 0;

    /**
     * Whether or not the simulation is finished
     */
    private boolean finished = false;

    /**
     * Static TEXAS model configuration data.
     */
    private ModelData modelData;

    /**
     * The list of vehicle messages on the current time step.
     */
    private List<VehicleCommand> vehicleCommands = new ArrayList<VehicleCommand>();

    /**
     * The list of vehicle injection requests for the current time step.
     */
    private List<VehicleInjectionRequest> vehicleInjectionRequests = new ArrayList<VehicleInjectionRequest>();

    /**
     * A reusable structure to hold the information for vehicle insertions.
     */
    private SIMPRO_VehicleInjectionData simvid = new SIMPRO_VehicleInjectionData();

    /**
     * A reusable structure to hold any errors caused by vehicle injections.
     */
    private SIMPRO_VehicleInjectionError simvie = new SIMPRO_VehicleInjectionError();

    /**
     * LaneManager to help fill in values required by TEXAS for vehicle injection that we don't want
     * to trouble the user with.
     */
    private final LaneManager lm;

    /**
     * Set of error codes that won't be reported, since they shouldn't stop the execution.
     * 
     * @see org.etexascode.api.SimproInterface#mapInjectionErrorCodeToString(int)
     */
    private final Set<Integer> nonFatalErrorCodes;

    /**
     * Constructor which initializes the TEXAS simulation program called SIMPRO.
     */
    @CoberturaIgnore
    public eTEXAS() {
        SimproInterface.initializeSIMPRO();

        modelData = new ModelData();
        modelData.initialize();
        lm = UtilsStaticData.populateLaneManager(modelData, new MapDataRetriever());
        nonFatalErrorCodes = new HashSet<Integer>(Arrays.asList(5, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35));
    }

    /**
     * Get the static TEXAS model configuration data.
     * 
     * @return The static TEXAS model configuration data.
     */
    public ModelData getModelData() {
        return modelData;
    }

    /**
     * Get the current time into the simulation in seconds
     * 
     * @return A decimal value representing the current time into simulation in seconds
     */
    public double getCurrentTime() {
        return currTime;
    }

    /**
     * Get whether or not the time step simulation is complete
     * 
     * @return True if the simulation is complete, false if not
     */
    public boolean isFinished() {
        return finished;
    }

    /**
     * Get the data which is static across the entire simulation
     * 
     * @return The static data on the simulation
     */
    @CoberturaIgnore
    public StaticData getStaticData() {
        StaticData ret = new StaticData();

        ret.setDetectorManager(UtilsStaticData.populateDetectorManager());
        ret.setSignalManager(new SignalManager());
        ret.setLaneManager(UtilsStaticData.populateLaneManager(modelData, new MapDataRetriever()));
        ret.setMetaData(UtilsStaticData.populateMetaData(modelData));

        return ret;
    }

    /**
     * Get the step data for step num in the simulation.
     * 
     * @param stepNum The number of the step the user wants data on
     * @return The data for the step stepNum. Returns null if the simulation is finished.
     * @throws IllegalStateException if the step number is not the number of the next step.
     */
    @CoberturaIgnore
    public StepData getStepData(long stepNum) {
        LOGGER.trace("Enter step data...");

        if (finished) {
            return null;
        }

        DoubleByReference doublePtr = new DoubleByReference();
        LOGGER.trace("Enter simulateDT...");
        SimproJNA.INSTANCE.simulateDT(doublePtr);
        LOGGER.trace("Exit simulateDT...");
        double retTime = doublePtr.getValue();
        if (retTime < 0) {
            finished = true;
            return null;
        }

        SimproInterface.getVehicleInsertionErrors(simvie);

        currStep++;
        if (currStep >= modelData.getMaxDT()) {
            finished = true;
            return null;
        }
        // TODO: bbadillo - Add logic for exception when stepNum is incorrect.
        // if(currStep != stepNum) {
        // throw new IllegalStateException(String.format("Step requested is %s but the current step
        // is %s.", stepNum, currStep));
        // }

        this.currTime = retTime;

        StepData ret = new StepData();

        // This needs to happen before and after command processing.
        // Beforehand because VMS Messages need vehicle data to do processing.
        // and after because of injection of new vehicles.
        ret.setVehicleList(UtilsStepData.populateVehicles(modelData, new SimproJNA.SIMPRO_VehicleData()));

        // Vehicle Command Processing
        for (VehicleCommand vc : vehicleCommands) {

            VMSMessage vms = convertVehicleCommandToVMSMessage(vc, ret);
            if (vms == null) {
                StringBuilder str = new StringBuilder();
                str.append("The following Vehicle command failed to be converted to a VMS Message: ");
                str.append("Vehicle ID: ");
                str.append(vc.getVehicleID());
                str.append(".");
                LOGGER.error(str.toString());
            }
            else {
                StringBuilder str = new StringBuilder();
                str.append("Vehicle command converted successfully to VMS message: ");
                str.append(vms.toString());
                LOGGER.trace(str.toString());
                if (vms.inject()) {
                    LOGGER.trace("Successfully Injected VMS.");
                }
                else {
                    LOGGER.error("Failed Injection of VMS Message.");
                }
            }
        }
        vehicleCommands.clear();

        SimproInterface.insertVehicles(currTime, modelData.getDTSize(), vehicleInjectionRequests, modelData.mapData, simvid, lm);
        vehicleInjectionRequests.clear();

        ret.setVehicleList(UtilsStepData.populateVehicles(modelData, new SimproJNA.SIMPRO_VehicleData()));
        ret.setSignalList(UtilsStepData.populateSignals(modelData));
        // ret.setDetectorList(UtilsStepData.populateDetectors()); TODO: Implement populateDetector
        // method

        for (int k = 0; k < simvie.ERRCNT; ++k) {

            String err = String.format("Error when inserting vehicle %d at time %f: %s", simvie.ERRIVN[k], simvie.ERRTIM[k], SimproInterface.mapInjectionErrorCodeToString(simvie.ERRNUM[k]));
            if (!nonFatalErrorCodes.contains(simvie.ERRNUM[k])) {
                // the injected vehicle was eliminated due to the error, so tell the user and stop
                // the execution
                // this keeps messages such as "vehicle forced to use different desired outbound
                // approach" from stopping everything
                ret.addErrorMessage(err);
            }
            else { // TEXAS could deal with the error, so just log it to be safe
                LOGGER.debug(err);
            }
        }

        LOGGER.trace("Exit step data...");
        return ret;
    }

    @CoberturaIgnore
    public void updateSignalData(List<SignalIndication> signals) {
        // TODO: ablatt - fill in this method
    }

    @CoberturaIgnore
    public void updateVehicleData(List<Vehicle> vehicles) {
        // TODO: ablatt - fill in this method
    }

    /**
     * Returns the list of vehicle commands.
     * 
     * @return The list of vehicle commands.
     */
    public List<VehicleCommand> getVehicleCommands() {
        return vehicleCommands;
    }

    /**
     * Adds a vehicle command to the list of commands to inject during the next time step.
     * 
     * @param command The command to add.
     */
    public void addVehicleCommand(VehicleCommand command) {
        VehicleCommand remove = null;
        if (vehicleCommands == null) {
            vehicleCommands = new ArrayList<VehicleCommand>();
        }
        for (VehicleCommand v : vehicleCommands) {
            if (command.getVehicleID() == v.getVehicleID() && command.getClass().equals(v.getClass())) {
                remove = v;
            }
        }
        vehicleCommands.remove(remove);
        vehicleCommands.add(command);
    }

    /**
     * Adds a vehicle injection request to the list of requests.
     * 
     * @param request The request to add.
     */
    public void addVehicleInjectionRequest(VehicleInjectionRequest request) {
        if (vehicleInjectionRequests == null) {
            vehicleInjectionRequests = new ArrayList<VehicleInjectionRequest>();
        }
        vehicleInjectionRequests.add(request);
    }

    /**
     * Converts a vehicle command to VMS message.
     * 
     * @param command The vehicle command object.
     * @return A VMS message.
     */
    private VMSMessage convertVehicleCommandToVMSMessage(VehicleCommand command, StepData data) {
        // Get simulation data.
        VMSMessage message = new VMSMessage();
        StaticData stcData = getStaticData();
        LaneManager laneMgr = stcData.getLaneManager();

        // Verify that the vehicle is available to add the command.
        List<Vehicle> vehicles = data.getVehicles();
        Vehicle selected = null;
        for (Vehicle v : vehicles) {
            if (v.getVehicleID() == command.getVehicleID()) {
                selected = v;
            }
        }
        if (vehicles.size() <= 0 || selected == null) {
            return null;
        }

        // Calculate end lane and get approach of the lane the
        // vehicle is in.
        int count = 0;
        int approach = laneMgr.getLaneById(selected.getLaneID()).getApproachId();
        for (int key : laneMgr.getLanes().keySet()) {
            Lane lane = laneMgr.getLaneById(key);
            if (lane.getApproachId() == approach) {
                count = count + 1;
            }
        }

        // Calculate lane length.
        double width = (stcData.getMetaData().getSimWidth() - 200) / 2;

        // Set the message type.
        message.setType(VMSMessage.DRIVER_IVMS);

        // Convert command ID to the corresponding eTexas API command ID.
        // Set vehicle parameter, in this case speed.
        if (command instanceof VehicleSpeedCommand) {
            int speedCommand = ((VehicleSpeedCommand)command).getSpeedCommand();
            if (speedCommand == VehicleSpeedCommand.MAX_ACCELERATE_TO_XX) {
                message.setMessage(VMSMessage.ETEXAS_API_MAX_ACCELERATE);
            }
            else if (speedCommand == VehicleSpeedCommand.MAX_DECELERATE_TO_XX) {
                message.setMessage(VMSMessage.ETEXAS_API_MAX_DECELERATE);
            }
            else if (speedCommand == VehicleSpeedCommand.NORMAL_ACCELERATE_TO_XX) {
                message.setMessage(VMSMessage.ETEXAS_API_NORMAL_ACCELERATE);
            }
            else if (speedCommand == VehicleSpeedCommand.NORMAL_DECELERATE_TO_XX) {
                message.setMessage(VMSMessage.ETEXAS_API_NORMAL_DECELERATE);
            }
            message.setParameter((((VehicleSpeedCommand)command).getSpeed()) * MS_TO_MPH);
        }
        else if (command instanceof VehicleLaneChangeCommand) {
            int laneCommand = ((VehicleLaneChangeCommand)command).getLaneCommand();
            if (laneCommand == VehicleLaneChangeCommand.CHANGE_LANE_LEFT) {
                message.setMessage(VMSMessage.ETEXAS_API_LEFT_LANE);
            }
            else if (laneCommand == VehicleLaneChangeCommand.CHANGE_LANE_RIGHT) {
                message.setMessage(VMSMessage.ETEXAS_API_RIGHT_LANE);
            }
            message.setParameter(0.0);
        }
        else if (command instanceof VehicleDestinationCommand) {
            int destCommand = ((VehicleDestinationCommand)command).getDestCommand();
            if (destCommand == VehicleDestinationCommand.STAY_STRAIGHT_AT_INTERSECTION) {
                message.setMessage(VMSMessage.NONE);
            } // NO ETEXAS API AVAILABLE.
            else if (destCommand == VehicleDestinationCommand.TURN_LEFT_AT_INTERSECTION) {
                message.setMessage(VMSMessage.NONE);
            } // NO ETEXAS API AVAILABLE.
            else if (destCommand == VehicleDestinationCommand.TURN_RIGHT_AT_INTERSECTION) {
                message.setMessage(VMSMessage.NONE);
            } // NO ETEXAS API AVAILABLE.
            message.setParameter(0.0);
        }

        // Set the remaining parameters.
        message.setStartTime(currTime + 1);
        message.setActiveTime(modelData.maxDT - (currTime + 1));
        message.setApproach(approach);
        message.setStartLane(1);
        message.setEndLane(count);
        message.setDistStart(0.0);
        message.setDistEnd(width);
        message.setVehicleID(command.getVehicleID());
        message.setDistType(1);
        message.setDistMean(0.0);
        message.setDistParam(0.0);
        return message;
    }

    /**
     * Adds a signal command to the list of commands to inject during the next time step.
     * 
     * @param command The command to add.
     */
    @CoberturaIgnore
    public void addSignalCommand(SignalCommand command) {
        SimproJNA lib = SimproJNA.INSTANCE;
        if (command.getSignalCommand() == SignalCommand.CHANGE_SIGNAL) {
            lib.changeSignal(command.getTime());
        }
        else if (command.getSignalCommand() == SignalCommand.HOLD_SIGNAL) {
            lib.holdSignalChange(command.getTime());
        }
    }

    @CoberturaIgnore
    public void close() {} // TODO: ablatt - implement this method
}