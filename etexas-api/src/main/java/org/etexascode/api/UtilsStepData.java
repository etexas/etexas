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
import java.util.List;

import org.etexascode.api.SimproJNA.SIMPRO_VehicleData;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.StepData;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.Vehicle.VEHICLE_TYPE;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is designed to populate the various components of StepData. Its 3 major input methods
 * are populateVehicles(StepData step, ModelData modelData), populateSignals(StepData step,
 * ModelData modelData) and populateDetectors(StepData step).
 * 
 * @author ablatt
 */
public class UtilsStepData {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(UtilsStepData.class);

    private static final int UNCONTROLLED = 2;

    private static final int YIELD_SIGN = 3;

    private static final int STOP_SIGN = 4;

    /**
     * A method for populating the list of Vehicles in the step data using the model data provided.
     * 
     * @param modelData The model data.
     * @param data The vehicle data.
     * @return The list of vehicles.
     */
    public static List<Vehicle> populateVehicles(ModelData modelData, SIMPRO_VehicleData data) {
        List<Vehicle> vehicles = new ArrayList<Vehicle>();
        SimproInterface.getVehicleData(data);

        // Only iterate through indexes 1 through NVE (length-2) because
        // the values at index 0 and NVE+1 (length-1) are not vehicles.
        for (int i = 1; i < data.IQ.length - 1; i++) {
            // If IQ[i]=0 then the vehicle at index i-1 is not currently in the system.
            int vehicleID = data.IQ[i];
            int index = i - 1;
            // If the vehicleID == 0 then it is not in use and should be skipped.
            if (vehicleID != 0) {
                Vehicle v = new Vehicle(vehicleID, (UtilsUnitConversion.convertFeetToCentimeters(data.LEN[index])), (UtilsUnitConversion.convertFeetToCentimeters(data.PWID[index])),
                        (UtilsUnitConversion.convertFeetToCentimeters(data.FBX[index] - modelData.getCenterX())),
                        (UtilsUnitConversion.convertFeetToCentimeters(data.FBY[index] - modelData.getCenterY())), 0.0);
                v.setVehicleID(vehicleID);
                v.setLength(UtilsUnitConversion.convertFeetToCentimeters(data.LEN[index]));
                v.setWidth(UtilsUnitConversion.convertFeetToCentimeters(data.PWID[index]));
                v.setSpeed(UtilsUnitConversion.convertFeetToMeters(data.VEL[index]));
                v.setAcceleration(UtilsUnitConversion.convertFeetToCentimeters(data.ACC[index]));
                v.setX(UtilsUnitConversion.convertFeetToCentimeters(data.FBX[index] - modelData.getCenterX()));
                v.setY(UtilsUnitConversion.convertFeetToCentimeters(data.FBY[index] - modelData.getCenterY()));
                v.setHeading(data.PHEAD[index]);
                v.setLaneID(data.LANE[index]);
                v.setHeight(UtilsUnitConversion.convertFeetToCentimeters(data.PHEIGHT[index]));
                if (data.PTYPE[index] == 0) {
                    v.setType(VEHICLE_TYPE.CAR);
                }
                else if (data.PTYPE[index] == 1) {
                    v.setType(VEHICLE_TYPE.BUS);
                }
                else if (data.PTYPE[index] == 2) {
                    v.setType(VEHICLE_TYPE.TRACTOR_TRAILER);
                }
                v.setBrakePressed(false); // THIS NEEDS TO BE RETRIEVED.
                vehicles.add(v);
                if (v.getLaneID() == -1) {
                    LOGGER.debug("Vehicle {} does not have an assigned lane.", v.getVehicleID());
                }
                else {
                    LOGGER.trace("Vehicle {} has been assigned lane {}.", v.getVehicleID(), v.getLaneID());
                }
            }
        }

        return vehicles;
    }

    /**
     * Populates the signal data with the current state of the signal in TEXAS.
     * 
     * @param modelData The current step which contains the list of signals to populate
     * @return The list of signal indications.
     */
    public static List<SignalIndication> populateSignals(ModelData modelData) {
        List<SignalIndication> signals = new ArrayList<SignalIndication>();
        SignalDataRetriever signalDataWrapper = new SignalDataRetriever();
        // Iterate over all lanes in the model
        MapDataRetriever mapData = modelData.getMapData();
        int numLanes = mapData.getNRLAN();
        for (int laneId = 1; laneId <= numLanes; laneId++) {
            // If the current lane is an inbound lane, then create the movement state
            int inboundID = mapData.getIBLN(laneId);
            if (inboundID == 0) {
                continue; // the lane is an outbound lane - skip this lane
            }

            int LCONTR = mapData.getLCONTR(laneId);
            SignalIndication[] sis = null;
            if (LCONTR == UNCONTROLLED) {
                SignalIndication si = new SignalIndication();

                si.setLaneId(laneId);
                si.setColorIndication(SignalIndication.Color.NONE);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.UNCONTROLLED);

                sis = new SignalIndication[] { si };
            }
            else if (LCONTR == YIELD_SIGN) {
                SignalIndication si = new SignalIndication();

                si.setLaneId(laneId);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.YIELD_SIGN);

                sis = new SignalIndication[] { si };
            }
            else if (LCONTR == STOP_SIGN) {
                SignalIndication si = new SignalIndication();

                si.setLaneId(laneId);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.STOP_SIGN);

                sis = new SignalIndication[] { si };
            }
            else {
                sis = getSignalsFromLight(signalDataWrapper, signalDataWrapper.getICAMPC(), inboundID);
                double timeToChange = getTimeToChange(signalDataWrapper, inboundID);

                for (SignalIndication s : sis) {
                    s.setLaneId(laneId);
                    s.setTimeToChange(timeToChange);
                }
            }
            signals.addAll(Arrays.asList(sis));
        }
        return signals;
    }

    /**
     * Populates the list of detector events in this step
     * 
     * @param step The step to populate
     */
    public static void populateDetectors(StepData step) {
        // TODO: ablatt - populate this with detector events once the actual populateDetectorManager
        // method is working properly
    }

    /**
     * Gets an array of signal indications from the light associated with laneNumber
     * 
     * @param signalDataParam Utility for retrieving signal information from TEXAS
     * @param camStackPosition
     * @param laneNumber The lane identifier to get the signal state of
     * @return All the signal states for lane laneNumber
     */
    static SignalIndication[] getSignalsFromLight(SignalDataRetriever signalDataParam, int camStackPosition, int laneNumber) {
        // Get the signal integer
        int simproSignal = signalDataParam.getISISET(camStackPosition, laneNumber);

        /*
         * All possibilities are as follows: Signal indication subscripted by cam stack position and
         * inbound lane number ICAMPC, IBLN) (1-25) 1=Signal for movement is green and checked for
         * u-turn and left turn AG 2= Signal for movement is amber and decision is made to go or
         * stop AA 3=Signal for movement is red and vehicle is stopped at stop line AR 4= Signal for
         * movement is protected green and intersection conflicts are not checked AP 5 =Left
         * =green(1) others=Amber(2) LGA 6 = Left= green (1) others=Red (3) LGR; 7= Left= amber (2)
         * others=Green (1) LAG 8 = Left= amber (2) others=Red (3) LAR; 9= Left= red (3) others=
         * Green (1) LRG 10= Left= red (3) others=Amber (2) LRA; 11= Straight=green (1) others =
         * Amber (2) SGA 12=Straight=Green (1) others= Red (3) SGR; 13= Straight=amber (2) others=
         * green (1) SAG 14= Straight= amber(2) others=Red (3) SAR 15= Straight=red(3) others=Green
         * (1) SRG 16= Straight= red (3) others= Amber (2) SRA; 17=Right = green (1) others=Amber
         * (2) RGA 18= Right=green (1) others=Red (3) RGR; 19= Right=amber (2) others=Green (1) RAG
         * 20= Right= amber (2) others=Red (3) RAR 21= Right= red (3) others= Green (1) RRG 22=
         * Right = red (3) others= Amber (2) RRA 23= Left = protected green (4) others = Green (1)
         * LPG; 24= Left = protected green (4) others= Amber(2) LPA 25= Left = protected green (4)
         * others = Red (3) LPR
         */
        switch (simproSignal) {
            case 0:
                SignalIndication si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.NONE);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.UNKNOWN);

                return new SignalIndication[] { si };
            case 1:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si };
            case 2:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si };
            case 3:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si };
            case 4:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si };
            case 5:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.FLASHING);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                SignalIndication si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 6:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.FLASHING);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 7:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.FLASHING);
                si2.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                return new SignalIndication[] { si, si2 };
            case 8:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.FLASHING);
                si2.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                return new SignalIndication[] { si, si2 };
            case 9:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.GREEN);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 10:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 11:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 12:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 13:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.GREEN);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 14:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                return new SignalIndication[] { si, si2 };
            case 15:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                return new SignalIndication[] { si, si2 };
            case 16:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.BALL);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.STRAIGHT_ARROW);

                return new SignalIndication[] { si, si2 };
            case 17:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 18:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 19:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.GREEN);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 20:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.YELLOW);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 21:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.GREEN);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 22:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.RED);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.RIGHT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 23:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.GREEN);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 24:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.YELLOW);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            case 25:
                si = new SignalIndication();

                si.setLaneId(laneNumber);
                si.setColorIndication(SignalIndication.Color.GREEN);
                si.setStateIndication(SignalIndication.State.STEADY);
                si.setTypeIndication(SignalIndication.Type.LEFT_ARROW);

                si2 = new SignalIndication();

                si2.setLaneId(laneNumber);
                si2.setColorIndication(SignalIndication.Color.RED);
                si2.setStateIndication(SignalIndication.State.STEADY);
                si2.setTypeIndication(SignalIndication.Type.BALL);

                return new SignalIndication[] { si, si2 };
            default:
                throw new AssertionError("Invalid value from ISISET detected in eTEXAS API");
        }
    }

    /**
     * Get the time remaining before the signal on the lane changes state. This method works for
     * both pre-timed and NEMA controlled simulations.
     * 
     * @param signalDataWrapper Utility for retrieving signal information from TEXAS
     * @param inboundId Lane identifier for which to retrieve the time to change.
     * @return Time to change (in seconds) of the specified inbound lane.
     */
    static double getTimeToChange(SignalDataRetriever signalDataWrapper, int inboundId) {
        // TODO: bbadillo - This code can benefit from using TRTCMN and TRTCMX
        // in SIMPRO. Which purportedly can be used for pretimed and NEMA intersections.

        // First get the time remaining in the current phase.
        double timeToChange = signalDataWrapper.getTR();

        // Track the current signal state of this lane.
        int currState = signalDataWrapper.getISISET(signalDataWrapper.getICAMPC(), inboundId);

        // Look through future phases until the signal state changes to add future phase
        // times to the time remaining which will give the time to change.
        int lookAheadIndex = signalDataWrapper.getICAMPC();
        for (int n = 0; n < signalDataWrapper.getNCAMSP(); n++) {
            // Loop through all values in the ISISET array at least once.
            lookAheadIndex++;
            if (lookAheadIndex > signalDataWrapper.getNCAMSP()) {
                lookAheadIndex = 1;
            }

            if (signalDataWrapper.getISISET(lookAheadIndex, inboundId) == currState) {
                // If the signal state is the same, add its time to the time remaining.
                timeToChange = timeToChange + signalDataWrapper.getTCAMSP(lookAheadIndex);
            }
            else {
                // If the signal state is different, stop accumulating time remaining.
                break;
            }
        }

        return timeToChange;
    }
}
