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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.etexascode.api.SimproJNA.SIMPRO_DTData;
import org.etexascode.api.SimproJNA.SIMPRO_DetectorData;
import org.etexascode.api.SimproJNA.SIMPRO_MapData;
import org.etexascode.api.SimproJNA.SIMPRO_NTCIP1202Data;
import org.etexascode.api.SimproJNA.SIMPRO_NTCIP1202PhaseData;
import org.etexascode.api.SimproJNA.SIMPRO_SignalData;
import org.etexascode.api.SimproJNA.SIMPRO_VehicleData;
import org.etexascode.api.SimproJNA.SIMPRO_VehicleInjectionData;
import org.etexascode.api.SimproJNA.SIMPRO_VehicleInjectionError;
import org.etexascode.interrep.datamodel.Lane;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.VehicleInjectionRequest;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * A wrapper class to hide data management logic needed in the Java SimproJNA interface. Part of the
 * Public TEXAS Model Java API
 * 
 * @author bbadillo
 * @author egaebel
 * @author janway
 */
public class SimproInterface {

    /**
     * Static logger.
     */
    static private final Logger LOGGER = LoggerFactory.getLogger(SimproInterface.class);

    /**
     * A one-time initialization method for SIMPRO
     */
    static public void initializeSIMPRO() {
        SimproJNA.INSTANCE.initcn();
        SimproJNA.INSTANCE.initModel();
        SimproJNA.INSTANCE.initExecution();
    }

    /**
     * Change the signal in the simulation at some specified time.
     * 
     * @param timeToChange The time in seconds at which point the signal shall change.
     */
    static public void changeSignal(double timeToChange) {
        SimproJNA.INSTANCE.changeSignal(timeToChange);
    }

    /**
     * Hold the signal in the simulation an additional amount of time.
     * 
     * @param holdTime The additional amount of time to hold the signal.
     */
    static public void holdSignal(double holdTime) {
        SimproJNA.INSTANCE.holdSignalChange(holdTime);
    }

    /**
     * Getter for data within SIMPRO that represents the intersection under simulation NOTE:
     * Intentionally package-level access to limit use of SIMPRO_MapData class to within package for
     * maintainability reasons.
     * 
     * @param mapdata The SIMPRO_MapData class that represents the data structure returned by the C
     *        function call
     */
    static void getMAPData(SIMPRO_MapData mapdata) {

        SimproJNA.INSTANCE.getMAPData(mapdata.intVals, mapdata.IBLN, mapdata.ISNA, mapdata.LIBAR, mapdata.LOBAR, mapdata.LCONTR, mapdata.LGEOM, mapdata.LTURN, mapdata.LWID, mapdata.BASELX,
                mapdata.BASELY, mapdata.ENDLNX, mapdata.ENDLNY, mapdata.ISLIM, mapdata.NLANES);

        // Split over 2 calls because we needed to get the size of this array.
        mapdata.LIBA = new int[mapdata.intVals[SimproJNA.NIBAINDEX]];
        SimproJNA.INSTANCE.getMoreMAPData(mapdata.LLANES, mapdata.LIBA);
    }

    /**
     * Getter for data within SIMPRO that represents the intersection signals during simulation
     * NOTE: Intentionally package-level access to limit use of SIMPRO_SPATData class to within
     * package for maintainability reasons.
     * 
     * @param spatData The SIMPRO_SignalData class that represents the data structure returned by
     *        the C function call
     */
    static void getSPATData(SIMPRO_SignalData spatData) {

        SimproJNA.INSTANCE.getSPATData(spatData.doubleVals, spatData.TCAMSP, spatData.intVals, spatData.ICAMPH, spatData.INTER, spatData.ISISET);

    }

    /**
     * Getter for data within SIMPRO that represents the data from all vehicles during simulation
     * such as position, velocity, etc. NOTE: Intentionally package-level access to limit use of
     * SIMPRO_SPATData class to within package for maintainability reasons.
     * 
     * @param retData The SIMPRO_VehicleData class that represents the data structure returned by
     *        the C function call
     */
    static void getVehicleData(SIMPRO_VehicleData retData) {

        DoubleByReference DECBRK = new DoubleByReference(retData.DECBRK);

        SimproJNA.INSTANCE.getVehicleData(retData.FBX, retData.FBY, retData.IQ, retData.INUSE, retData.CLASS, retData.LANE, retData.VEL, retData.LEN, retData.PHEAD, retData.PANGLE, retData.PWID,
                retData.ACC, DECBRK, retData.PHEIGHT, retData.PTYPE);

        retData.DECBRK = DECBRK.getValue();
    }

    static void getNTCIP1202Data(SIMPRO_NTCIP1202Data ntcipData) {
        SimproJNA.INSTANCE.getNTCIP1202(ntcipData.DETEC, ntcipData.NPEDS, ntcipData.PEDETT, ntcipData.VEHDET, ntcipData.VEHSTA);
    }

    static void getNTCIPPhaseData(SIMPRO_NTCIP1202PhaseData ntcipData) {
        SimproJNA.INSTANCE.getNTCIPPhaseData(ntcipData.OVRGRP, ntcipData.PHASET, ntcipData.PHASEG);
    }

    static void getDetectorInfo(SIMPRO_DetectorData data) {
        SimproJNA.INSTANCE.getDetectorInfo(data.PLOOPS, data.PCLER, data.PCROS, data.PTRIP, data.PSTAT, data.PNLDLN, data.PSTRT, data.PSTOP, data.PITYPE, data.PLLDLN, data.PLDA);
    }

    static void getDTData(SIMPRO_DTData data) {

        DoubleByReference pdtRef = new DoubleByReference(data.PDT);
        DoubleByReference psimti = new DoubleByReference(data.PSIMTI);
        SimproJNA.INSTANCE.getDTData(pdtRef, psimti);

        data.PDT = pdtRef.getValue();
        data.PSIMTI = psimti.getValue();
    }

    /**
     * Inserts vehicles into the simulation.
     * 
     * @param simTime The simulation time when the insertion happens.
     * @param stepSize The step size of the simulation.
     * @param vehs The list of vehicles to insert.
     * @param mdr MapDataRetriever used to get Entry approach from entry lane.
     * @param data A reusable structure to hold the vehicles information.
     */
    static void insertVehicles(double simTime, double stepSize, List<VehicleInjectionRequest> vehs, MapDataRetriever mdr, SIMPRO_VehicleInjectionData data, LaneManager lm) {
        data.VIDTIM = simTime;
        data.VIDDT = stepSize;
        data.NVID = Math.min(25, vehs.size());
        LOGGER.debug(String.format("SimTime: %.2f, Step Size: %.2f, Number of Vehicles to Insert: %d", simTime, stepSize, data.NVID));

        // defaults
        int driver_class_number = 1;
        boolean print_logout_stats = false;
        boolean use_U_turn = false;
        boolean emergency_vehicle = false;

        // TODO: janway - should find out and document what these do. Using default values provided
        // by Tom for now.
        double forced_stop_time = 0; // seconds. 0 -> no forced stop.
        int forced_stop_link_number = 0;
        double forced_stop_position = 0.0; // ft
        double forced_stop_dwell_time = 0.0; // seconds
        double forced_go_time = 0; // seconds. 0 -> no forced go.
        double forced_go_active_time = 0.0; // seconds
        double forced_run_red_time = 0; // seconds. 0 -> no forced run red.
        double forced_run_red_active_time = 0.0; // seconds

        double jerk = 0.0;

        // find the possible outbound approaches
        Set<Integer> outboundApproaches = new HashSet<Integer>();
        for (Lane l : lm.getLanes().values()) {
            if (l.getType().equals(Lane.OUTBOUND)) {
                outboundApproaches.add(l.getApproachId());
            }
        }

        int outbound_approach = outboundApproaches.iterator().next();

        for (int k = 0; k < data.NVID; ++k) {
            VehicleInjectionRequest currVeh = vehs.get(k);

            int laneID = currVeh.getVehicle().getLaneID();
            int approachID = 0;
            int laneIdForApproach = 0;

            for (int j = 1; j <= mdr.getNIBA(); ++j) {
                int tempApproachID = mdr.getLIBA(j);
                for (int tempLaneIdForApproach = 1; tempLaneIdForApproach <= mdr.getNLANES(tempApproachID); ++tempLaneIdForApproach) {
                    if (mdr.getLLANES(tempLaneIdForApproach, tempApproachID) == laneID) {
                        laneIdForApproach = tempLaneIdForApproach;
                        approachID = tempApproachID;
                    }
                }
            }

            data.VIDQIT[k] = currVeh.getInjectionTime();
            data.VIDVCN[k] = currVeh.getVehicle().getType().getValue();
            data.VIDDCN[k] = driver_class_number;
            data.VIDDSP[k] = (int)UtilsUnitConversion.convertMetersToFeet(currVeh.getVehicle().getSpeed());
            data.VIDOBN[k] = outbound_approach;
            data.VIDIBN[k] = approachID;
            data.VIDILN[k] = laneIdForApproach;
            data.VIDPLO[k] = print_logout_stats ? 1 : 0;
            data.VIDFUT[k] = use_U_turn ? 1 : 0;
            data.VIDFST[k] = forced_stop_time;
            data.VIDFSL[k] = forced_stop_link_number;
            data.VIDFSP[k] = UtilsUnitConversion.convertMetersToFeet(forced_stop_position);
            data.VIDFSD[k] = forced_stop_dwell_time;
            data.VIDFGT[k] = forced_go_time;
            data.VIDFGA[k] = forced_go_active_time;
            data.VIDFRT[k] = forced_run_red_time;
            data.VIDFRA[k] = forced_run_red_active_time;
            data.VIDIVN[k] = currVeh.getVehicle().getVehicleID();
            data.VIDEMV[k] = emergency_vehicle ? 1 : 0;
            data.VIDSPD[k] = UtilsUnitConversion.convertMetersToFeet(currVeh.getVehicle().getSpeed());
            data.VIDACC[k] = UtilsUnitConversion.convertMetersToFeet(currVeh.getVehicle().getAcceleration());
            data.VIDSLP[k] = UtilsUnitConversion.convertMetersToFeet(jerk);

            LOGGER.debug(String
                    .format("Inserting Vehicle %d (id=%d)%nQIN=%.4f, Driver class=%d, Target speed=%d, Target approach=%d, inbound approach=%d, lane=%d, initial speed=%.4f, initial acceleration=%.4f, car type=%d",
                            k + 1, data.VIDIVN[k], data.VIDQIT[k], data.VIDDCN[k], data.VIDDSP[k], data.VIDOBN[k], data.VIDIBN[k], data.VIDILN[k], data.VIDSPD[k], data.VIDACC[k], data.VIDVCN[k]));
        }

        SimproJNA.INSTANCE.injectVehicles(data.VIDTIM, data.VIDDT, data.NVID, data.VIDQIT, data.VIDVCN, data.VIDDCN, data.VIDDSP, data.VIDOBN, data.VIDIBN, data.VIDILN, data.VIDPLO, data.VIDFUT,
                data.VIDFST, data.VIDFSL, data.VIDFSP, data.VIDFSD, data.VIDFGT, data.VIDFGA, data.VIDFRT, data.VIDFRA, data.VIDIVN, data.VIDEMV, data.VIDSPD, data.VIDACC, data.VIDSLP);
    }

    static void getVehicleInsertionErrors(SIMPRO_VehicleInjectionError vie) {
        IntByReference vieERRCNT = new IntByReference(vie.ERRCNT);

        SimproJNA.INSTANCE.getInjectionErrors(vie.ERRTIM, vie.ERRIVN, vie.ERRNUM, vieERRCNT);

        vie.ERRCNT = vieERRCNT.getValue();
    }

    static String mapInjectionErrorCodeToString(int code) {
        switch (code) {
            case 1:
                return "Vehicle eliminated - Queue-in time is < (simTime - stepSize) or >= simTime.";
            case 2:
                return "Vehicle eliminated - Queue-in time is < (simTime - stepSize) or >= simTime.";
            case 3:
                return "Vehicle eliminated - Vehicle class number is less than one or greater than the number of vehicle classes.";
            case 4:
                return "Vehicle eliminated - Driver class number is less than one or greater than the number of driver classes.";
            case 5:
                return "Vehicle eliminated - Desired speed is less than 1 ft/sec or greater than the maximum desired speed.";

            case 6:
                return "Vehicle eliminated - Desired outbound approach number is not on the list of outbound approach numbers.";
            case 7:
                return "Vehicle eliminated - Inbound approach number is not on the list of inbound approach numbers.";
            case 8:
                return "Vehicle eliminated - Inbound lane number is less than one or greater than the number of lanes for inbound approaches.";
            case 9:
                return "Vehicle eliminated - Invalid input for whether indiviudal statistics should be printed at logout (should be 0 or 1).";
            case 10:
                return "Vehicle eliminated - Invalid input for whether the vehicle should try to use the free U-turn lane at a diamond interchange (should be 0 or 1).";

            case 11:
                return "Vehicle eliminated - Forced stop time is < queue-in time or > max simTime.";
            case 12:
                return "Vehicle eliminated - Forced stop link number is less than the number of paths, equal to 0, or greater than the number of approaches.";
            case 13:
                return "Vehicle eliminated - Forced stop link number for approach is not on the list of inbound or outbound approaches.";
            case 14:
                return "Vehicle eliminated - Forced stop position on link is less than 0.0 or greater than the length of the path.";
            case 15:
                return "Vehicle eliminated - Forced stop position on link is less than MINLLN or greater than MAXLLN.";

            case 16:
                return "Vehicle eliminated - Forced stop dwell time is less than 0.0 or greater than MSTSEC.";
            case 17:
                return "Vehicle eliminated - Forced go time is less than queue-in time or greater than max simTime.";
            case 18:
                return "Vehicle eliminated - Forced go active time is less than 0.0 or greater than MSTSEC.";
            case 19:
                return "Vehicle eliminated - Forced run red signal time is less than queue-in time or greater than max simTime.";
            case 20:
                return "Vehicle eliminated - Forced run red signal active time is less than 0.0 or greater than MSTSEC.";

            case 21:
                return "Vehicle eliminated - Vehicle ID is less than 0.";
            case 22:
                return "Vehicle eliminated - Invalid input for whether the vehicle is an emergency vehicle (should be 0 or 1).";
            case 23:
                return "Vehicle eliminated - Initial speed is less than 0.0 or greater than the maximum speed.";
            case 24:
                return "Vehicle eliminated - Initial acceleration is less than the max negative acceleration or greater than the max positive aceleration.";
            case 25:
                return "Vehicle eliminated - Initial jerk rate is out of bounds.";

            case 26:
                return "Vehicle eliminated - Lane full.";
            case 27:
                return "Vehicle eliminated - Another vehicle already set to login to lane.";
            case 28:
                return "Input initial speed is greater than calculated entry speed.";
            case 29:
                return "Vehicle forced to use different desired outbound approach.";
            case 30:
                return "Vehicle forced to use different desired outbound approach for free U-turn.";

            case 31:
                return "Vehicle forced to use different intersection path.";
            case 32:
                return "Lane change needs more length in first part of lane.";
            case 33:
                return "Block zone intrusion - vehicle in blocked part of lane.";
            case 34:
                return "Vehicle involved in a major collision.";
            case 35:
                return "Vehicle involved in a clear intrusion zone.";
            default:
                return "Undefined Error Code";
        }
    }
}
