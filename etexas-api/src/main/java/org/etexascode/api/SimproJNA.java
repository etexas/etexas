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

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Structure;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * This class is the interface to all routines in the eTEXAS SIMPRO command (Fortran)
 * 
 * @author bbadillo
 * @author emyers
 */
public interface SimproJNA extends Library {

    // Constant values
    public static final int NCM = 72;

    public static final int NRG = 4;

    public static final int NON = 16;

    public static final int NIL = 25;

    public static final int NIS = 10;

    public static final int NOL = 25;

    public static final int NLA = 50;

    public static final int NAL = 6;

    public static final int NAP = 16;

    public static final int NVE = 500;

    public static final int NVEP1 = 502;

    public static final int NLS = 30;

    // Map data int value index
    public static final int NRLAN = 0;

    public static final int NIBAINDEX = 1;

    // SPAT data int value index
    public static final int ICONTR = 0;

    public static final int NIBL = 1;

    public static final int IARRPH = 2;

    public static final int ICAMPC = 3;

    public static final int ICAMPO = 4;

    public static final int ICPHAS = 5;

    public static final int IGO = 6;

    public static final int NCAMSP = 7;

    // SPAT data float value index
    public static int TP = 0;

    public static int TR = 1;

    // Intersection Control types
    public static final int ICUNCT = 1;

    public static final int ICYELD = 2;

    public static final int ICLTAS = 3;

    public static final int ICAWST = 4;

    public static final int ICPSIG = 5;

    public static final int ICSACT = 6;

    public static final int ICFACT = 7;

    public static final int ICTDF3 = 8;

    public static final int ICTDF4 = 9;

    public static final int ICTDF6 = 10;

    public static final int ICTDF7 = 11;

    public static final int ICDDF3 = 12;

    public static final int ICDDF4 = 13;

    public static final int ICDDF6 = 14;

    public static final int ICDDF7 = 15;

    public static final int ICNEMA = 16;

    public static final int ICNEMV = 17;

    public static final int ICHDWR = 18;

    // VMS Message messages
    public static final int DYNAMIC_MESSAGE_SIGN = 1;

    public static final int DRIVER_MESSAGE = 2;

    public static final int VEHICLE_MESSAGE = 3;

    public static final int SET_SPEED_USING_NORMAL_ACCELERATION = 1;

    public static final int SET_SPEED_USING_MAX_ACCELERATION = 2;

    public static final int STOP_AT_INTERSECTION = 3;

    public static final int SET_STOP_LOCATION = 4;

    public static final int STOP_MAX_DECELERATION = 5;

    public static final int STOP_USING_DECELERATION_PARAM = 6;

    public static final int CHANGE_LANES_LEFT = 7;

    public static final int CHANGE_LANES_RIGHT = 8;

    public static final int FORCED_GO = 9;

    public static final int RUN_RED_SIGNAL = 10;

    public static final int DISTRACTED_DRIVER = 11;

    // VMS Message Types
    public static final int CONSTAN = 1;

    public static final int ERLANG = 2;

    public static final int GAMMA = 3;

    public static final int LOGNRML = 4;

    public static final int NEGEXP = 5;

    public static final int SNEGEXP = 6;

    public static final int UNIFORM = 7;

    // NTCIP SIZES
    // (int[] DETEC, int[] NPEDS, int[] PEDETT, int[] VEHDET, int[] VEHSTA,
    // char[] SOVRTA, int[] OVRTAB, int[] OVRGRP, int[] PHASET, int[] PHASEC, int[] PHASEG);
    // NTCIP multiples
    public static final int MPEDDETTS = 6;

    public static final int MVEHDETS = 14;

    public static final int MVEHSTAS = 3;

    public static final int MPHASETS = 23;

    public static final int MPHASEGS = 9;

    public static final int DETECS = 1;

    public static final int NPEDSIZ = 1;

    public static final int PEDDETTS = 1530; // 6*255

    public static final int VEHDETS = 3570; // 14 * 255

    public static final int VEHSTAS = 765; // 3 * 255

    public static final int OVRGRPS = 255;

    public static final int PHASETS = 5865; // 23 * 255

    public static final int PHASEGS = 2295; // 9 * 255

    // NTCIP1202 variables
    public static final int detecNumLoopDetec = 1; // NLOOPS

    public static final int pnpedsNumPedDetecs = 1; // NPEDS

    public static final int pedettSignalPhase = 1; // PEDS

    public static final int vehdetCallExtend = 1; // LDCLEX

    public static final int vehdetPhaseCall = 2; // LDPHCL

    public static final int vehdetDetecDelay = 3; // LDDELY

    public static final int vehdetDetecExtend = 4; // LDEXTD

    public static final int vehstaDetecTrip = 1; // LDTRIP

    public static final int ovrgrpOvrLapState = 1; // IOVRLP

    public static final int phasetEntryNumber = 1; // IP

    public static final int phasetWalk = 2; // TWK

    public static final int phasetPedClearance = 3; // TPC

    public static final int phasetMinInterval = 4; // TMI

    public static final int phasetVehicleIncrement = 5; // TVI

    public static final int phasetMax1 = 6; // TM1

    public static final int phasetMax2 = 7; // TM2

    public static final int phasetClearanceInterval = 8; // TCI

    public static final int phasetAllRedInterval = 9; // TAR

    public static final int phasetRedRevert = 10; // TRR

    public static final int phasetVolDenPerAct = 11; // TIIADD

    public static final int phasetVolDenMaxInit = 12; // TIIMAX

    public static final int phasetVolDenBeforeReduct = 13; // TVITBR

    public static final int phasetVolDenToReduce = 15; // TVITTR

    public static final int phasetInitVehInterval = 21; // TIIVEH

    public static final int phasetRingPhase = 22; // PHRNG

    public static final int phasetMaxConcurrentPhase = 23; // NPHRC

    public static final int phasegEntryNumber = 1; // IP

    public static final int phasegIntervalForLight = 2; // PHINT

    public static final int phasegIntervalForPedSignal = 5; // PEDINT

    public static final int phasegCallForPhase = 8; // CLPH

    public static final int phasegPedestrianCall = 9; // PDCALL

    /**
     * This static initializer is used to load the SIMPRO library that implements the native methods
     * declared in this interface. NOTE: Intentionally package-level access to limit use of
     * SIMPRO_MapData class to within package for maintainability reasons.
     */
    static SimproJNA INSTANCE = (SimproJNA)Native.loadLibrary("SIMPRO", SimproJNA.class);

    /**
     * Fortran subroutine call: Initializes constant values. NOTE: Intentionally package-level
     * access to limit use of SIMPRO_MapData class to within package for maintainability reasons.
     */
    void initcn();

    /**
     * Fortran subroutine call: A mirror of the INITAL Subroutine except that simulation parameters
     * are read from file instead of command line. NOTE: Intentionally package-level access to limit
     * use of SIMPRO_MapData class to within package for maintainability reasons.
     */
    void initModel();

    /**
     * Fortran subroutine call: Code copied out from before the simulation loop in the EXEC
     * Subroutine. NOTE: Intentionally package-level access to limit use of SIMPRO_MapData class to
     * within package for maintainability reasons.
     */
    void initExecution();

    /**
     * The simulation routine that uses the Fortran eTEXAS SIMPRO command to perform a time step of
     * the simulation. This method returns -1.0 when the simulation is complete. NOTE: Intentionally
     * package-level access to limit use of SIMPRO_MapData class to within package for
     * maintainability reasons.
     * 
     * @param currTime A decimal value representing the current time into simulation in seconds
     */
    void simulateDT(DoubleByReference currTime);

    /**
     * Print the summary statistics from the simulation.
     */
    void printSummary();

    /**
     * Getter for data within SIMPRO that represents contants used for array initialization NOTE:
     * Intentionally package-level access to limit use of SIMPRO_ConstantsData class to within
     * package for maintainability reasons.
     * 
     * @param retData The SIMPRO_ConstantsData class that represents the data structure returned by
     *        the C function call
     */
    void getConstantsData(SIMPRO_ConstantsData retData);

    /**
     * Getter for data within SIMPRO that represents the intersection under simulation NOTE:
     * Intentionally package-level access to limit use of SIMPRO_MapData class to within package for
     * maintainability reasons
     *
     * @param intVals The integer values.
     * @param IBLN The IBLN.
     * @param ISNA The ISNA.
     * @param LIBAR The LIBAR.
     * @param LOBAR The LOBAR.
     * @param LCONTR The LCONTR.
     * @param LGEOM The LGEOM.
     * @param LTURN The LTURN.
     * @param LWID The LWID.
     * @param BASELX The BASELX.
     * @param BASELY The BASELY.
     * @param ENDLNX The ENDLNX.
     * @param ENDLNY The ENDLNY.
     * @param ISLIM The ISLIM.
     * @param NLANES The NLANES.
     */
    void getMAPData(int[] intVals, int[] IBLN, int[] ISNA, int[] LIBAR, int[] LOBAR, int[] LCONTR, int[] LGEOM, int[] LTURN, int[] LWID, double[] BASELX, double[] BASELY, double[] ENDLNX,
            double[] ENDLNY, int[] ISLIM, int[] NLANES);

    /**
     * Gets map data needed to map our lane IDs to texas' (approachID, laneID within approach)
     * needed for vehicle injection.
     * 
     * @param LLANES The LLANES.
     * @param LIBA The LIBA.
     */
    void getMoreMAPData(int[] LLANES, int[] LIBA);

    /**
     * Getter for data within SIMPRO that represents the intersection signals during simulation
     * NOTE: Intentionally package-level access to limit use of SIMPRO_SPATData class to within
     * package for maintainability reasons.
     * 
     * @param doubleVals The double values.
     * @param TCAMSP The TCAMSP.
     * @param intVals The integer values.
     * @param ICAMPH The ICAMPH.
     * @param INTER The INTER.
     * @param ISISET The ISISET.
     */
    void getSPATData(double[] doubleVals, double[] TCAMSP, int[] intVals, int[] ICAMPH, int[] INTER, int[] ISISET);

    /**
     * Getter for data within SIMPRO that represents the data from all vehicles during simulation
     * such as position, velocity, etc. NOTE: Intentionally package-level access to limit use of
     * SIMPRO_SPATData class to within package for maintainability reasons.
     * 
     * @param FBX The FBX.
     * @param FBY The FBY.
     * @param IQ The IQ.
     * @param INUSE The INUSE.
     * @param CLASS The CLASS.
     * @param LANE The LANE.
     * @param VEL The VEL.
     * @param LEN The LEN.
     * @param PHEAD The PHEAD.
     * @param PANGLE The PANGLE.
     * @param PWID The PWID.
     * @param PIACC The PIACC.
     * @param DECBRK The DECBRK.
     * @param HEIGHT The HEIGHT.
     * @param TYPE The TYPE.
     */
    void getVehicleData(double[] FBX, double[] FBY, int[] IQ, int[] INUSE, short[] CLASS, int[] LANE, double[] VEL, double[] LEN, double[] PHEAD, double[] PANGLE, double[] PWID, double[] PIACC,
            DoubleByReference DECBRK, int[] HEIGHT, int[] TYPE);

    /**
     * Extend the signal time in the simulation. NOTE: Intentionally package-level access to limit
     * use of SIMPRO_SPATData class to within package for maintainability reasons.
     * 
     * @param holdTime The time in seconds to extend the signal time.
     */
    void holdSignalChange(double holdTime);

    /**
     * Change the signal in the simulation at some specified time. NOTE: Intentionally package-level
     * access to limit use of SIMPRO_SPATData class to within package for maintainability reasons.
     * 
     * @param timeToChange The time in seconds at which point the signal shall change.
     */
    void changeSignal(double timeToChange);

    /**
     * Getter for data within SIMPRO that represents the data from Detectors and actuated light
     * signals NOTE: Intentionally package-level access to limit use of SIMPRO_NTCIP1202Data class
     * to within package for maintainability reasons.
     */
    /**
     * Getter for data within SIMPRO that represents the data from Detectors and actuated light
     * signals NOTE: Intentionally package-level access to limit use of SIMPRO_NTCIP1202Data class
     * to within package for maintainability reasons.
     * 
     * @param DETEC The DETEC.
     * @param PNPEDS The PNPEDS.
     * @param PEDETT The PEDETT.
     * @param VEHDET The VEHDET.
     * @param VEHSTA The VEHSTA.
     */
    void getNTCIP1202(int[] DETEC, int[] PNPEDS, boolean[] PEDETT, int[] VEHDET, int[] VEHSTA);

    /**
     * Getter for data within SIMPRO that represents the phase data from Detectors NOTE:
     * Intentionally package-level access to limit use of SIMPRO_NTCIP1202PhaseData class to within
     * package for maintainability reasons.
     */
    /**
     * Getter for data within SIMPRO that represents the phase data from Detectors NOTE:
     * Intentionally package-level access to limit use of SIMPRO_NTCIP1202PhaseData class to within
     * package for maintainability reasons.
     * 
     * @param OVRGRP The OVRGRP.
     * @param PHASET The PHASET.
     * @param PHASEG The PHASEG.
     */
    void getNTCIPPhaseData(int[] OVRGRP, int[] PHASET, int[] PHASEG);

    /**
     * Getter for data within SIMPRO that represents the location data from Detectors NOTE:
     * Intentionally package-level access to limit use of SIMPRO_DetectorData class to within
     * package for maintainability reasons.
     * 
     * @param PLOOPS The PLOOPS.
     * @param PCLER The PCLER.
     * @param PCROS The PCROS.
     * @param PTRIP The PTRIP.
     * @param PSTAT The PSTAT.
     * @param PNLDLN The PNLDLN.
     * @param PSTRT The PSTRT.
     * @param PSTOP The PSTOP.
     * @param PITYPE The PITYPE.
     * @param PLLDLN The PLLDLN.
     * @param PLDA The PLDA.
     */
    void getDetectorInfo(int[] PLOOPS, boolean[] PCLER, boolean[] PCROS, boolean[] PTRIP, int[] PSTAT, int[] PNLDLN, int[] PSTRT, int[] PSTOP, boolean[] PITYPE, int[] PLLDLN, int[] PLDA);

    void getBrakeStatus(double[] PIACC, double[] PIVEL);

    /**
     * TODO: Re-enable when we can figure out what is breaking getMapData
     * 
     * @param PDT The PDT.
     * @param PSIMTI The PSIMTI.
     */
    void getDTData(DoubleByReference PDT, DoubleByReference PSIMTI);

    /**
     * Injects a VMS message into the simulator.
     * 
     * @param JVMSMT Type of Message (1=Driver DMS, 2=Driver IVDMS, 3=Vehicle IVDMS)
     * @param JVMSMG Message (Between 1 and 11)
     * @param RVMSMP Message Parameter (MPH), *Only required sometimes.
     * @param RVMSST Starting Time
     * @param RVMSAT Active Time
     * @param JVMSAP Approach (+) or Intersection Path (-)
     * @param JVMSLB Beginning Lane
     * @param JVMSLE Ending Lane
     * @param RVMSPB Beginning Position
     * @param RVMSPE Ending Position
     * @param JVMSVN Vehicle Number (0 = all)
     * @param JVMSDT Reaction Time Distribution Type
     * @param RVMSDM Reaction Time Distribution Mean
     * @param RVMSDP Reaction Time Distribution Parameter
     */
    void injmsg(int JVMSMT, int JVMSMG, double RVMSMP, double RVMSST, double RVMSAT, int JVMSAP, int JVMSLB, int JVMSLE, double RVMSPB, double RVMSPE, int JVMSVN, int JVMSDT, double RVMSDM,
            double RVMSDP);

    /**
     * Injects a vehicle into the simulator.
     * 
     * @param VIDTIM The simTime to do the injection.
     * @param VIDDT The step size.
     * @param NVID The number of vehicles being injected.
     * @param VIDQIT The time the vehicle should enter the simulation.
     * @param VIDVCN The vehicle class number.
     * @param VIDDCN The driver class number.
     * @param VIDDSP The vehicle's desired speed.
     * @param VIDOBN The outbound approach number.
     * @param VIDIBN The inbound approach number.
     * @param VIDILN The inbound lane number.
     * @param VIDPLO Whether stats should be printed on vehicle logout.
     * @param VIDFUT Whether the vehicle should use U-turns.
     * @param VIDFST Forced stop time.
     * @param VIDFSL Forced stop link number.
     * @param VIDFSP Forced stop position.
     * @param VIDFSD Forced stop dwell time.
     * @param VIDFGT Forced go time.
     * @param VIDFGA Forced go active time.
     * @param VIDFRT Forced run red time.
     * @param VIDFRA Forced run red active time.
     * @param VIDIVN The vehicle id number.
     * @param VIDEMV Whether the vehicle is an emergency vehicle.
     * @param VIDSPD The vehicle's initial speed.
     * @param VIDACC The vehicle's initial acceleration.
     * @param VIDSLP The vehicle's initial jerk rate.
     */
    void injectVehicles(double VIDTIM, double VIDDT, int NVID, double[] VIDQIT, int[] VIDVCN, int[] VIDDCN, int[] VIDDSP, int[] VIDOBN, int[] VIDIBN, int[] VIDILN, int[] VIDPLO, int[] VIDFUT,
            double[] VIDFST, int[] VIDFSL, double[] VIDFSP, double[] VIDFSD, double[] VIDFGT, double[] VIDFGA, double[] VIDFRT, double[] VIDFRA, int[] VIDIVN, int[] VIDEMV, double[] VIDSPD,
            double[] VIDACC, double[] VIDSLP);

    /**
     * Gets any errors that occurred during vehicle injection.
     * 
     * @param ERRTIM The time the error occurred.
     * @param ERRIVN The ID of the vehicle affected.
     * @param ERRNUM The error code.
     * @param ERRCNT The number of errors.
     */
    void getInjectionErrors(double[] ERRTIM, int[] ERRIVN, int[] ERRNUM, IntByReference ERRCNT);

    /**
     * An inner class that mirrors the structure defined in the eTEXAS C library. NOTE:
     * Intentionally package-level access to limit use of SIMPRO_ConstantsData class to within
     * package for maintainability reasons.
     */
    static class SIMPRO_ConstantsData extends Structure {

        public int intVals[] = { 0, 0, 0, 0 };

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "intVals" });
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("Constants Data: ");
            sb.append("[NCM = ");
            sb.append(intVals[0]);
            sb.append("], [NRG = ");
            sb.append(intVals[1]);
            sb.append("], [NON = ");
            sb.append(intVals[2]);
            sb.append("], [NIL = ");
            sb.append(intVals[3]);
            sb.append("]");
            return sb.toString();
        }
    }

    /**
     * An inner class that mirrors the structure defined in the eTEXAS C library. NOTE:
     * Intentionally package-level access to limit use of SIMPRO_MapData class to within package for
     * maintainability reasons.
     */
    static class SIMPRO_MapData extends Structure {

        public int intVals[] = new int[2];

        public int IBLN[] = new int[NLA];

        public int ISNA[] = new int[NLA];

        public int LIBAR[] = new int[NAP];

        public int LOBAR[] = new int[NAP];

        public int LCONTR[] = new int[NLA];

        public int LGEOM[] = new int[NLA * 4];

        public int LTURN[] = new int[NLA];

        public int LWID[] = new int[NLA];

        public int ISLIM[] = new int[NLA];

        public int NLANES[] = new int[NAP];

        public double BASELX[] = new double[NLA];

        public double BASELY[] = new double[NLA];

        public double ENDLNX[] = new double[NLA];

        public double ENDLNY[] = new double[NLA];

        public int[] LIBA;

        public int[] LLANES = new int[NAP * NAL];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(
                    new String[] { "intVals", "IBLN", "ISNA", "LIBAR", "LOBAR", "LCONTR", "LGEOM", "LTURN", "LWID", "ISLIM", "NLANES", "BASELX", "BASELY", "ENDLNX", "ENDLNY", "LIBA", "LLANES" });
        }
    }

    /**
     * An inner class that mirrors the structure defined in the eTEXAS C library. NOTE:
     * Intentionally package-level access to limit use of SIMPRO_SPATData class to within package
     * for maintainability reasons.
     */
    static class SIMPRO_SignalData extends Structure {

        public double doubleVals[] = new double[2];

        public double TCAMSP[] = new double[NCM];

        public int intVals[] = new int[8];

        public int ICAMPH[] = new int[NCM];

        public int INTER[] = new int[NRG];

        public int ISISET[] = new int[(NCM + 2 + NON + NON) * NIL];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "doubleVals", "TCAMSP", "intVals", "ICAMPH", "INTER", "ISISET" });
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("Signal Data: ");
            sb.append("ints - [ICONTR = ");
            sb.append(intVals[ICONTR]);
            sb.append("], [NIBL = ");
            sb.append(intVals[NIBL]);
            sb.append("], [IARRPH = ");
            sb.append(intVals[IARRPH]);
            sb.append("], [ICAMPC = ");
            sb.append(intVals[ICAMPC]);
            sb.append("], [ICAMPO = ");
            sb.append(intVals[ICAMPO]);
            sb.append("], [ICPHAS = ");
            sb.append(intVals[ICPHAS]);
            sb.append("], [IGO = ");
            sb.append(intVals[IGO]);
            sb.append("], [NCAMSP = ");
            sb.append(intVals[NCAMSP]);
            sb.append("]");
            sb.append(", doubles - [TP = ");
            sb.append(doubleVals[0]);
            sb.append("], [TR = ");
            sb.append(doubleVals[1]);
            sb.append("]\n");
            sb.append("ICAMPH: ");
            sb.append(ICAMPH[0]);
            for (int i = 1; i < ICAMPH.length; i++) {
                sb.append(", ");
                sb.append(ICAMPH[i]);
            }
            sb.append("\nINTER: ");
            sb.append(INTER[0]);
            for (int i = 1; i < INTER.length; i++) {
                sb.append(", ");
                sb.append(INTER[i]);
            }
            sb.append("\nISISET:\n");
            sb.append(ISISET[0]);
            for (int i = 1; i < ISISET.length / 3; i++) {
                sb.append(", ");
                if (i % NIL == 0) {
                    sb.append("\n");
                }
                sb.append(ISISET[i]);
            }
            return sb.toString();
        }
    }

    /**
     * An inner class that mirrors the structure defined in the eTEXAS C library. NOTE:
     * Intentionally package-level access to limit use of SIMPRO_VehicleData class to within package
     * for maintainability reasons.
     */
    static class SIMPRO_VehicleData extends Structure {

        public double FBX[] = new double[NVE];

        public double FBY[] = new double[NVE];

        /*
         * List of the vehicle number (NUMV), indexed by entry number for vehicle entities. IQ(N)=0,
         * means that the Nth entry of the VEH entities is unused. The number of elements in this
         * array is NVE+2 because the first element is 0 and the last element is NVE+1. Elements at
         * index 1 through NVE represent vehicle IDs.
         */
        public int IQ[] = new int[NVEP1];

        public int INUSE[] = new int[NVE];

        public short CLASS[] = new short[NVE];

        public int LANE[] = new int[NVE];

        public double VEL[] = new double[NVE];

        public double LEN[] = new double[NVE];

        public double PHEAD[] = new double[NVE];

        public double PANGLE[] = new double[NVE];

        public double PWID[] = new double[NVE];

        public double ACC[] = new double[NVE];

        public double DECBRK;

        public int PHEIGHT[] = new int[NVE];

        public int PTYPE[] = new int[NVE];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "FBX", "FBY", "IQ", "INUSE", "CLASS", "LANE", "VEL", "LEN", "PHEAD", "PANGLE", "PWID", "ACC", "DECBRK", "PHEIGHT", "PTYPE" });
        }
    }

    static class SIMPRO_NTCIP1202Data extends Structure {

        // (int[] DETEC, int[] NPEDS, int[] PEDETT, int[] VEHDET, int[] VEHSTA,
        // char[] SOVRTA, int[] OVRTAB, int[] OVRGRP, int[] PHASET, int[] PHASEC, int[] PHASEG);

        public int DETEC[] = new int[DETECS];

        public int NPEDS[] = new int[NPEDSIZ];

        public boolean PEDETT[] = new boolean[PEDDETTS];

        public int VEHDET[] = new int[VEHDETS];

        public int VEHSTA[] = new int[VEHSTAS];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "DETEC", "NPEDS", "PEDETT", "VEHDET", "VEHSTA" });
        }
    }

    static class SIMPRO_NTCIP1202PhaseData extends Structure {

        public int OVRGRP[] = new int[OVRGRPS];

        public int PHASET[] = new int[PHASETS];

        public int PHASEG[] = new int[PHASEGS];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "OVRGRP", "PHASET", "PHASEG" });
        }
    }

    static class SIMPRO_DetectorData extends Structure {

        public int PLOOPS[] = new int[NLS];

        public boolean PCLER[] = new boolean[NLS];

        public boolean PCROS[] = new boolean[NLS];

        public boolean PTRIP[] = new boolean[NLS];

        public int PSTAT[] = new int[NLS];

        public int PNLDLN[] = new int[NLS];

        public int PSTRT[] = new int[NLS];

        public int PSTOP[] = new int[NLS];

        public boolean[] PITYPE = new boolean[NLS];

        public int PLDA[] = new int[NLS];

        public int PLLDLN[] = new int[NAL * NLS];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "PLOOPS", "PCLER", "PCROS", "PTRIP", "PSTAT", "PNLDLN", "PSTRT", "PSTOP", "PITYPE", "PLDA", "PLLDLN" });
        }
    }

    static class SIMPRO_DTData extends Structure {

        public double PDT;

        public double PSIMTI;

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "PDT", "PSIMTI" });
        }
    }

    static class SIMPRO_VehicleInjectionData extends Structure {

        public double VIDTIM;

        public double VIDDT;

        public int NVID;

        public double VIDQIT[] = new double[NIL];

        public int VIDVCN[] = new int[NIL];

        public int VIDDCN[] = new int[NIL];

        public int VIDDSP[] = new int[NIL];

        public int VIDOBN[] = new int[NIL];

        public int VIDIBN[] = new int[NIL];

        public int VIDILN[] = new int[NIL];

        public int VIDPLO[] = new int[NIL];

        public int VIDFUT[] = new int[NIL];

        public double VIDFST[] = new double[NIL];

        public int VIDFSL[] = new int[NIL];

        public double VIDFSP[] = new double[NIL];

        public double VIDFSD[] = new double[NIL];

        public double VIDFGT[] = new double[NIL];

        public double VIDFGA[] = new double[NIL];

        public double VIDFRT[] = new double[NIL];

        public double VIDFRA[] = new double[NIL];

        public int VIDIVN[] = new int[NIL];

        public int VIDEMV[] = new int[NIL];

        public double VIDSPD[] = new double[NIL];

        public double VIDACC[] = new double[NIL];

        public double VIDSLP[] = new double[NIL];

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "VIDTIM", "VIDDT", "NVID", "VIDQIT", "VIDVCN", "VIDDCN", "VIDDSP", "VIDOBN", "VIDIBN", "VIDILN", "VIDPLO", "VIDFUT", "VIDFST", "VIDFSL", "VIDFSP",
                    "VIDFSD", "VIDFGT", "VIDFGA", "VIDFRT", "VIDFRA", "VIDIVN", "VIDEMV", "VIDSPD", "VIDACC", "VIDSLP" });
        }
    }

    static class SIMPRO_VehicleInjectionError extends Structure {

        public double ERRTIM[] = new double[NIL];

        public int ERRIVN[] = new int[NIL];

        public int ERRNUM[] = new int[NIL];

        public int ERRCNT;

        /**
         * Returns an ordered list of field names for this structure.
         * 
         * @return a string list of ordered field names
         */
        @Override
        protected List<String> getFieldOrder() {
            return Arrays.asList(new String[] { "ERRTIM", "ERRIVN", "ERRNUM", "ERRCNT" });
        }
    }
}
