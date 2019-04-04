package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_SpecialVeh.java                             */
/******************************************************************************/
/*                                                                            */
/*     gdvsim COPYRIGHT (C) 2004 by Rioux Engineering, Austin, Texas USA      */
/*                                                                            */
/*   Permission is hereby granted to use, modify, copy, and distribute this   */
/*   software and its documentation for any purpose only without profit,      */
/*   provided that the above Copyright Notice appears in all copies and that  */
/*   both the Copyright Notice and this Permission Notice appears in every    */
/*   copy of supporting documentation.  No title to nor ownership of the      */
/*   software is transferred hereby.  The name of Rioux Engineering shall not */
/*   be used in advertising or publicity related to the distribution of the   */
/*   software without specific, written, prior permission.  This software is  */
/*   provided as-delivered without expressed or implied warranty.  Rioux      */
/*   Engineering makes no representation about the suitability of this        */
/*   software for any purpose and accepts no responsibility for its use.      */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*   This program is free software; you can redistribute it and/or modify     */
/*   it under the terms of the GNU General Public License as published by     */
/*   the Free Software Foundation; either version 2 of the License, or        */
/*   (at your option) any later version.                                      */
/*                                                                            */
/*   This program is distributed in the hope that it will be useful,          */
/*   but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*   GNU General Public License for more details.                             */
/*                                                                            */
/*   You should have received a copy of the GNU General Public License        */
/*   along with this program; if not, write to the Free Software              */
/*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  */
/*                                                                            */
/******************************************************************************/

/*********************/
/* import statements */
/*********************/

import java.io.*;
import java.lang.*;
import java.util.*;

class GDV_SpecialVeh {

    double mdfv_queue_in_time; /*
                                * User-defined special vehicle queue-in time in seconds
                                */

    int msiv_vehicle_class; /* User-defined special vehicle vehicle class number */

    int msiv_driver_class; /* User-defined special vehicle driver class number */

    int msiv_desired_speed; /*
                             * User-defined special vehicle desired speed in ft/sec
                             */

    int msiv_desired_outbound_leg; /*
                                    * User-defined special vehicle desired outbound leg number
                                    */

    int msiv_inbound_leg; /* User-defined special vehicle inbound leg number */

    int msiv_inbound_lane; /* User-defined special vehicle inbound lane number */

    int msiv_vehicle_logout_summary; /*
                                      * User-defined special vehicle vehicle logout summary (0=NO or
                                      * 1=YES)
                                      */

    String mstv_free_uturn_use; /*
                                 * User-defined special vehicle free u-turn use (F=free u-turn,
                                 * O=other)
                                 */

    double mdfv_forced_stop_time; /*
                                   * User-defined special vehicle forced stop time (seconds) (0=not
                                   * used)
                                   */

    String mstv_forced_stop_inb_out_path; /*
                                           * User-defined special vehicle forced stop location
                                           */

    /*
     * (I=leg Inbound, L=inbound to center Left, O=leg Outbound, P=intersection Path, R=inbound to
     * center Right)
     */
    int msiv_forced_stop_leg_or_path; /*
                                       * User-defined special vehicle forced stop leg or path number
                                       */

    double mdfv_forced_stop_position; /*
                                       * User-defined special vehicle forced stop position (feet)
                                       */

    double mdfv_forced_stop_dwell_time; /*
                                         * User-defined special vehicle forced stop dwell time
                                         * (seconds)
                                         */

    double mdfv_forced_go_time; /*
                                 * User-defined special vehicle forced go time (seconds) (0=not
                                 * used)
                                 */

    double mdfv_forced_go_active_time; /*
                                        * User-defined special vehicle forced go active time
                                        * (seconds)
                                        */

    double mdfv_forced_run_red_sig_time; /*
                                          * User-defined special vehicle forced run red signal time
                                          * (seconds) (0=not used)
                                          */

    double mdfv_forced_run_red_sig_acttim; /*
                                            * User-defined special vehicle forced run red signal
                                            * active time (seconds)
                                            */

    String mstv_emergency_vehicle; /*
                                    * User-defined special vehicle emergency vehicle (YES/NO)
                                    */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_SPECIAL_VEH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_queue_in_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_vehicle_class
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_driver_class
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_desired_speed
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_desired_outbound_leg
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_inbound_leg
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_inbound_lane
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_vehicle_logout_summary
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mstv_free_uturn_use
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_stop_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mstv_forced_stop_inb_out_path
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].msiv_forced_stop_leg_or_path
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_stop_position
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_stop_dwell_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_go_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_go_active_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_run_red_sig_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mdfv_forced_run_red_sig_acttim
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_special_veh[lsiv_sv].mstv_emergency_vehicle
    // .mclv_aux.msia_stat[TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH]

    public GDV_SpecialVeh() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_SpecialVeh

    public void checkForErrors(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_SpecialVeh data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_SpecialVeh.checkForErrors psiv_veh=" + psiv_veh);
        // if no special vehicles then return success
        if (Intersection.msiv_specialVehicles == 0) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.msiv_specialVehicles)) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.checkForErrors: psiv_veh = " + psiv_veh + " is < 1 or > " + Intersection.msiv_specialVehicles + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SPECIAL_VEH];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_SpecialVeh.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mdfv_forced_stop_time == 0.0) {
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP)
                    continue;
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP)
                    continue;
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS)
                    continue;
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT)
                    continue;
            }
            if (mdfv_forced_go_time == 0.0) {
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT)
                    continue;
            }
            if (mdfv_forced_run_red_sig_time == 0.0) {
                if (lsiv_field == Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT)
                    continue;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_SpecialVeh.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_max;
        int lsiv_max;
        int lsiv_num_lines;
        String lstv_name;

        // printToFile prints all GDV_SpecialVeh data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_SpecialVeh.printToFile psiv_veh=" + psiv_veh);
        // if no special vehicles then return success
        if (Intersection.msiv_specialVehicles == 0) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.msiv_specialVehicles)) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: psiv_veh = " + psiv_veh + " is < 1 or > " + Intersection.msiv_specialVehicles + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SPECIAL_VEH];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_SpecialVeh.printToFile printing " + lstv_name);

        // calculate the number of lines
        lsiv_num_lines = 10;
        if (mdfv_forced_stop_time > 0.0)
            lsiv_num_lines += 5;
        if (mdfv_forced_go_time > 0.0)
            lsiv_num_lines += 2;
        if (mdfv_forced_run_red_sig_time > 0.0)
            lsiv_num_lines += 2;

        // go to a new page if there is not enough room on the current page
        if (psiv_veh == 1) {
            Intersection.filesPrintGDV_check_newpage(lsiv_num_lines + 2);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            Intersection.filesPrintGDV_check_newpage(lsiv_num_lines);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // set queue-in time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_queue_in_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set queue-in time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set vehicle class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_vehicle_class, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set vehicle class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set driver class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_driver_class, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set driver class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_desired_speed, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set outbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_desired_outbound_leg, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set outbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_inbound_leg, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set inbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound lane max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_inbound_lane, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);

        // re-set inbound lane max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_vehicle_logout_summary, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // change a blank free u-turn use into the default value
        if (mstv_free_uturn_use.equals(" ")) {
            mstv_free_uturn_use = (Intersection.mbov_is_diamond_interchange ? "O" : "N");
        }

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_free_uturn_use, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_stop_time > 0.0) {
            // set forced stop time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_stop_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced stop time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_StringToFile(mstv_forced_stop_inb_out_path, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O") || mstv_forced_stop_inb_out_path.equals("P")) {
                // set forced stop leg or path max to current value
                // special code that must also be performed by the menu system
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
                if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                }
                else if (mstv_forced_stop_inb_out_path.equals("P")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = PARAMS.TEXAS_MODEL_NPA;
                }

                // print data to a file
                Intersection.filesPrintGDV_IntToFile(msiv_forced_stop_leg_or_path, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP, mclv_aux,
                        Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                        Intersection.GDVSIM_ADD_TRAILING_DASHES);

                // re-set forced stop leg or path max to original value
                // special code that must also be performed by the menu system
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = lsiv_max;
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            } // end if ( mstv_forced_stop_inb_out_path.equals( "I" ) ||
              // mstv_forced_stop_inb_out_path.equals( "O" ) ||
              // mstv_forced_stop_inb_out_path.equals( "P" ) )

            // set forced stop position max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS];
            if (mstv_forced_stop_inb_out_path.equals("I")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
            }
            else if (mstv_forced_stop_inb_out_path.equals("L") || mstv_forced_stop_inb_out_path.equals("R")) {
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
            }
            else if (mstv_forced_stop_inb_out_path.equals("O")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
            }
            else if (mstv_forced_stop_inb_out_path.equals("P")) {
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = PARAMS.TEXAS_MODEL_POSMAX;
            }
            else {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: mstv_forced_stop_inb_out_path is not correct.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_stop_position, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced stop position max to current value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set forced stop dwell time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_stop_dwell_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced stop dwell time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // if ( mdfv_forced_stop_time > 0.0 )

        if (mdfv_forced_go_time > 0.0) {
            // set forced go time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_go_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced go time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set forced go active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_go_active_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced go active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_go_time > 0.0 )

        if (mdfv_forced_run_red_sig_time > 0.0) {
            // set forced run red signal time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_run_red_sig_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced run red signal time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set forced run red signal active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // print data to a file
            Intersection.filesPrintGDV_DoubleToFile(mdfv_forced_run_red_sig_acttim, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);

            // re-set forced run red signal active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_run_red_sig_time > 0.0 )

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_emergency_vehicle, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_max;
        int lsiv_card;
        int lsiv_max;
        String lstv_name;

        // readFromCards reads all GDV_SpecialVeh fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_SpecialVeh.readFromCards psiv_veh=" + psiv_veh);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if no special vehicles then return success
        if (Intersection.msiv_specialVehicles == 0) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.msiv_specialVehicles)) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: psiv_veh = " + psiv_veh + " is < 1 or > " + Intersection.msiv_specialVehicles + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if special vehicle card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SPECIAL_VEH_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SPECIAL_VEH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card + psiv_veh - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_SpecialVeh.readFromCards reading " + lstv_name);

        // set queue-in time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // read data from cards
        mdfv_queue_in_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set queue-in time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set vehicle class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;

        // read data from cards
        msiv_vehicle_class = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set vehicle class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set driver class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;

        // read data from cards
        msiv_driver_class = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set driver class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_desired_speed = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set outbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // read data from cards
        msiv_desired_outbound_leg = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set outbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // read data from cards
        msiv_inbound_leg = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set inbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound lane max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;

        // read data from cards
        msiv_inbound_lane = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        // re-set inbound lane max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_vehicle_logout_summary = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_free_uturn_use = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // change a blank free u-turn use into the default value
        if (mstv_free_uturn_use.equals(" ")) {
            mstv_free_uturn_use = (Intersection.mbov_is_diamond_interchange ? "O" : "N");
        }

        // set forced stop time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // read data from cards
        mdfv_forced_stop_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

        // re-set forced stop time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_stop_time > 0.0) {
            // read data from cards
            mstv_forced_stop_inb_out_path = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY,
                    Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O") || mstv_forced_stop_inb_out_path.equals("P")) {
                // set forced stop leg or path max to current value
                // special code that must also be performed by the menu system
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
                if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                }
                else if (mstv_forced_stop_inb_out_path.equals("P")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = PARAMS.TEXAS_MODEL_NPA;
                }

                // read data from cards
                msiv_forced_stop_leg_or_path = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP,
                        Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

                // re-set forced stop leg or path max to original value
                // special code that must also be performed by the menu system
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = lsiv_max;
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            } // end if ( mstv_forced_stop_inb_out_path.equals( "I" ) ||
              // mstv_forced_stop_inb_out_path.equals( "O" ) ||
              // mstv_forced_stop_inb_out_path.equals( "P" ) )

            // set forced stop position max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS];
            if (mstv_forced_stop_inb_out_path.equals("I")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
            }
            else if (mstv_forced_stop_inb_out_path.equals("L") || mstv_forced_stop_inb_out_path.equals("R")) {
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
            }
            else if (mstv_forced_stop_inb_out_path.equals("O")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
            }
            else if (mstv_forced_stop_inb_out_path.equals("P")) {
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = PARAMS.TEXAS_MODEL_POSMAX;
            }
            else {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: mstv_forced_stop_inb_out_path is not correct.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // read data from cards
            mdfv_forced_stop_position = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            // re-set forced stop position max to current value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set forced stop dwell time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // read data from cards
            mdfv_forced_stop_dwell_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

            // re-set forced stop dwell time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_stop_time > 0.0 )

        // set forced go time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // read data from cards
        mdfv_forced_go_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

        // re-set forced go time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_go_time > 0.0) {
            // set forced go active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // read data from cards
            mdfv_forced_go_active_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

            // re-set forced go active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_go_time > 0.0 )

        // set forced run red signal time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // read data from cards
        mdfv_forced_run_red_sig_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

        // re-set forced run red signal time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_run_red_sig_time > 0.0) {
            // set forced run red signal active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // read data from cards
            mdfv_forced_run_red_sig_acttim = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);

            // re-set forced run red signal active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_run_red_sig_time > 0.0 )

        // read data from cards
        mstv_emergency_vehicle = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_SpecialVeh fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_SpecialVeh.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_veh) {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_max;
        int lsiv_card;
        int lsiv_max;
        String lstv_name;

        // writeToCards writes all GDV_SpecialVeh fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_SpecialVeh.writeToCards psiv_veh=" + psiv_veh);
        // if no special vehicles then return success
        if (Intersection.msiv_specialVehicles == 0) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check parameters
        if ((psiv_veh < 1) || (psiv_veh > Intersection.msiv_specialVehicles)) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: psiv_veh = " + psiv_veh + " is < 1 or > " + Intersection.msiv_specialVehicles + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if special vehicle card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SPECIAL_VEH_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SPECIAL_VEH];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card + psiv_veh - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_veh));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_SpecialVeh.writeToCards writing " + lstv_name);

        // set queue-in time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_queue_in_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set queue-in time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_QUEUE_IN_TIME] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set vehicle class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl;

        // write data to cards
        Intersection.writeIntToCard(msiv_vehicle_class, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set vehicle class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_VEHICLE_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set driver class max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl;

        // write data to cards
        Intersection.writeIntToCard(msiv_driver_class, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set driver class max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DRIVER_CLASS] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_desired_speed, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_SPEED, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set outbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // write data to cards
        Intersection.writeIntToCard(msiv_desired_outbound_leg, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set outbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_DESIRED_O_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound leg max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;

        // write data to cards
        Intersection.writeIntToCard(msiv_inbound_leg, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set inbound leg max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LEG] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set inbound lane max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE];
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = Intersection.mcla_leg[msiv_inbound_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;

        // write data to cards
        Intersection.writeIntToCard(msiv_inbound_lane, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        // re-set inbound lane max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_INBOUND_LANE] = lsiv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_vehicle_logout_summary, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_LOGOUT_SUMMRY, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // change a blank free u-turn use into the default value
        if (mstv_free_uturn_use.equals(" ")) {
            mstv_free_uturn_use = (Intersection.mbov_is_diamond_interchange ? "O" : "N");
        }

        // write data to cards
        Intersection.writeStringToCard(mstv_free_uturn_use, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FREE_UTURN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set forced stop time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_forced_stop_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

        // re-set forced stop time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPTIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_stop_time > 0.0) {
            // write data to cards
            Intersection.writeStringToCard(mstv_forced_stop_inb_out_path, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPIOP,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O") || mstv_forced_stop_inb_out_path.equals("P")) {
                // set forced stop leg or path max to current value
                // special code that must also be performed by the menu system
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lsiv_max = lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP];
                if (mstv_forced_stop_inb_out_path.equals("I") || mstv_forced_stop_inb_out_path.equals("O")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
                }
                else if (mstv_forced_stop_inb_out_path.equals("P")) {
                    lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = PARAMS.TEXAS_MODEL_NPA;
                }

                // write data to cards
                Intersection.writeIntToCard(msiv_forced_stop_leg_or_path, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP,
                        Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

                // re-set forced stop leg or path max to original value
                // special code that must also be performed by the menu system
                lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPLIP] = lsiv_max;
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;
            } // end if ( mstv_forced_stop_inb_out_path.equals( "I" ) ||
              // mstv_forced_stop_inb_out_path.equals( "O" ) ||
              // mstv_forced_stop_inb_out_path.equals( "P" ) )

            // set forced stop position max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS];
            if (mstv_forced_stop_inb_out_path.equals("I")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_inb;
            }
            else if (mstv_forced_stop_inb_out_path.equals("L") || mstv_forced_stop_inb_out_path.equals("R")) {
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_DIST_BETWEEN] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_dist_between;
            }
            else if (mstv_forced_stop_inb_out_path.equals("O")) {
                if (Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_LEN_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mcla_leg[" + msiv_forced_stop_leg_or_path
                            + "].mclv_TX_Leg_Data.mclv_geo.msiv_len_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = Intersection.mcla_leg[msiv_forced_stop_leg_or_path].mclv_TX_Leg_Data.mclv_geo.msiv_len_out;
            }
            else if (mstv_forced_stop_inb_out_path.equals("P")) {
                lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = PARAMS.TEXAS_MODEL_POSMAX;
            }
            else {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: mstv_forced_stop_inb_out_path is not correct.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_forced_stop_position, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            // re-set forced stop position max to current value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPPOS] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // set forced stop dwell time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_forced_stop_dwell_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

            // re-set forced stop dwell time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCEDSTOPDWT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_stop_time > 0.0 )

        // set forced go time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_forced_go_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

        // re-set forced go time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_TIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_go_time > 0.0) {
            // set forced go active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_forced_go_active_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

            // re-set forced go active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_GO_ACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_go_time > 0.0 )

        // set forced run red signal time max to current value
        // special code that must also be performed by the menu system
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM];
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_forced_run_red_sig_time, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

        // re-set forced run red signal time max to original value
        // special code that must also be performed by the menu system
        lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSTIM] = ldfv_max;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (mdfv_forced_run_red_sig_time > 0.0) {
            // set forced run red signal active time max to current value
            // special code that must also be performed by the menu system
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_TIME] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_SpecialVeh.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            ldfv_max = lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT];
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = 60.0 * Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time;

            // write data to cards
            Intersection.writeDoubleToCard(mdfv_forced_run_red_sig_acttim, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT,
                    Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);

            // re-set forced run red signal active time max to original value
            // special code that must also be performed by the menu system
            lclv_tx_fmt.mdfa_max[Intersection.TX_FMT_GDV_SPECIAL_VEH_FORCED_RRSACT] = ldfv_max;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( mdfv_forced_run_red_sig_time > 0.0 )

        // write data to cards
        Intersection.writeStringToCard(mstv_emergency_vehicle, lstv_name, Intersection.TX_FMT_GDV_SPECIAL_VEH, Intersection.TX_FMT_GDV_SPECIAL_VEH_EMERGENCY_VEH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_SpecialVeh

/******************************************************************************/
/* GDV_SpecialVeh.java */
/******************************************************************************/
