package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              SIM_Par_Opt.java                              */
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

class SIM_Par_Opt {

    double mdfv_start_time; /* startup time in minutes */

    double mdfv_sim_time; /* simulation time in minutes */

    double mdfv_dt; /* simulation time increment in seconds */

    String mstv_inter_control; /* type of intersection control */

    String mstv_turn_sum; /* print statistical summary by turning movements ? */

    String mstv_appro_sum; /* print statistical summary by inbound approaches ? */

    String mstv_compressed; /*
                             * write file of compressed (spreadsheet) statistics ?
                             */

    String mstv_pva_data; /*
                           * write file of vehicle position data for animation or pollution ?
                           */

    int msiv_pva_end; /* ending time in minutes for vehicle position data */

    String mstv_wide; /* printed output uses 132 columns ? (no uses 80 columns) */

    String mstv_lt_pullout; /* left turning vehicles pull into intersection? */

    String mstv_nem2_simul_gapout; /* Enable NEMA Simultaneous Gap Out */

    int msiv_num_vms_messages; /* number of VMS messages */

    double mdfv_dilemma_zone_time_beg; /*
                                        * dilemma zone beginning time in seconds
                                        */

    double mdfv_dilemma_zone_time_end; /* dilemma zone ending time in seconds */

    int msiv_hitl_sleep_time; /* hardware-in-the-loop sleep time in seconds */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_PAR_OPT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_start_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_START_TIME ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_sim_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_SIM_TIME ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dt
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_DT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_INTER_CONTROL ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_turn_sum
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_TURN_SUM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_appro_sum
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_APPRO_SUM ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_compressed
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_COMPRESSED ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_pva_data
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_PVA_DATA ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_pva_end
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_PVA_END ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_wide
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_WIDE ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_lt_pullout
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_LT_PULLOUT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_nem2_simul_gapout
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_num_vms_messages
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_beg
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mdfv_dilemma_zone_time_end
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.msiv_hitl_sleep_time
    // .mclv_aux.msia_stat[TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME ]

    public SIM_Par_Opt() {
        mclv_aux = new TX_Aux();
    } // end of method SIM_Par_Opt

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all SIM_Par_Opt data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Par_Opt.checkForErrors");
        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Par_Opt.checkForErrors checking " + lstv_name);

        // set defaults -------------------- usage ---------------------
        // mstv_nem2_simul_gapout NEMA
        // mdfv_dilemma_zone_time_beg PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // mdfv_dilemma_zone_time_end PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // msiv_hitl_sleep_time HARDWARE
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_PRETIMED
                    || Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT || Intersection.mbov_is_ic_TEX_DIA || Intersection.mbov_is_ic_HARDWARE) {
                mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                mdfv_dilemma_zone_time_beg = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                mdfv_dilemma_zone_time_end = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_PRETIMED
                    || Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT || Intersection.mbov_is_ic_TEX_DIA || Intersection.mbov_is_ic_NEMA) {
                msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = Intersection.TX_DEFAULT;
            }
        }

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in SIM_Par_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        String lstv_name;

        // printToFile prints all SIM_Par_Opt data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("SIM_Par_Opt.printToFile");
        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("SIM_Par_Opt.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        lsiv_lines = 12;
        if (Intersection.mbov_is_ic_PRETIMED || Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT || Intersection.mbov_is_ic_TEX_DIA) {
            // mdfv_dilemma_zone_time_beg and mdfv_dilemma_zone_time_end
            lsiv_lines += 2;
        }
        if (Intersection.mbov_is_ic_NEMA || Intersection.mbov_is_ic_HARDWARE) {
            // NEMA mstv_nem2_simul_gapout, mdfv_dilemma_zone_time_beg, and
            // mdfv_dilemma_zone_time_end
            // HARDWARE mdfv_dilemma_zone_time_beg, mdfv_dilemma_zone_time_end, and
            // msiv_hitl_sleep_time
            lsiv_lines += 3;
        }
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_start_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_START_TIME, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_sim_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_SIM_TIME, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_DoubleToFile(mdfv_dt, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_inter_control, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_INTER_CONTROL, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_turn_sum, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_TURN_SUM, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_appro_sum, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_APPRO_SUM, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_compressed, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_COMPRESSED, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_pva_data, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_DATA, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_pva_end, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_END, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_wide, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_WIDE, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_lt_pullout, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_LT_PULLOUT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_ic_NEMA) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_nem2_simul_gapout, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintSIM_IntToFile(msiv_num_vms_messages, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;
        if (Intersection.mbov_is_ic_signal_controlled) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_dilemma_zone_time_beg, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_dilemma_zone_time_end, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if (Intersection.mbov_is_ic_HARDWARE) {
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msiv_hitl_sleep_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        double ldfv_pva_end;
        int lsiv_card;
        int lsiv_nf;
        String lstv_name;
        String lstv_pva_end;

        // readFromCards reads all SIM_Par_Opt fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Par_Opt.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // SIM_Par_Opt card is always after the SIM_Title card
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card + 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Par_Opt.readFromCards reading " + lstv_name);

        // read data from cards
        mdfv_start_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_START_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_sim_time = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_SIM_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_dt = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DT, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_inter_control = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_INTER_CONTROL, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check and set intersection control Intersection.mbov_is_ic_* values using
        // mstv_inter_control
        Intersection.check_and_set_intersection_control();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_turn_sum = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_TURN_SUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_appro_sum = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_APPRO_SUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_compressed = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_COMPRESSED, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_pva_data = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_DATA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        lsiv_nf = lclv_tx_fmt.msiv_nf;
        lclv_tx_fmt.msiv_nf = Intersection.TX_FMT_SIM_PAR_OPT_PVA_END_STRING;
        lstv_pva_end = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_END_STRING, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        lclv_tx_fmt.msiv_nf = lsiv_nf;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (lstv_pva_end.indexOf('.') >= 0) {
            lsiv_nf = lclv_tx_fmt.msiv_nf;
            lclv_tx_fmt.msiv_nf = Intersection.TX_FMT_SIM_PAR_OPT_PVA_END_DOUBLE;
            ldfv_pva_end = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_END_DOUBLE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            lclv_tx_fmt.msiv_nf = lsiv_nf;
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            msiv_pva_end = Intersection.nint(ldfv_pva_end);
        }
        else {
            msiv_pva_end = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_END, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // read data from cards
        mstv_wide = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_WIDE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_lt_pullout = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_LT_PULLOUT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_nem2_simul_gapout = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_num_vms_messages = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_dilemma_zone_time_beg = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_dilemma_zone_time_end = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_hitl_sleep_time = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_ic_HARDWARE) {
            if (mdfv_dt < (PARAMS.TEXAS_MODEL_HDTMIN - 0.0001)) {
                Intersection.mstv_warningMessage = lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + mdfv_dt + " is less than the minimum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMIN + ".";
                Intersection.warningMessage();
            }
            if (mdfv_dt > (PARAMS.TEXAS_MODEL_HDTMAX + 0.0001)) {
                Intersection.mstv_warningMessage = lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + mdfv_dt + " is greater than the maximum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMAX + ".";
                Intersection.warningMessage();
            }
        }

        if (mdfv_dilemma_zone_time_beg <= mdfv_dilemma_zone_time_end) {
            Intersection.mstv_warningMessage = "Dilemma Zone Begin Time = " + mdfv_dilemma_zone_time_beg + " is less than or equal to Dilemma Zone End Time = " + mdfv_dilemma_zone_time_end + ".";
            Intersection.warningMessage();
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all SIM_Par_Opt fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("SIM_Par_Opt.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all SIM_Par_Opt fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Par_Opt.writeToCards");
        // check if sim par opt card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PAR_OPT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Par_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PAR_OPT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_par_opt_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Par_Opt.writeToCards writing " + lstv_name);

        if (Intersection.mbov_is_ic_HARDWARE) {
            if (mdfv_dt < (PARAMS.TEXAS_MODEL_HDTMIN - 0.0001)) {
                Intersection.mstv_errorMessage = lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + mdfv_dt + " is less than the minimum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMIN + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            if (mdfv_dt > (PARAMS.TEXAS_MODEL_HDTMAX + 0.0001)) {
                Intersection.mstv_errorMessage = lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_PAR_OPT_DT] + " = " + mdfv_dt + " is greater than the maximum for HARDWARE Intersection Control = "
                        + PARAMS.TEXAS_MODEL_HDTMAX + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        // set defaults -------------------- usage ---------------------
        // mstv_nem2_simul_gapout NEMA
        // mdfv_dilemma_zone_time_beg PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // mdfv_dilemma_zone_time_end PRETIMED SEMI-ACT FULL-ACT TEX-DIA NEMA HARDWARE
        // msiv_hitl_sleep_time HARDWARE
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_PRETIMED
                    || Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT || Intersection.mbov_is_ic_TEX_DIA || Intersection.mbov_is_ic_HARDWARE) {
                mstv_nem2_simul_gapout = lclv_tx_fmt.msta_def[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                mdfv_dilemma_zone_time_beg = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP) {
                mdfv_dilemma_zone_time_end = lclv_tx_fmt.mdfa_def[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT] = Intersection.TX_DEFAULT;
            }
        }
        if ((mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] == Intersection.TX_DATA_ERROR)
                || (mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] == Intersection.TX_DATA_IS_INVALID)) {
            if (Intersection.mbov_is_ic_UNCONTRL || Intersection.mbov_is_ic_YIELD || Intersection.mbov_is_ic_STOP || Intersection.mbov_is_ic_ALL_STOP || Intersection.mbov_is_ic_PRETIMED
                    || Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT || Intersection.mbov_is_ic_TEX_DIA || Intersection.mbov_is_ic_NEMA) {
                msiv_hitl_sleep_time = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME];
                mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME] = Intersection.TX_DEFAULT;
            }
        }

        if (mdfv_dilemma_zone_time_beg <= mdfv_dilemma_zone_time_end) {
            Intersection.mstv_errorMessage = "Dilemma Zone Begin Time = " + mdfv_dilemma_zone_time_beg + " is less than or equal to Dilemma Zone End Time = " + mdfv_dilemma_zone_time_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_start_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_START_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_sim_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_SIM_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_dt, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DT, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten,
                lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_inter_control, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_INTER_CONTROL, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_turn_sum, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_TURN_SUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_appro_sum, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_APPRO_SUM, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_compressed, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_COMPRESSED, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_pva_data, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_DATA, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_pva_end, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_PVA_END, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_wide, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_WIDE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_lt_pullout, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_LT_PULLOUT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_nem2_simul_gapout, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NEM2_SIMUL_GAPOUT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_num_vms_messages, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_NUM_VMS_MESSAGES, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_dilemma_zone_time_beg, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_BEGT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_dilemma_zone_time_end, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_DILEMMA_ZONE_ENDT, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_hitl_sleep_time, lstv_name, Intersection.TX_FMT_SIM_PAR_OPT, Intersection.TX_FMT_SIM_PAR_OPT_HITL_SLEEP_TIME, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class SIM_Par_Opt

/******************************************************************************/
/* SIM_Par_Opt.java */
/******************************************************************************/
