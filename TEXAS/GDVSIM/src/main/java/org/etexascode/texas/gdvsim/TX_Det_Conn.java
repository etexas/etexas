package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              TX_Det_Conn.java                              */
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

class TX_Det_Conn {

    String mstv_conn_type; /* connection type (AND or OR) */

    int msia_detector[]; /* list of detectors */

    String msta_detector_call_extend[]; /*
                                         * list of detectors call, extend, or call & extend
                                         */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_DETECT_CONNM1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONNM1_DET_10 ]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_DETECT_CONN2H]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_DET_10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_02]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_03]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_04]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_05]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_06]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_07]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_08]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_09]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msta_detector_call_extend[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONN2H_CALEXT_10]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_DETECT_CONSFA]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].mstv_conn_type
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_det_conn[lsiv_phase].msia_detector[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_DETECT_CONSFA_DET_10 ]

    public TX_Det_Conn() {
        msia_detector = new int[PARAMS.TEXAS_MODEL_NPL + 1];
        msta_detector_call_extend = new String[PARAMS.TEXAS_MODEL_NPL + 1];
        mclv_aux = new TX_Aux();

        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPL + 1); lsiv_i++) {
            msta_detector_call_extend[lsiv_i] = new String();
        }
    } // end of method TX_Det_Conn

    public void checkForErrors(int psiv_phase // phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_field;
        int lsiv_i;
        int lsiv_member;
        int lsiv_num_fields;
        int lsiv_no_of_detectors;
        int lsiv_type;
        String lstv_name;

        // checkForErrors checks all TX_Det_Conn data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Det_Conn.checkForErrors psiv_phase=" + psiv_phase);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_conn) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"SEMI-ACT\", \"FULL-ACT\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONSFA;
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1;
            lsiv_num_fields = 1;
            lsiv_type = 0;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONN2H;
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1;
            lsiv_num_fields = 2;
            lsiv_type = Intersection.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Det_Conn.checkForErrors checking " + lstv_name);

        // check all data for errors
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lsiv_field = Intersection.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE;
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in TX_Det_Conn.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        // find the last non-default phase defined
        lsiv_no_of_detectors = 1;
        for (lsiv_detector = PARAMS.TEXAS_MODEL_NPL; lsiv_detector >= 1; lsiv_detector--) {
            if (mclv_aux.msia_stat[lsiv_member + lsiv_detector] == Intersection.TX_DEFAULT) {
                continue;
            }
            if (msia_detector[lsiv_detector] == 0) {
                continue;
            }
            lsiv_no_of_detectors = lsiv_detector;
            break;
        }

        for (lsiv_detector = 1; lsiv_detector <= lsiv_no_of_detectors; lsiv_detector++) {
            for (lsiv_i = 1; lsiv_i <= lsiv_num_fields; lsiv_i++) {
                if (lsiv_i == 1) {
                    lsiv_field = lsiv_member + lsiv_detector;
                }
                else {
                    lsiv_field = lsiv_type + lsiv_detector;
                }
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in TX_Det_Conn.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Det_Conn.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_phase // phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_i;
        int lsiv_line;
        int lsiv_member;
        int lsiv_num_fields;
        int lsiv_no_of_detectors;
        int lsiv_type;
        String lstv_name;

        // printToFile prints all TX_Det_Conn data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("TX_Det_Conn.printToFile psiv_phase=" + psiv_phase);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_conn) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"SEMI-ACT\", \"FULL-ACT\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.printToFile: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1;
        }
        else {
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1;
        }
        // find the last non-default phase defined
        lsiv_no_of_detectors = 1;
        for (lsiv_detector = PARAMS.TEXAS_MODEL_NPL; lsiv_detector >= 1; lsiv_detector--) {
            if (mclv_aux.msia_stat[lsiv_member + lsiv_detector] == Intersection.TX_DEFAULT) {
                continue;
            }
            if (msia_detector[lsiv_detector] == 0) {
                continue;
            }
            lsiv_no_of_detectors = lsiv_detector;
            break;
        }

        // set local data for printing data to a file
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONSFA;
            lsiv_line = 1;
            lsiv_num_fields = 1;
            lsiv_type = 0;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONN2H;
            lsiv_line = lsiv_no_of_detectors;
            lsiv_num_fields = 2;
            lsiv_type = Intersection.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("TX_Det_Conn.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(lsiv_line + lsiv_no_of_detectors);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_conn_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_CONSFA, Intersection.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        for (lsiv_detector = 1; lsiv_detector <= lsiv_no_of_detectors; lsiv_detector++) {
            for (lsiv_i = 1; lsiv_i <= lsiv_num_fields; lsiv_i++) {
                if (lsiv_i == 1) {
                    // print data to a file
                    Intersection.filesPrintSIM_IntToFile(msia_detector[lsiv_detector], lstv_name, lsiv_class, lsiv_member + lsiv_detector, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                            Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else {
                    // print data to a file
                    Intersection.filesPrintSIM_StringToFile(msta_detector_call_extend[lsiv_detector], lstv_name, lsiv_class, lsiv_type + lsiv_detector, mclv_aux,
                            Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                            Intersection.GDVSIM_ADD_TRAILING_DASHES);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_phase // phase number
    ) {
        boolean lbov_force_default;
        boolean lbov_nema_1;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_i;
        int lsiv_member;
        int lsiv_num_fields;
        int lsiv_type;
        String lstv_name;

        // readFromCards reads all TX_Det_Conn fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("TX_Det_Conn.readFromCards psiv_phase=" + psiv_phase);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_conn) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"SEMI-ACT\", \"FULL-ACT\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first detector connection card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_con_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_con_card - 1 + psiv_phase;
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lbov_force_default = false;
            lbov_nema_1 = false;
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONSFA;
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1;
            lsiv_num_fields = 1;
            lsiv_type = 0;
        }
        else {
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver >= 6.00) {
                lbov_force_default = true;
                lbov_nema_1 = false;
                lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONN2H;
                lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1;
                lsiv_num_fields = 2;
                lsiv_type = Intersection.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1;
            }
            else {
                lbov_force_default = true;
                lbov_nema_1 = true;
                lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONNM1;
                lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONNM1_DET_01 - 1;
                lsiv_num_fields = 1;
                lsiv_type = Intersection.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1;
            }
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("TX_Det_Conn.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_conn_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_DETECT_CONSFA, Intersection.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_detector = 1; lsiv_detector <= PARAMS.TEXAS_MODEL_NPL; lsiv_detector++) {
            for (lsiv_i = 1; lsiv_i <= lsiv_num_fields; lsiv_i++) {
                if (lsiv_i == 1) {
                    // read data from cards
                    msia_detector[lsiv_detector] = Intersection.readIntFromCard(lstv_name, lsiv_class, lsiv_member + lsiv_detector, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead,
                            lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else {
                    // read data from cards
                    msta_detector_call_extend[lsiv_detector] = Intersection.readStringFromCard(lstv_name, lsiv_class, lsiv_type + lsiv_detector, Intersection.mcla_simdataCards,
                            Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }

        if (lbov_nema_1) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_DETECT_CONN2H];
            for (lsiv_detector = 1; lsiv_detector <= PARAMS.TEXAS_MODEL_NPL; lsiv_detector++) {
                msta_detector_call_extend[lsiv_detector] = lclv_tx_fmt.msta_def[lsiv_type + lsiv_detector];
                mclv_aux.msia_stat[lsiv_type + lsiv_detector] = Intersection.TX_DATA_IS_INVALID;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Det_Conn fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Det_Conn.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_phase // phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_class;
        int lsiv_detector;
        int lsiv_i;
        int lsiv_member;
        int lsiv_no_of_detectors;
        int lsiv_num_fields;
        int lsiv_type;
        String lstv_name;

        // writeToCards writes all TX_Det_Conn fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("TX_Det_Conn.writeToCards psiv_phase=" + psiv_phase);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is SEMI-ACT, FULL-ACT, NEMA, or HARDWARE
        if (!Intersection.mbov_is_ic_detector_conn) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"SEMI-ACT\", \"FULL-ACT\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.writeToCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first detector connection card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_FIRST_CON_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in TX_Det_Conn.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_con_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_con_card - 1 + psiv_phase;
        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONSFA;
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONSFA_DET_01 - 1;
            lsiv_num_fields = 1;
            lsiv_type = 0;
        }
        else {
            lsiv_class = Intersection.TX_FMT_SIM_DETECT_CONN2H;
            lsiv_member = Intersection.TX_FMT_SIM_DETECT_CONN2H_DET_01 - 1;
            lsiv_num_fields = 2;
            lsiv_type = Intersection.TX_FMT_SIM_DETECT_CONN2H_CALEXT_01 - 1;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[lsiv_class];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("TX_Det_Conn.writeToCards writing " + lstv_name);

        if (Intersection.mbov_is_ic_SEMI_ACT || Intersection.mbov_is_ic_FULL_ACT) {
            // write data to cards
            Intersection.writeStringToCard(mstv_conn_type, lstv_name, Intersection.TX_FMT_SIM_DETECT_CONSFA, Intersection.TX_FMT_SIM_DETECT_CONSFA_CONN_TYPE, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // find the last non-default phase defined
        lsiv_no_of_detectors = 1;
        for (lsiv_detector = PARAMS.TEXAS_MODEL_NPL; lsiv_detector >= 1; lsiv_detector--) {
            if (mclv_aux.msia_stat[lsiv_member + lsiv_detector] == Intersection.TX_DEFAULT) {
                continue;
            }
            if (msia_detector[lsiv_detector] == 0) {
                continue;
            }
            lsiv_no_of_detectors = lsiv_detector;
            break;
        }

        for (lsiv_detector = 1; lsiv_detector <= lsiv_no_of_detectors; lsiv_detector++) {
            for (lsiv_i = 1; lsiv_i <= lsiv_num_fields; lsiv_i++) {
                if (lsiv_i == 1) {
                    // write data to cards
                    Intersection.writeIntToCard(msia_detector[lsiv_detector], lstv_name, lsiv_class, lsiv_member + lsiv_detector, Intersection.mcla_simdataCards,
                            Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
                else {
                    // write data to cards
                    Intersection.writeStringToCard(msta_detector_call_extend[lsiv_detector], lstv_name, lsiv_class, lsiv_type + lsiv_detector, Intersection.mcla_simdataCards,
                            Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                    if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                        return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class TX_Det_Conn

/******************************************************************************/
/* TX_Det_Conn.java */
/******************************************************************************/
