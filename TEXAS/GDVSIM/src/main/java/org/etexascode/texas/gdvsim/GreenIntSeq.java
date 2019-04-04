package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              GreenIntSeq.java                              */
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

class GreenIntSeq {

    String msta_green_int_seq[]; /*
                                  * list of green interval sequence items for a phase or overlap
                                  */

    /* C = Circular Green */
    /* L = Left Protected Green Arrow */
    /* S = Straight Protected Green Arrow */
    /* R = Right Protected Green Arrow */
    /* UN = Unsignalized */
    /* = Red */
    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GREEN_INT_SEQ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_02]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_03]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_04]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_05]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_06]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_07]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_08]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_09]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_10]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_11]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_12]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_13]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_14]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_15]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_PHASE_16]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[17]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_A ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[18]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_B ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[19]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_C ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[20]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_D ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[21]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_E ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[22]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_F ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[23]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_G ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[24]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_H ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[25]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_I ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[26]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_J ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[27]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_K ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[28]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_L ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[29]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_M ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[30]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_N ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[31]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_O ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_green_int_seq.msta_green_int_seq[32]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GREEN_INT_SEQ_OVRLP_P ]

    public GreenIntSeq() {
        msta_green_int_seq = new String[PARAMS.TEXAS_MODEL_NGI + 1];
        mclv_aux = new TX_Aux();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NGI + 1); lsiv_i++) {
            msta_green_int_seq[lsiv_i] = new String();
        }
    } // end of method GreenIntSeq

    public void checkForErrors(int psiv_ph_ovlp, // phase/overlap number (1-16 is phase 1-16, 17-32
                                                 // is overlap A-P)
            int psiv_leg, // leg number
            int psiv_lane // inbound lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        String lstv_name;

        // checkForErrors checks all GreenIntSeq data for invalid data and data with errors
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GreenIntSeq.checkForErrors psiv_ph_ovlp=" + psiv_ph_ovlp + " psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane);
        // check parameters
        if ((psiv_ph_ovlp < 1) || (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NGI)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: psiv_ph_ovlp = " + psiv_ph_ovlp + " is < 1 or > " + PARAMS.TEXAS_MODEL_NGI + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending lane number
        lsiv_lane_beg = 1;
        lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
            // read leg 3 green interval sequence for free u-turn lane and store in inbound lane 0
            // read leg 6 green interval sequence for free u-turn lane and store in inbound lane 0
            lsiv_lane_beg = 0;
        }

        // check parameters
        if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is signal controlled
        if (!Intersection.mbov_is_ic_signal_controlled) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GREEN_INT_SEQ];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GreenIntSeq.checkForErrors checking " + lstv_name);

        // check all data for errors
        lsiv_field = Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + psiv_ph_ovlp;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in GreenIntSeq.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_ph_ovlp, // phase/overlap number (1-16 is phase 1-16, 17-32 is
                                              // overlap A-P)
            int psiv_leg, // leg number
            int psiv_lane // inbound lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_desc;
        String lstv_name;

        // printToFile prints all GreenIntSeq data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("GreenIntSeq.printToFile psiv_ph_ovlp=" + psiv_ph_ovlp + " psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            // special code that must also be performed by the menu system
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending lane number
        lsiv_lane_beg = 1;
        lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
            // write leg 3 green interval sequence for free u-turn lane from inbound lane 0
            // special code that must also be performed by the menu system
            // write leg 6 green interval sequence for free u-turn lane from inbound lane 0
            // special code that must also be performed by the menu system
            lsiv_lane_beg = 0;
        }

        // check parameters
        if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GREEN_INT_SEQ];

        // check if intersection control is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_PAR_OPT_INTER_CONTROL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is signal controlled
        if (!Intersection.mbov_is_ic_signal_controlled) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lsiv_member = Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + psiv_ph_ovlp;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("GreenIntSeq.printToFile printing " + lstv_name);

        lstv_desc = lclv_tx_fmt.msta_desc[lsiv_member].toString();
        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            // special code that must also be performed by the menu system
            if (psiv_leg == lsiv_leg_beg) {
                lclv_tx_fmt.msta_desc[lsiv_member] = lclv_tx_fmt.msta_desc[lsiv_member].concat(" Leg IR Inbound Lane " + psiv_lane);
            }
            else if (psiv_leg == lsiv_leg_end) {
                lclv_tx_fmt.msta_desc[lsiv_member] = lclv_tx_fmt.msta_desc[lsiv_member].concat(" Leg IL Inbound Lane " + psiv_lane);
            }
            else {
                lclv_tx_fmt.msta_desc[lsiv_member] = lclv_tx_fmt.msta_desc[lsiv_member].concat(" Leg " + psiv_leg + "  Inbound Lane " + psiv_lane);
            }
        }
        else {
            lclv_tx_fmt.msta_desc[lsiv_member] = lclv_tx_fmt.msta_desc[lsiv_member].concat(" Leg " + psiv_leg + " Inbound Lane " + psiv_lane);
        }

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(msta_green_int_seq[psiv_ph_ovlp], lstv_name, Intersection.TX_FMT_SIM_GREEN_INT_SEQ, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        lclv_tx_fmt.msta_desc[lsiv_member] = lstv_desc.toString();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_ph_ovlp, // phase/overlap number (1-16 is phase 1-16, 17-32
                                                // is overlap A-P)
            int psiv_leg, // leg number
            int psiv_lane, // inbound lane number
            int psiv_col // column number on card
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_last_col;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_name;

        // readFromCards reads all GreenIntSeq fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GreenIntSeq.readFromCards psiv_ph_ovlp=" + psiv_ph_ovlp + " psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane
                    + " psiv_col=" + psiv_col);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_ph_ovlp < 1) || (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NGI)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: psiv_ph_ovlp = " + psiv_ph_ovlp + " is < 1 or > " + PARAMS.TEXAS_MODEL_NGI + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            // special code that must also be performed by the menu system
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending lane number
        lsiv_lane_beg = 1;
        lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
            // read leg 3 green interval sequence for free u-turn lane and store in inbound lane 0
            // special code that must also be performed by the menu system
            // read leg 6 green interval sequence for free u-turn lane and store in inbound lane 0
            // special code that must also be performed by the menu system
            lsiv_lane_beg = 0;
        }

        // check parameters
        if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GREEN_INT_SEQ];

        // check parameters
        lsiv_last_col = Intersection.TX_FMT_NC - lclv_tx_fmt.msia_fs[Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01] + 1;
        if ((psiv_col < 1) || (psiv_col > lsiv_last_col)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: psiv_col = " + psiv_col + " is < 1 or > " + lsiv_last_col + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is signal controlled
        if (!Intersection.mbov_is_ic_signal_controlled) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if signal sequence card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card - 1 + psiv_ph_ovlp;
        if (Intersection.mbov_is_ic_TEX_DIA) {
            // TEX-DIA uses phases 1, 2, 3, 5, 6, and 7 thus skip card for phase 4 and 8
            if (psiv_ph_ovlp > 4)
                lsiv_card--;
            if (psiv_ph_ovlp > 8)
                lsiv_card--;
            // TEX-DIA has PARAMS.TEXAS_MODEL_NOT overlaps
            if (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NPN) {
                lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - PARAMS.TEXAS_MODEL_NPN);
            }
        }
        else if ((Intersection.mbov_is_ic_NEMA || Intersection.mbov_is_ic_HARDWARE) && (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NPN)) {
            lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - PARAMS.TEXAS_MODEL_NPN);
        }
        lsiv_member = Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + psiv_ph_ovlp;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GreenIntSeq.readFromCards reading " + lstv_name);

        // read data from cards
        lsiv_col = lclv_tx_fmt.msia_fo[lsiv_member];
        lclv_tx_fmt.msia_fo[lsiv_member] = psiv_col;
        msta_green_int_seq[psiv_ph_ovlp] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_GREEN_INT_SEQ, lsiv_member, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GreenIntSeq fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GreenIntSeq.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_ph_ovlp, // phase/overlap number (1-16 is phase 1-16, 17-32 is
                                               // overlap A-P)
            int psiv_leg, // leg number
            int psiv_lane, // inbound lane number
            int psiv_col // column number on card
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_last_col;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_name;

        // writeToCards writes all GreenIntSeq fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GreenIntSeq.writeToCards psiv_ph_ovlp=" + psiv_ph_ovlp + " psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane
                    + " psiv_col=" + psiv_col);
        // check parameters
        if ((psiv_ph_ovlp < 1) || (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NGI)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: psiv_ph_ovlp = " + psiv_ph_ovlp + " is < 1 or > " + PARAMS.TEXAS_MODEL_NGI + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            // special code that must also be performed by the menu system
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending lane number
        lsiv_lane_beg = 1;
        lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
            // write leg 3 green interval sequence for free u-turn lane from inbound lane 0
            // special code that must also be performed by the menu system
            // write leg 6 green interval sequence for free u-turn lane from inbound lane 0
            // special code that must also be performed by the menu system
            lsiv_lane_beg = 0;
        }

        // check parameters
        if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GREEN_INT_SEQ];

        // check parameters
        lsiv_last_col = Intersection.TX_FMT_NC - lclv_tx_fmt.msia_fs[Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01] + 1;
        if ((psiv_col < 1) || (psiv_col > lsiv_last_col)) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: psiv_col = " + psiv_col + " is < 1 or > " + lsiv_last_col + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is signal controlled
        if (!Intersection.mbov_is_ic_signal_controlled) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control
                    + "\" is not \"PRETIMED\", \"SEMI-ACT\", \"FULL-ACT\", \"TEX-DIA\", \"NEMA\", or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if signal sequence card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_GRN_INT_SEQ_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in GreenIntSeq.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_first_grn_int_seq_card - 1 + psiv_ph_ovlp;
        if (Intersection.mbov_is_ic_TEX_DIA) {
            // TEX-DIA uses phases 1, 2, 3, 5, 6, and 7 thus skip card for phase 4 and 8
            if (psiv_ph_ovlp > 4)
                lsiv_card--;
            if (psiv_ph_ovlp > 8)
                lsiv_card--;
            // TEX-DIA has PARAMS.TEXAS_MODEL_NOT overlaps
            if (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NPN) {
                lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - PARAMS.TEXAS_MODEL_NPN);
            }
        }
        else if ((Intersection.mbov_is_ic_NEMA || Intersection.mbov_is_ic_HARDWARE) && (psiv_ph_ovlp > PARAMS.TEXAS_MODEL_NPN)) {
            lsiv_card += (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - PARAMS.TEXAS_MODEL_NPN);
        }
        lsiv_member = Intersection.TX_FMT_SIM_GREEN_INT_SEQ_PHASE_01 - 1 + psiv_ph_ovlp;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GreenIntSeq.writeToCards writing " + lstv_name);

        // write data to cards
        lsiv_col = lclv_tx_fmt.msia_fo[lsiv_member];
        lclv_tx_fmt.msia_fo[lsiv_member] = psiv_col;
        Intersection.writeStringToCard(msta_green_int_seq[psiv_ph_ovlp], lstv_name, Intersection.TX_FMT_SIM_GREEN_INT_SEQ, lsiv_member, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GreenIntSeq

/******************************************************************************/
/* GreenIntSeq.java */
/******************************************************************************/
