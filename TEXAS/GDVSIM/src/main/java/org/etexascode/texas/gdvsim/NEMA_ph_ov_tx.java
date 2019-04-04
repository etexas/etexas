package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             NEMA_ph_ov_tx.java                             */
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

class NEMA_ph_ov_tx {

    int msiv_num_chars_tx; /*
                            * NEMA/HARDWARE number of characters in phase and overlap text
                            */

    String msta_phase_text[]; /* NEMA/HARDWARE phase text */

    String msta_overlap_text[]; /* NEMA/HARDWARE overlap text */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEMA_PH_OV_TX]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msiv_num_chars_tx
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_02 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_03 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_04 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_05 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_06 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 7]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_07 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 8]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_08 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [ 9]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_09 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_10 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_11 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_12 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_13 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_14 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_15 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_phase_text [16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_16 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_B ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_C ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_D ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_E ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_F ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_G ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_H ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[09]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_I ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_J ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_K ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_L ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_M ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_N ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_O ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mclv_NEMA_ph_ov_tx.msta_overlap_text[16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_P ]

    public NEMA_ph_ov_tx() {
        msta_phase_text = new String[PARAMS.TEXAS_MODEL_NPN + 1];
        msta_overlap_text = new String[PARAMS.TEXAS_MODEL_NON + 1];
        mclv_aux = new TX_Aux();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPN + 1); lsiv_i++) {
            msta_phase_text[lsiv_i] = new String();
        }
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NON + 1); lsiv_i++) {
            msta_overlap_text[lsiv_i] = new String();
        }
    } // end of method NEMA_ph_ov_tx

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_max_overlaps;
        int lsiv_phase;
        String lstv_name;

        // checkForErrors checks all NEMA_ph_ov_tx data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_ph_ov_tx.checkForErrors");
        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
        }
        else {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
        }
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 0)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > lsiv_max_overlaps)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 0 or > " + lsiv_max_overlaps + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("NEMA_ph_ov_tx.checkForErrors checking " + lstv_name);

        // check all data for errors
        lsiv_field = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX;
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in NEMA_ph_ov_tx.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        for (lsiv_phase = 1; lsiv_phase <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            lsiv_field = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 - 1 + lsiv_phase;
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in NEMA_ph_ov_tx.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        } // end for ( lsiv_phase = 1 ; lsiv_phase <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_phase++ )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_max_overlaps;
        int lsiv_member;
        int lsiv_overlap;
        int lsiv_phase;
        String lstv_name;

        // readFromCards reads all NEMA_ph_ov_tx fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_ph_ov_tx.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
        }
        else {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
        }
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 0)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > lsiv_max_overlaps)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 0 or > " + lsiv_max_overlaps + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("NEMA_ph_ov_tx.readFromCards reading " + lstv_name);
        // read data from cards
        msiv_num_chars_tx = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01];
        for (lsiv_phase = 1; lsiv_phase <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 - 1 + lsiv_phase;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            lclv_tx_fmt.msia_fs[lsiv_member] = msiv_num_chars_tx;
            // read data from cards
            msta_phase_text[lsiv_phase] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += msiv_num_chars_tx;
        } // end for ( lsiv_phase = 1 ; lsiv_phase <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_phase++ )

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver >= 6.00) {
            lsiv_card++;
            lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A];
        }
        for (lsiv_overlap = 1; lsiv_overlap <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; lsiv_overlap++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A - 1 + lsiv_overlap;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            lclv_tx_fmt.msia_fs[lsiv_member] = msiv_num_chars_tx;
            // read data from cards
            msta_overlap_text[lsiv_overlap] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += msiv_num_chars_tx;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all NEMA_ph_ov_tx fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("NEMA_ph_ov_tx.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_max_overlaps;
        int lsiv_member;
        int lsiv_overlap;
        int lsiv_phase;
        String lstv_name;

        // writeToCards writes all NEMA_ph_ov_tx fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_ph_ov_tx.writeToCards");
        // check if intersection control is NEMA or HARDWARE
        if ((!Intersection.mbov_is_ic_NEMA) && (!Intersection.mbov_is_ic_HARDWARE)) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\" or \"HARDWARE\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if (gdvsim.gclv_inter.mbov_is_ic_NEMA) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NON;
        }
        else if (gdvsim.gclv_inter.mbov_is_ic_HARDWARE) {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_HOV;
        }
        else {
            lsiv_max_overlaps = PARAMS.TEXAS_MODEL_NOV;
        }
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 0)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > lsiv_max_overlaps)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 0 or > " + lsiv_max_overlaps + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if NEMA/HARDWARE phase and overlap text card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_PH_OV_TX_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in NEMA_ph_ov_tx.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ph_ov_tx_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ph_ov_tx_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("NEMA_ph_ov_tx.writeToCards writing " + lstv_name);

        // write data to cards
        msiv_num_chars_tx = lclv_tx_fmt.msia_def[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX];
        Intersection.writeIntToCard(msiv_num_chars_tx, lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_NUM_CHAR_TX, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01];
        for (lsiv_phase = 1; lsiv_phase <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phase++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_PHASE_01 - 1 + lsiv_phase;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            lclv_tx_fmt.msia_fs[lsiv_member] = msiv_num_chars_tx;
            // write data to cards
            Intersection.writeStringToCard(msta_phase_text[lsiv_phase], lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += msiv_num_chars_tx;
        } // end for ( lsiv_phase = 1 ; lsiv_phase <=
          // Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases ;
          // lsiv_phase++ )

        lsiv_card++;
        lsiv_col = lclv_tx_fmt.msia_fo[Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A];
        for (lsiv_overlap = 1; lsiv_overlap <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov; lsiv_overlap++) {
            lsiv_member = Intersection.TX_FMT_SIM_NEMA_PH_OV_TX_OVRLP_A - 1 + lsiv_overlap;
            lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
            lclv_tx_fmt.msia_fs[lsiv_member] = msiv_num_chars_tx;
            // write data to cards
            Intersection.writeStringToCard(msta_overlap_text[lsiv_overlap], lstv_name, Intersection.TX_FMT_SIM_NEMA_PH_OV_TX, lsiv_member, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_col += msiv_num_chars_tx;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class NEMA_ph_ov_tx

/******************************************************************************/
/* NEMA_ph_ov_tx.java */
/******************************************************************************/
