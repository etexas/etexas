package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             Pha_Sequence.java                              */
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

class Pha_Sequence {

    int msia_ph_sequence[]; /* phase sequence */

    String msta_ph_sequence[]; /* phase sequence */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_PHASE_SEQ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_02]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_03]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_04]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_05]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_06]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_07]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msia_ph_sequence[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_08]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_02]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_03]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_04]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_05]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_06]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_07]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_phase_sequence[lsiv_phseq].msta_ph_sequence[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_08]

    public static final int PHA_SEQUENCE_FS = 2;

    public Pha_Sequence() {
        msia_ph_sequence = new int[PARAMS.TEXAS_MODEL_NPH + 1];
        msta_ph_sequence = new String[PARAMS.TEXAS_MODEL_NPH + 1];
        mclv_aux = new TX_Aux();
        for (int lsiv_i = 0; lsiv_i < (PARAMS.TEXAS_MODEL_NPH + 1); lsiv_i++) {
            msta_ph_sequence[lsiv_i] = new String();
        }
    } // end of method Pha_Sequence

    public void checkForErrors(int psiv_phase // signal phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_member_chr;
        int lsiv_member_num;
        int lsiv_phseq;
        String lstv_name;

        // checkForErrors checks all Pha_Sequence data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Pha_Sequence.checkForErrors psiv_phase=" + psiv_phase);
        // check if intersection control is PRETIMED, SEMI-ACT, or FULL-ACT
        if ((!Intersection.mbov_is_ic_PRETIMED) && (!Intersection.mbov_is_ic_SEMI_ACT) && (!Intersection.mbov_is_ic_FULL_ACT)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"PRETIMED\", \"SEMI-ACT\", or \"FULL-ACT\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.checkForErrors: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PHASE_SEQ];
        lsiv_member_num = Intersection.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Pha_Sequence.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            lsiv_field = lsiv_member_num + lsiv_phseq;
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Pha_Sequence.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Pha_Sequence.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_phase // signal phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        int lsiv_member_num;
        int lsiv_phseq;
        String lstv_name;

        // printToFile prints all Pha_Sequence data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Pha_Sequence.printToFile psiv_phase=" + psiv_phase);
        // check if intersection control is PRETIMED, SEMI-ACT, or FULL-ACT
        if ((!Intersection.mbov_is_ic_PRETIMED) && (!Intersection.mbov_is_ic_SEMI_ACT) && (!Intersection.mbov_is_ic_FULL_ACT)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"PRETIMED\", \"SEMI-ACT\", or \"FULL-ACT\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.printToFile: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PHASE_SEQ];
        lsiv_member_num = Intersection.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Pha_Sequence.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        lsiv_lines = 0;
        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            if (msia_ph_sequence[lsiv_phseq] == 0)
                break;
            lsiv_lines++;
        }
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            if (msia_ph_sequence[lsiv_phseq] == 0)
                break;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msia_ph_sequence[lsiv_phseq], lstv_name, Intersection.TX_FMT_SIM_PHASE_SEQ, lsiv_member_num + lsiv_phseq, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_phase // signal phase number
    ) {
        boolean lboa_phaseUsed[];
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_member_chr;
        int lsiv_member_num;
        int lsiv_phseq;
        String lstv_def;
        String lstv_name;

        lboa_phaseUsed = new boolean[PARAMS.TEXAS_MODEL_NPH + 1];

        // readFromCards reads all Pha_Sequence fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Pha_Sequence.readFromCards psiv_phase=" + psiv_phase);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is PRETIMED, SEMI-ACT, or FULL-ACT
        if ((!Intersection.mbov_is_ic_PRETIMED) && (!Intersection.mbov_is_ic_SEMI_ACT) && (!Intersection.mbov_is_ic_FULL_ACT)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"PRETIMED\", \"SEMI-ACT\", or \"FULL-ACT\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PHASE_SEQ];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card - 1 + psiv_phase;
        lsiv_member_chr = Intersection.TX_FMT_SIM_PHASE_SEQ_PHASE_CHR_01 - 1;
        lsiv_member_num = Intersection.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Pha_Sequence.readFromCards reading " + lstv_name);

        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            lboa_phaseUsed[lsiv_phseq] = false;
        }

        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            // read data from cards
            msta_ph_sequence[lsiv_phseq] = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_PHASE_SEQ, lsiv_member_chr + lsiv_phseq, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            if (msta_ph_sequence[lsiv_phseq] == null) {
                msia_ph_sequence[lsiv_phseq] = 0;
            }
            else if (msta_ph_sequence[lsiv_phseq].trim().length() == 0) {
                msia_ph_sequence[lsiv_phseq] = 0;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("A")) {
                msia_ph_sequence[lsiv_phseq] = 1;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("B")) {
                msia_ph_sequence[lsiv_phseq] = 2;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("C")) {
                msia_ph_sequence[lsiv_phseq] = 3;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("D")) {
                msia_ph_sequence[lsiv_phseq] = 4;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("E")) {
                msia_ph_sequence[lsiv_phseq] = 5;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("F")) {
                msia_ph_sequence[lsiv_phseq] = 6;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("G")) {
                msia_ph_sequence[lsiv_phseq] = 7;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("H")) {
                msia_ph_sequence[lsiv_phseq] = 8;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("0")) {
                msia_ph_sequence[lsiv_phseq] = 0;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("1")) {
                msia_ph_sequence[lsiv_phseq] = 1;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("2")) {
                msia_ph_sequence[lsiv_phseq] = 2;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("3")) {
                msia_ph_sequence[lsiv_phseq] = 3;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("4")) {
                msia_ph_sequence[lsiv_phseq] = 4;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("5")) {
                msia_ph_sequence[lsiv_phseq] = 5;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("6")) {
                msia_ph_sequence[lsiv_phseq] = 6;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("7")) {
                msia_ph_sequence[lsiv_phseq] = 7;
            }
            else if (msta_ph_sequence[lsiv_phseq].equals("8")) {
                msia_ph_sequence[lsiv_phseq] = 8;
            }
            else {
                Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member_chr + lsiv_phseq] + " = "
                        + msta_ph_sequence[lsiv_phseq] + " is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            mclv_aux.msia_stat[lsiv_member_num + lsiv_phseq] = Intersection.TX_FROM_USER;

            if (msia_ph_sequence[lsiv_phseq] == 0)
                continue;

            if (lboa_phaseUsed[msia_ph_sequence[lsiv_phseq]]) {
                Intersection.mstv_errorMessage = "Error in Pha_Sequence.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_member_chr + lsiv_phseq] + " = "
                        + msia_ph_sequence[lsiv_phseq] + " has already been used.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            lboa_phaseUsed[msia_ph_sequence[lsiv_phseq]] = true;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Pha_Sequence fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Pha_Sequence.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_phase // signal phase number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_member_num;
        int lsiv_phseq;
        String lstv_name;

        // writeToCards writes all Pha_Sequence fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Pha_Sequence.writeToCards psiv_phase=" + psiv_phase);
        // check if intersection control is PRETIMED, SEMI-ACT, or FULL-ACT
        if ((!Intersection.mbov_is_ic_PRETIMED) && (!Intersection.mbov_is_ic_SEMI_ACT) && (!Intersection.mbov_is_ic_FULL_ACT)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"PRETIMED\", \"SEMI-ACT\", or \"FULL-ACT\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_phase < 1) || (psiv_phase > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases)) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.writeToCards: psiv_phase = " + psiv_phase + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Pha_Sequence.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_PHASE_SEQ];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card - 1 + psiv_phase;
        lsiv_member_num = Intersection.TX_FMT_SIM_PHASE_SEQ_PHASE_NUM_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_phase));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Pha_Sequence.writeToCards writing " + lstv_name);

        for (lsiv_phseq = 1; lsiv_phseq <= Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases; lsiv_phseq++) {
            if (msia_ph_sequence[lsiv_phseq] == 0)
                break;
            // write data to cards
            Intersection.writeIntToCard(msia_ph_sequence[lsiv_phseq], lstv_name, Intersection.TX_FMT_SIM_PHASE_SEQ, lsiv_member_num + lsiv_phseq, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Pha_Sequence

/******************************************************************************/
/* Pha_Sequence.java */
/******************************************************************************/
