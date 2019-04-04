package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              Overlap_Def.java                              */
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

class Overlap_Def {

    int msia_phase[]; /* list of phases don't use msia_phase[0] */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_NEMA_OVLP_DEF]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 1] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 2] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_02]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 3] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_03]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 4] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_04]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 5] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_05]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 6] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_06]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 7] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_07]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 8] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_08]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[
    // 9] .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_09]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[10]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_10]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[11]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_11]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[12]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_12]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[13]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_13]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[14]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_14]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[15]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_15]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_overlap_def[lsiv_overlap].msia_phase[16]
    // .mclv_aux.msia_stat[TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_16]

    public Overlap_Def() {
        msia_phase = new int[PARAMS.TEXAS_MODEL_NPN + 1];
        mclv_aux = new TX_Aux();
    } // end of method Overlap_Def

    public void checkForErrors(int psiv_overlap // overlap number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_member;
        int lsiv_phase;
        String lstv_name;

        // checkForErrors checks all Overlap_Def data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Overlap_Def.checkForErrors psiv_overlap=" + psiv_overlap);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set intersection control Intersection.mbov_is_ic_* values
        Intersection.check_and_set_intersection_control();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_NON + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_overlap < 1) || (psiv_overlap > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov)) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: psiv_overlap = " + psiv_overlap + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_OVLP_DEF];
        lsiv_member = Intersection.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_overlap));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Overlap_Def.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_phase = 1; lsiv_phase <= (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - 1); lsiv_phase++) {
            if (msia_phase[lsiv_phase] == 0)
                break;
            lsiv_field = lsiv_member + lsiv_phase;
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Overlap_Def.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Overlap_Def.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_overlap // overlap number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_member;
        int lsiv_no_of_phases;
        int lsiv_phase;
        String lstv_name;
        String lstv_overlap;

        // printToFile prints all Overlap_Def data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Overlap_Def.printToFile psiv_overlap=" + psiv_overlap);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set intersection control Intersection.mbov_is_ic_* values
        Intersection.check_and_set_intersection_control();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_NON + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_overlap < 1) || (psiv_overlap > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov)) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: psiv_overlap = " + psiv_overlap + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        switch (psiv_overlap) {
            case 1:
                lstv_overlap = "A";
                break;

            case 2:
                lstv_overlap = "B";
                break;

            case 3:
                lstv_overlap = "C";
                break;

            case 4:
                lstv_overlap = "D";
                break;

            case 5:
                lstv_overlap = "E";
                break;

            case 6:
                lstv_overlap = "F";
                break;

            case 7:
                lstv_overlap = "G";
                break;

            case 8:
                lstv_overlap = "H";
                break;

            case 9:
                lstv_overlap = "I";
                break;

            case 10:
                lstv_overlap = "J";
                break;

            case 11:
                lstv_overlap = "K";
                break;

            case 12:
                lstv_overlap = "L";
                break;

            case 13:
                lstv_overlap = "M";
                break;

            case 14:
                lstv_overlap = "N";
                break;

            case 15:
                lstv_overlap = "O";
                break;

            case 16:
                lstv_overlap = "P";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Overlap_Def.printToFile: psiv_overlap = " + psiv_overlap + " is < 1 or > 4.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_OVLP_DEF];
        lsiv_member = Intersection.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_overlap));

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Overlap_Def.printToFile printing " + lstv_name);

        // find the last non-default phase defined
        lsiv_no_of_phases = 0;
        for (lsiv_phase = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - 1; lsiv_phase >= 1; lsiv_phase--) {
            if (mclv_aux.msia_stat[lsiv_member + lsiv_phase] == Intersection.TX_DEFAULT) {
                continue;
            }
            lsiv_no_of_phases = lsiv_phase;
            break;
        }

        // if no overlaps then print message and return
        if (lsiv_no_of_phases == 0) {
            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintSIM_check_newpage(1);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print intersection type
            Intersection.filesPrintSIM_CommentToFile("Overlap " + lstv_overlap + " is not defined", Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT,
                    Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(lsiv_no_of_phases);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        for (lsiv_phase = 1; lsiv_phase <= lsiv_no_of_phases; lsiv_phase++) {
            if (msia_phase[lsiv_phase] == 0)
                break;
            // print data to a file
            Intersection.filesPrintSIM_IntToFile(msia_phase[lsiv_phase], lstv_name, Intersection.TX_FMT_SIM_NEMA_OVLP_DEF, lsiv_member + lsiv_phase, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_overlap // overlap number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_member;
        int lsiv_phase;
        String lstv_name;

        // readFromCards reads all Overlap_Def fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Overlap_Def.readFromCards psiv_overlap=" + psiv_overlap);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set intersection control Intersection.mbov_is_ic_* values
        Intersection.check_and_set_intersection_control();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_NON + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_overlap < 1) || (psiv_overlap > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov)) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: psiv_overlap = " + psiv_overlap + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if first non-NEMA clear to or NEMA/HARDWARE movement text card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if SIM version is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_OVLP_DEF];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card + 3 + psiv_overlap;
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver >= 6.00)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > 0)) {
            lsiv_card++;
        }
        lsiv_member = Intersection.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_overlap));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Overlap_Def.readFromCards reading " + lstv_name);

        for (lsiv_phase = 1; lsiv_phase <= (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - 1); lsiv_phase++) {
            // read data from cards
            msia_phase[lsiv_phase] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_NEMA_OVLP_DEF, lsiv_member + lsiv_phase, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (msia_phase[lsiv_phase] == 0)
                break;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Overlap_Def fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Overlap_Def.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_overlap // overlap number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_member;
        int lsiv_no_of_phases;
        int lsiv_phase;
        String lstv_name;

        // writeToCards writes all Overlap_Def fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Overlap_Def.writeToCards psiv_overlap=" + psiv_overlap);
        // check if number of phases is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check number of phases for errors
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases < PARAMS.TEXAS_MODEL_MINNPH) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_NO_OF_PHASES] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases + " is < " + PARAMS.TEXAS_MODEL_MINNPH + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check and set intersection control Intersection.mbov_is_ic_* values
        Intersection.check_and_set_intersection_control();
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // check if intersection control is NEMA
        if (!Intersection.mbov_is_ic_NEMA) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + " is not \"NEMA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check NEMA/HARDWARE number of overlaps for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov < 1)
                || (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov > PARAMS.TEXAS_MODEL_NON)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is < 1 or > " + PARAMS.TEXAS_MODEL_NON + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_overlap < 1) || (psiv_overlap > Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov)) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: psiv_overlap = " + psiv_overlap + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if NEMA overlap definition card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_NEMA_OVLP_DEF_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Overlap_Def.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ovlp_def_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_NEMA_OVLP_DEF];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_nema_ovlp_def_card - 1 + psiv_overlap;
        lsiv_member = Intersection.TX_FMT_SIM_NEMA_OVLP_DEF_PHASE_01 - 1;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_overlap));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Overlap_Def.writeToCards writing " + lstv_name);

        // find the last non-default phase defined
        lsiv_no_of_phases = 1;
        for (lsiv_phase = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_no_of_phases - 1; lsiv_phase >= 1; lsiv_phase--) {
            if (mclv_aux.msia_stat[lsiv_member + lsiv_phase] == Intersection.TX_DEFAULT) {
                continue;
            }
            lsiv_no_of_phases = lsiv_phase;
            break;
        }

        for (lsiv_phase = 1; lsiv_phase <= lsiv_no_of_phases; lsiv_phase++) {
            if (msia_phase[lsiv_phase] == 0)
                break;
            // write data to cards
            Intersection.writeIntToCard(msia_phase[lsiv_phase], lstv_name, Intersection.TX_FMT_SIM_NEMA_OVLP_DEF, lsiv_member + lsiv_phase, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Overlap_Def

/******************************************************************************/
/* Overlap_Def.java */
/******************************************************************************/
