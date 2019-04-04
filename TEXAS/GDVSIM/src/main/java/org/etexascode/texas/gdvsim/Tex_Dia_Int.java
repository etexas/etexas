package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              Tex_Dia_Int.java                              */
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

class Tex_Dia_Int {

    double mdfv_ph_3_5_clr_grn_f467; /* phases 3-5 clearance green (fig 4,6,7) */

    double mdfv_ph_1_7_adv_grn_f346; /* phases 1-7 advance green (fig 3,4,6 ) */

    double mdfv_ph_2_6_adv_grn_f347; /* phases 2-6 advance green (fig 3,4, 7) */

    double mdfv_ph_2_trans_gap_f467; /* phase 2 transfer gap (fig 4,6,7) */

    double mdfv_ph_7_trans_gap_f467; /* phase 7 transfer gap (fig 4,6,7) */

    double mdfv_ph_1_6_adv_grn_min_f6; /*
                                        * phases 1-6 advance green minimum (fig 6 )
                                        */

    double mdfv_ph_1_6_adv_grn_max_f6; /*
                                        * phases 1-6 advance green maximum (fig 6 )
                                        */

    double mdfv_ph_2_7_adg_grn_f6; /* phases 2-7 advance green (fig 6 ) */

    double mdfv_ph_1_6_adv_grn_min_f7; /*
                                        * phases 1-6 advance green minimum (fig 7)
                                        */

    double mdfv_ph_1_6_adv_grn_max_f7; /*
                                        * phases 1-6 advance green maximum (fig 7)
                                        */

    double mdfv_ph_2_7_adv_grn_f7; /* phases 2-7 advance green (fig 7) */

    double mdfv_ph_3_5_clr_grn_f3; /* phases 3-5 clearance green (fig 3 ) */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_TEX_DIA_INT]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_3_5_clr_grn_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P35CG_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_1_7_adv_grn_f346
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P17AG_F346 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_2_6_adv_grn_f347
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P26AG_F347 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_2_trans_gap_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P2TG_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_7_trans_gap_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P7TG_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_1_6_adv_grn_min_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_1_6_adv_grn_max_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_2_7_adg_grn_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P27AG_F6 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_1_6_adv_grn_min_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_1_6_adv_grn_max_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_2_7_adv_grn_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P27AG_F7 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_int[lsiv_phase].mdfv_ph_3_5_clr_grn_f3
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_INT_P35CG_F3 ]

    public Tex_Dia_Int() {
        mclv_aux = new TX_Aux();
    } // end of method Tex_Dia_Int

    public void checkForErrors(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Tex_Dia_Int data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Tex_Dia_Int.checkForErrors psiv_figure=" + psiv_figure);
        // check parameters
        if ((psiv_figure != 3) && (psiv_figure != 4) && (psiv_figure != 6) && (psiv_figure != 7)) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.checkForErrors: psiv_figure = " + psiv_figure + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_INT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Tex_Dia_Int.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Tex_Dia_Int.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lines;
        String lstv_name;

        // printToFile reads all Tex_Dia_Int data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Tex_Dia_Int.printToFile psiv_figure=" + psiv_figure);
        // check parameters
        switch (psiv_figure) {
            case 3:
                lsiv_lines = 3;
                break;

            case 4:
                lsiv_lines = 5;
                break;

            case 6:
                lsiv_lines = 7;
                break;

            case 7:
                lsiv_lines = 7;
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.printToFile: psiv_figure = " + psiv_figure + " is not 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_INT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Tex_Dia_Int.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintSIM_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintSIM_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_3_5_clr_grn_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 3) || (psiv_figure == 4) || (psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_1_7_adv_grn_f346, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P17AG_F346, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 3) || (psiv_figure == 4) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_2_6_adv_grn_f347, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P26AG_F347, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_2_trans_gap_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P2TG_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_7_trans_gap_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P7TG_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_1_6_adv_grn_min_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_1_6_adv_grn_max_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_2_7_adg_grn_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_1_6_adv_grn_min_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F7, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_1_6_adv_grn_max_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F7, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_2_7_adv_grn_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F7, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 3)) {
            // print data to a file
            Intersection.filesPrintSIM_DoubleToFile(mdfv_ph_3_5_clr_grn_f3, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F3, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        boolean lbov_force_default;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Tex_Dia_Int fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Tex_Dia_Int.readFromCards psiv_figure=" + psiv_figure);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if timing card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TIMING_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card + 6;

        // check parameters
        switch (psiv_figure) {
            case 3:
                lsiv_card += 0;
                break;

            case 4:
                lsiv_card += 1;
                break;

            case 6:
                lsiv_card += 2;
                break;

            case 7:
                lsiv_card += 3;
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: psiv_figure = " + psiv_figure + " is not 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_INT];
        lbov_force_default = (psiv_figure != Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov);
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Tex_Dia_Int.readFromCards " + lstv_name);
        // read data from cards
        mdfv_ph_3_5_clr_grn_f467 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_1_7_adv_grn_f346 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P17AG_F346, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_2_6_adv_grn_f347 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P26AG_F347, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_2_trans_gap_f467 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P2TG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_7_trans_gap_f467 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P7TG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_1_6_adv_grn_min_f6 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_1_6_adv_grn_max_f6 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_2_7_adg_grn_f6 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_1_6_adv_grn_min_f7 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_1_6_adv_grn_max_f7 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_2_7_adv_grn_f7 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_ph_3_5_clr_grn_f3 = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F3, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Tex_Dia_Int fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Tex_Dia_Int.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Tex_Dia_Int fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Tex_Dia_Int.writeToCards psiv_figure=" + psiv_figure);
        // check if diamond interval card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PSFPS_NHMOV_DIAINT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_psfps_nhmov_diaint_card;

        // check parameters
        switch (psiv_figure) {
            case 3:
                lsiv_card += 0;
                break;

            case 4:
                lsiv_card += 1;
                break;

            case 6:
                lsiv_card += 2;
                break;

            case 7:
                lsiv_card += 3;
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.writeToCards: psiv_figure = " + psiv_figure + " is not 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number for errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Int.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_INT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Tex_Dia_Int.writeToCards writing " + lstv_name);

        // write "XXXX" if not selected figure number
        if (psiv_figure != Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
            Intersection.mcla_simdataCards[lsiv_card].mstv_card = "XXXX" + Intersection.mcla_simdataCards[lsiv_card].mstv_card.substring(4);
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_3_5_clr_grn_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_1_7_adv_grn_f346, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P17AG_F346, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_2_6_adv_grn_f347, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P26AG_F347, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_2_trans_gap_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P2TG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_7_trans_gap_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P7TG_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_1_6_adv_grn_min_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_1_6_adv_grn_max_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_2_7_adg_grn_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_1_6_adv_grn_min_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMIN_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_1_6_adv_grn_max_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P16AGMAX_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_2_7_adv_grn_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P27AG_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_ph_3_5_clr_grn_f3, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_INT, Intersection.TX_FMT_SIM_TEX_DIA_INT_P35CG_F3, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Tex_Dia_Int

/******************************************************************************/
/* Tex_Dia_Int.java */
/******************************************************************************/
