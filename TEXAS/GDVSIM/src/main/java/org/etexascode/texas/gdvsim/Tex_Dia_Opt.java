package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              Tex_Dia_Opt.java                              */
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

class Tex_Dia_Opt {

    String mstv_ena_d_3_ph_3_7_f467; /* enable D3 during phases 3-7 (fig 4,6,7) */

    String mstv_ena_d13_ph_3_7_f467; /* enable D13 during phases 3-7 (fig 4,6,7) */

    String mstv_ena_d_5_ph_2_5_f467; /* enable D5 during phases 2-5 (fig 4,6,7) */

    String mstv_ena_d13_ph_2_5_f467; /* enable D13 during phases 2-5 (fig 4,6,7) */

    String mstv_term_logic_ph_2_7_f3h; /*
                                        * terminate logic for phases 2-7 high (fig 3 )
                                        */

    String mstv_term_logic_ph_2_7_f3o; /*
                                        * terminate logic for phases 2-7 off (fig 3 )
                                        */

    String mstv_f6_opt_a_ph_1_6_tim_f6; /*
                                         * figure 6 option A (1-6 timing) (fig 6 )
                                         */

    String mstv_f6_opt_b_ph_2_7_tim_f6; /*
                                         * figure 6 option B (2-7 timing) (fig 6 )
                                         */

    String mstv_f6_opt_c_ph_6_skip_f6; /*
                                        * figure 6 option C (phase 6 skipping) (fig 6 )
                                        */

    String mstv_f7_opt_a_ph_1_6_tim_f7; /*
                                         * figure 7 option A (1-6 timing) (fig 7)
                                         */

    String mstv_f7_opt_b_ph_2_7_tim_f7; /*
                                         * figure 7 option B (2-7 timing) (fig 7)
                                         */

    String mstv_f7_opt_c_ph_1_skip_f7; /*
                                        * figure 7 option C (phase 1 skipping) (fig 7)
                                        */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_TEX_DIA_OPT ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_ena_d_3_ph_3_7_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_ena_d13_ph_3_7_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_ena_d_5_ph_2_5_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_ena_d13_ph_2_5_f467
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467 ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_term_logic_ph_2_7_f3h
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_term_logic_ph_2_7_f3o
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f6_opt_a_ph_1_6_tim_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f6_opt_b_ph_2_7_tim_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f6_opt_c_ph_6_skip_f6
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f7_opt_a_ph_1_6_tim_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f7_opt_b_ph_2_7_tim_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_tx_sig_setup.mcla_tex_dia_opt[lsiv_phase].mstv_f7_opt_c_ph_1_skip_f7
    // .mclv_aux.msia_stat[TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7]

    public Tex_Dia_Opt() {
        mclv_aux = new TX_Aux();
    } // end of method Tex_Dia_Opt

    public void checkForErrors(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Tex_Dia_Opt data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Tex_Dia_Opt.checkForErrors psiv_figure=" + psiv_figure);
        // check parameters
        if ((psiv_figure != 3) && (psiv_figure != 4) && (psiv_figure != 6) && (psiv_figure != 7)) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.checkForErrors: psiv_figure = " + psiv_figure + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or o number of overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number of errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.checkForErrors: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Tex_Dia_Opt.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Tex_Dia_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile reads all Tex_Dia_Opt data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Tex_Dia_Opt.printToFile psiv_figure=" + psiv_figure);
        // check parameters
        switch (psiv_figure) {
            case 3:
                lsiv_lines = 2;
                break;

            case 4:
                lsiv_lines = 4;
                break;

            case 6:
                lsiv_lines = 7;
                break;

            case 7:
                lsiv_lines = 7;
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.printToFile: psiv_figure = " + psiv_figure + " is ot 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number of errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.printToFile: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Tex_Dia_Opt.printToFile printing " + lstv_name);

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
            Intersection.filesPrintSIM_StringToFile(mstv_ena_d_3_ph_3_7_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_ena_d13_ph_3_7_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_ena_d_5_ph_2_5_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 4) || (psiv_figure == 6) || (psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_ena_d13_ph_2_5_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 3)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_term_logic_ph_2_7_f3h, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 3)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_term_logic_ph_2_7_f3o, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f6_opt_a_ph_1_6_tim_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f6_opt_b_ph_2_7_tim_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 6)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f6_opt_c_ph_6_skip_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f7_opt_a_ph_1_6_tim_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f7_opt_b_ph_2_7_tim_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        if ((psiv_figure == 7)) {
            // print data to a file
            Intersection.filesPrintSIM_StringToFile(mstv_f7_opt_c_ph_1_skip_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7, mclv_aux,
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

        // readFromCards reads all Tex_Dia_Opt fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Tex_Dia_Opt.readFromCards psiv_figure=" + psiv_figure);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if timing card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TIMING_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_timing_card + 6 + 4;

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
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: psiv_figure = " + psiv_figure + " is ot 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number of errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.readFromCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_OPT];
        lbov_force_default = (psiv_figure != Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov);
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Tex_Dia_Opt.readFromCards psiv_figure=" + psiv_figure);

        // read data from cards
        mstv_ena_d_3_ph_3_7_f467 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_ena_d13_ph_3_7_f467 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_ena_d_5_ph_2_5_f467 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_ena_d13_ph_2_5_f467 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_term_logic_ph_2_7_f3h = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_term_logic_ph_2_7_f3o = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f6_opt_a_ph_1_6_tim_f6 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6,
                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f6_opt_b_ph_2_7_tim_f6 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6,
                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f6_opt_c_ph_6_skip_f6 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f7_opt_a_ph_1_6_tim_f7 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7,
                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f7_opt_b_ph_2_7_tim_f7 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7,
                Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_f7_opt_c_ph_1_skip_f7 = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, lbov_force_default, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Tex_Dia_Opt fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Tex_Dia_Opt.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_figure // Texas Diamond Signal Controller Figure number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Tex_Dia_Opt fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Tex_Dia_Opt.writeToCards psiv_figure=" + psiv_figure);
        // check if diamond options card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_TEX_DIA_OPT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_tex_dia_opt_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_tex_dia_opt_card;

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
                Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.writeToCards: psiv_figure = " + psiv_figure + " is ot 3, 4, 6, or 7.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // check if intersection control is TEX-DIA
        if (!Intersection.mbov_is_ic_TEX_DIA) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control = \""
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_par_opt.mstv_inter_control + "\" is not \"TEX-DIA\".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if pretimed cycle length or diamond figure number or NEMA/HARDWARE number of
        // overlaps is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check diamond figure number of errors
        if ((Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 3)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 4)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 6)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov != 7)) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_HEADER];
            Intersection.mstv_errorMessage = "Error in Tex_Dia_Opt.writeToCards: " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_SIM_HEADER_PCLEN_DFIG_NHITLOV] + " = "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov + " is not 3, 4, 6, or 7.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TEX_DIA_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_figure));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Tex_Dia_Opt.writeToCards writing " + lstv_name);

        // write "XXX" if not selected figure number
        if (psiv_figure != Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_preclen_diafig_nemahitlov) {
            Intersection.mcla_simdataCards[lsiv_card].mstv_card = "XXX" + Intersection.mcla_simdataCards[lsiv_card].mstv_card.substring(3);
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // write data to cards
        Intersection.writeStringToCard(mstv_ena_d_3_ph_3_7_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED03P36_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_ena_d13_ph_3_7_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P37_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_ena_d_5_ph_2_5_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED05P25_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_ena_d13_ph_2_5_f467, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_ED13P25_F467, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_term_logic_ph_2_7_f3h, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3H, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_term_logic_ph_2_7_f3o, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_TLP27_F3O, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f6_opt_a_ph_1_6_tim_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTAP16T_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f6_opt_b_ph_2_7_tim_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTBP27T_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f6_opt_c_ph_6_skip_f6, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F6OPTCP6SK_F6, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f7_opt_a_ph_1_6_tim_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTAP16T_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f7_opt_b_ph_2_7_tim_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTBP27T_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_f7_opt_c_ph_1_skip_f7, lstv_name, Intersection.TX_FMT_SIM_TEX_DIA_OPT, Intersection.TX_FMT_SIM_TEX_DIA_OPT_F7OPTCP1SK_F7, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Tex_Dia_Opt

/******************************************************************************/
/* Tex_Dia_Opt.java */
/******************************************************************************/
