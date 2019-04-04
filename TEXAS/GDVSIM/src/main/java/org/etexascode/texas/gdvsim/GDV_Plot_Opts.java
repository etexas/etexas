package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Plot_Opts.java                             */
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

class GDV_Plot_Opts {

    String mstv_path_type; /* Path type (PRIMARY or OPTION1 default=PRIMARY ) */

    String mstv_plot_option; /* Plot option (PLOT or NOPLOT default=PLOT ) */

    String mstv_plot_type; /* Plot type (SAME or SEPARAT default=SAME ) */

    int msiv_plot_scale_appr; /* Plot scale for approach in feet/inch (0=scaled) */

    int msiv_plot_scale_intr; /*
                               * Plot scale for intersection in feet/inch (0=scaled)
                               */

    int msiv_max_radius_path; /*
                               * Maximum radius for paths (min=100, max=9999, def=500)
                               */

    int msiv_min_dist_paths; /*
                              * Minimum distance between paths (min=6, max=20, def=10)
                              */

    double mdfv_plot_paper_wdth; /* Plot paper width in inches (def=7.5) */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_PLOT_OPTS]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_path_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PATH_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_option
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mstv_plot_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_appr
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_plot_scale_intr
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_max_radius_path
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.msiv_min_dist_paths
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_plot_opts.mdfv_plot_paper_wdth
    // .mclv_aux.msia_stat[TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH]

    public GDV_Plot_Opts() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_Plot_Opts

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Plot_Opts data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Plot_Opts.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PLOT_OPTS];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Plot_Opts.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Plot_Opts.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Plot_Opts.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        String lstv_name;

        // printToFile prints all GDV_Plot_Opts data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Plot_Opts.printToFile");

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PLOT_OPTS];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Plot_Opts.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(8);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_path_type, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_plot_option, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_plot_type, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_plot_scale_appr, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_plot_scale_intr, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_max_radius_path, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_min_dist_paths, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_plot_paper_wdth, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all GDV_Plot_Opts fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Plot_Opts.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Plot_Opts.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if plot opts card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PLOT_OPTS_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Plot_Opts.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PLOT_OPTS];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Plot_Opts.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_path_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_plot_option = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_plot_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_plot_scale_appr = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_plot_scale_intr = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_max_radius_path = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_min_dist_paths = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_plot_paper_wdth = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set par opt 2 card to plot opts card + 1
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card + 1;
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PAR_OPT_2_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Plot_Opts.readFromCards msiv_par_opt_2_card="
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card + "  "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.status(Intersection.TX_FMT_GDV_HEADER_PAR_OPT_2_CARD));

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Plot_Opts fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Plot_Opts.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all GDV_Plot_Opts fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Plot_Opts.writeToCards");
        // check if plot opts card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PLOT_OPTS_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Plot_Opts.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PLOT_OPTS];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Plot_Opts.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_path_type, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PATH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_plot_option, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_OPTION, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_plot_type, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_plot_scale_appr, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_APPR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_plot_scale_intr, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_SCALE_INTR, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_max_radius_path, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MAX_RADIUS_PATH, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeIntToCard(msiv_min_dist_paths, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_MIN_DIST_PATHS, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write parameter if non-default value
        if (mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH] != Intersection.TX_DEFAULT) {
            // write data to cards
            Intersection.writeDoubleToCard(mdfv_plot_paper_wdth, lstv_name, Intersection.TX_FMT_GDV_PLOT_OPTS, Intersection.TX_FMT_GDV_PLOT_OPTS_PLOT_PAPER_WDTH, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Plot_Opts

/******************************************************************************/
/* GDV_Plot_Opts.java */
/******************************************************************************/
