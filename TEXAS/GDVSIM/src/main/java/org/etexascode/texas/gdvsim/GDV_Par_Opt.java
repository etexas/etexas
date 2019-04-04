package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                              GDV_Par_Opt.java                              */
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

class GDV_Par_Opt {

    int msiv_no_legs; /* number of legs */

    int msiv_time; /* total (startup+simulation) time in minutes */

    double mdfv_min_hw; /* minimum headway in seconds */

    int msiv_no_veh_cl; /* number of vehicle classes */

    int msiv_no_drv_cl; /* number of driver classes */

    int msiv_pct_lt; /* percent of left turning vehicles to enter in median lane */

    int msiv_pct_rt; /* percent of right turning vehicles to enter in curb lane */

    String mstv_plot_opt; /* create a plot data file ("YES" or "NO") */

    double mdfv_plot_size; /* size of geometry plot */

    String mstv_image_file_attached; /* image file attached ("YES" or "NO") */

    int msiv_num_veh_attributes; /*
                                  * number of vehicle attributes (V<6.00=6 and V=6.00=10)
                                  */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_PAR_OPT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_NO_LEGS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_time
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_TIME ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_min_hw
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_MIN_HW ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_NO_VEH_CL ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_NO_DRV_CL ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_lt
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_PCT_LT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_pct_rt
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_PCT_RT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_plot_opt
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_PLOT_OPT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mdfv_plot_size
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_PLOT_SIZE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes
    // .mclv_aux.msia_stat[TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB ]

    public GDV_Par_Opt() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_Par_Opt

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Par_Opt data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Par_Opt.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Par_Opt.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Par_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Par_Opt.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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

        // printToFile prints all GDV_Par_Opt data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Par_Opt.printToFile");
        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_Par_Opt.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(11);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_no_legs, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_time, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_TIME, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_min_hw, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_MIN_HW, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_no_veh_cl, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_no_drv_cl, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_pct_lt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_LT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_pct_rt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_RT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_plot_opt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_OPT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_DoubleToFile(mdfv_plot_size, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_SIZE, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_image_file_attached, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_num_veh_attributes, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB, mclv_aux,
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

        // readFromCards reads all GDV_Par_Opt fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // GDV_Par_Opt card is always after the GDV_Title card
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card + 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Par_Opt.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_no_legs = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_time = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_TIME, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead,
                lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_min_hw = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_MIN_HW, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_no_veh_cl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_no_drv_cl = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_pct_lt = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_LT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_pct_rt = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_RT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_plot_opt = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_OPT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mdfv_plot_size = Intersection.readDoubleFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_SIZE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_image_file_attached = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_num_veh_attributes = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Par_Opt fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Par_Opt.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all GDV_Par_Opt fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Par_Opt.writeToCards");
        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Par_Opt.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set number of vehicle attributes
        msiv_num_veh_attributes = 10;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] = Intersection.TX_SET_BY_SOFTWARE;

        // set local data for writing data to cards
        // GDV_Par_Opt card is always after the GDV_Title card
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card + 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Par_Opt.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_no_legs, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_time, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_TIME, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten,
                lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_min_hw, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_MIN_HW, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_no_veh_cl, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_no_drv_cl, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NO_DRV_CL, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_pct_lt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_LT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_pct_rt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PCT_RT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_plot_opt, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_OPT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeDoubleToCard(mdfv_plot_size, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_PLOT_SIZE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.writeStringToCard(mstv_image_file_attached, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_IMAGE_FILE_ATTACH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_num_veh_attributes, lstv_name, Intersection.TX_FMT_GDV_PAR_OPT, Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Par_Opt

/******************************************************************************/
/* GDV_Par_Opt.java */
/******************************************************************************/
