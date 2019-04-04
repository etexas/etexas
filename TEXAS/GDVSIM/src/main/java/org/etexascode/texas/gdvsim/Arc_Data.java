package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Arc_Data.java                                */
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

import org.etexascode.texas.gdvsim.Intersection;
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.awt.Color;
import java.io.*;
import java.lang.*;
import java.util.*;

class Arc_Data {

    int msiv_arc_number; /* Arc number */

    int msiv_center_x; /* Center X coordinate */

    int msiv_center_y; /* Center Y coordinate */

    int msiv_begin_azimuth; /* Beginning azimuth in degrees */

    int msiv_sweep_angle; /* Sweep angle in degrees */

    int msiv_radius; /* Radius */

    /* the following variables are defined but are not read or written */
    Color mclv_color; /* Arc Color */

    int msiv_view_type; /* View Type */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_ARC_DATA]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_arc_number
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_ARC_NUMBER ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_x
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_CENTER_X ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_center_y
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_CENTER_Y ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_begin_azimuth
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_sweep_angle
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_radius
    // .mclv_aux.msia_stat[TX_FMT_GDV_ARC_DATA_RADIUS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].mclv_color
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mcla_arc_data[lsiv_arc].msiv_view_type

    public Arc_Data() {
        mclv_aux = new TX_Aux();
    } // end of method Arc_Data

    public void checkForErrors(int psiv_arc // arc number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Arc_Data data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Arc_Data.checkForErrors");
        // check if number of arcs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_arc < 1) || (psiv_arc > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs)) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.checkForErrors: psiv_arc = " + psiv_arc + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_ARC_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_arc));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Arc_Data.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Arc_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for arc " + psiv_arc + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Arc_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for arc " + psiv_arc + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_arc) {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // printToFile prints all Arc_Data data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Arc_Data.printToFile psiv_arc=" + psiv_arc);
        // check if number of arcs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_arc < 1) || (psiv_arc > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs)) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.printToFile: psiv_arc = " + psiv_arc + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_ARC_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_arc));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Arc_Data.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(6);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_arc_number, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_ARC_NUMBER, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_center_x, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_X, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_center_y, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_Y, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        msiv_begin_azimuth = Intersection.bound_angle_0_to_359(msiv_begin_azimuth);
        Intersection.filesPrintGDV_IntToFile(msiv_begin_azimuth, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        msiv_sweep_angle = Intersection.bound_angle_minus_360_to_360(msiv_sweep_angle);
        Intersection.filesPrintGDV_IntToFile(msiv_sweep_angle, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_radius, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_RADIUS, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_arc) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Arc_Data fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Arc_Data.readFromCards psiv_arc=" + psiv_arc);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of arcs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_arc < 1) || (psiv_arc > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs)) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.readFromCards: psiv_arc = " + psiv_arc + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_ARC_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_ARC_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card + psiv_arc;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_arc));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Arc_Data.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_arc_number = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_ARC_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_center_x = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_center_y = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_begin_azimuth = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;
        msiv_begin_azimuth = Intersection.bound_angle_0_to_359(msiv_begin_azimuth);

        // read data from cards
        msiv_sweep_angle = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;
        msiv_sweep_angle = Intersection.bound_angle_minus_360_to_360(msiv_sweep_angle);

        // read data from cards
        msiv_radius = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        mclv_color = Color.BLACK;
        msiv_view_type = Intersection.GDVSIM_VIEW_USER_ARCS;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Arc_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Arc_Data.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_arc) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Arc_Data fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Arc_Data.writeToCards psiv_arc=" + psiv_arc);
        // check if number of arcs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_arc < 1) || (psiv_arc > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs)) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.writeToCards: psiv_arc = " + psiv_arc + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if arc header is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_ARC_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Arc_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_ARC_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card + psiv_arc;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_arc));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Arc_Data.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_arc_number, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_ARC_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_center_x, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_center_y, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_CENTER_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        msiv_begin_azimuth = Intersection.bound_angle_0_to_359(msiv_begin_azimuth);
        Intersection.writeIntToCard(msiv_begin_azimuth, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_BEGIN_AZIMUTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        msiv_sweep_angle = Intersection.bound_angle_minus_360_to_360(msiv_sweep_angle);
        Intersection.writeIntToCard(msiv_sweep_angle, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_SWEEP_ANGLE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_radius, lstv_name, Intersection.TX_FMT_GDV_ARC_DATA, Intersection.TX_FMT_GDV_ARC_DATA_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Arc_Data

/******************************************************************************/
/* Arc_Data.java */
/******************************************************************************/
