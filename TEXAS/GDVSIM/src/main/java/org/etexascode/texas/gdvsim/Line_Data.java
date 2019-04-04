package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Line_Data.java                               */
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

import java.awt.Color;
import java.io.*;
import java.lang.*;
import java.util.*;

class Line_Data {

    int msiv_line_number; /* Line number */

    int msiv_beg_x; /* Begin X coordinate */

    int msiv_beg_y; /* Begin Y coordinate */

    int msiv_end_x; /* End X coordinate */

    int msiv_end_y; /* End Y coordinate */

    /* the following variables are defined but are not read or written */
    Color mclv_color; /* line color */

    int msiv_view_type; /* view type */

    double mdfv_beg_x; /* Begin X coordinate */

    double mdfv_beg_y; /* Begin Y coordinate */

    double mdfv_end_x; /* End X coordinate */

    double mdfv_end_y; /* End Y coordinate */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_LINE_DATA]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_line_number
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_LINE_NUMBER]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_x
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_BEG_X ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_beg_y
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_BEG_Y ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_x
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_END_X ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_end_y
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_END_Y ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].mclv_color
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_COLOR ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mcla_line_data[lsiv_line].msiv_view_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_DATA_VIEW_TYPE ]

    public Line_Data() {
        mclv_aux = new TX_Aux();
    } // end of method Line_Data

    public void checkForErrors(int psiv_line // line number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Line_Data data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Line_Data.checkForErrors psiv_line=" + psiv_line);
        // check if number of lines is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_line < 1) || (psiv_line > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines)) {
            Intersection.mstv_errorMessage = "Error in Line_Data.checkForErrors: psiv_line = " + psiv_line + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_line));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Line_Data.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Line_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for line " + psiv_line + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Line_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for line " + psiv_line + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_line) {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // printToFile prints all Line_Data data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Line_Data.printToFile psiv_line=" + psiv_line);
        // check if number of lines is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_line < 1) || (psiv_line > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines)) {
            Intersection.mstv_errorMessage = "Error in Line_Data.printToFile: psiv_line = " + psiv_line + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_line));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Line_Data.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(5);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_line_number, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_LINE_NUMBER, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_beg_x, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_X, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_beg_y, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_Y, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_end_x, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_X, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_end_y, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_Y, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_line) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Line_Data fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Line_Data.readFromCards psiv_line=" + psiv_line);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Line_Data.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of lines is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_line < 1) || (psiv_line > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines)) {
            Intersection.mstv_errorMessage = "Error in Line_Data.readFromCards: psiv_line = " + psiv_line + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LINE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card + psiv_line;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_line));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Line_Data.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_line_number = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_LINE_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_beg_x = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_beg_y = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_end_x = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_end_y = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        mclv_color = Color.BLACK;
        msiv_view_type = Intersection.GDVSIM_VIEW_USER_LINES;
        mdfv_beg_x = msiv_beg_x;
        mdfv_beg_y = msiv_beg_y;
        mdfv_end_x = msiv_end_x;
        mdfv_end_y = msiv_end_y;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Line_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Line_Data.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_line) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Line_Data fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Line_Data.writeToCards psiv_line=" + psiv_line);
        // check if number of lines is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_line < 1) || (psiv_line > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines)) {
            Intersection.mstv_errorMessage = "Error in Line_Data.writeToCards: psiv_line = " + psiv_line + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if line card is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LINE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card + psiv_line;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_line));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Line_Data.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_line_number, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_LINE_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_beg_x, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_beg_y, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_BEG_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_end_x, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_X, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_end_y, lstv_name, Intersection.TX_FMT_GDV_LINE_DATA, Intersection.TX_FMT_GDV_LINE_DATA_END_Y, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Line_Data

/******************************************************************************/
/* Line_Data.java */
/******************************************************************************/
