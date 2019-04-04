package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Line_Hdr.java                                */
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

class Line_Hdr {

    int msiv_num_lines; /* Number of Lines */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_LINE_HEADER]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines
    // .mclv_aux.msia_stat[TX_FMT_GDV_LINE_HEADER_NUM_LINES]

    public Line_Hdr() {
        mclv_aux = new TX_Aux();
        msiv_num_lines = 0;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] = Intersection.TX_SET_BY_SOFTWARE;
    } // end of method Line_Hdr

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Line_Hdr data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Line_Hdr.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_HEADER];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Line_Hdr.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Line_Hdr.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Line_Hdr.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all Line_Hdr fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Line_Hdr.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Line_Hdr.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if line card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LINE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Hdr.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_HEADER];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Line_Hdr.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_num_lines = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LINE_HEADER, Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set sdr card to line card + 1 + number of lines
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card + 1 + msiv_num_lines;
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SDR_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("msiv_sdr_card        " + "=" + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card + "  "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.status(Intersection.TX_FMT_GDV_HEADER_SDR_CARD));

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Line_Hdr fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Line_Hdr.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all Line_Hdr fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Line_Hdr.writeToCards");
        // check if line card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LINE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Line_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LINE_HEADER];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Line_Hdr.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_num_lines, lstv_name, Intersection.TX_FMT_GDV_LINE_HEADER, Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Line_Hdr

/******************************************************************************/
/* Line_Hdr.java */
/******************************************************************************/
