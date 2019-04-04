package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               SDR_Data.java                                */
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

class SDR_Data {

    int msiv_sdr_leg_number; /* SDR leg number */

    int msiv_sdr_setback; /* SDR setback */

    int msiv_sdr_offset; /* SDR offset */

    /* the following variables are defined but are not read or written */
    int msiv_sdr_x; /* SDR x coordinate */

    int msiv_sdr_y; /* SDR y coordinate */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_SDR_DATA]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_leg_number
    // .mclv_aux.msia_stat[TX_FMT_GDV_SDR_DATA_LEG_NUMBER]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_setback
    // .mclv_aux.msia_stat[TX_FMT_GDV_SDR_DATA_SETBACK ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_offset
    // .mclv_aux.msia_stat[TX_FMT_GDV_SDR_DATA_OFFSET ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_x
    // .mclv_aux.msia_stat[TX_FMT_GDV_SDR_DATA_X ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mcla_sdr_data[lsiv_sdr].msiv_sdr_y
    // .mclv_aux.msia_stat[TX_FMT_GDV_SDR_DATA_Y ]

    public SDR_Data() {
        mclv_aux = new TX_Aux();
    } // end of method SDR_Data

    public void checkForErrors(int psiv_sdr // SDR number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all SDR_Data data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("SDR_Data.checkForErrors psiv_sdr=" + psiv_sdr);
        // check if number of sdrs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_sdr < 1) || (psiv_sdr > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs)) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.checkForErrors: psiv_sdr = " + psiv_sdr + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SDR_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_sdr));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("SDR_Data.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in SDR_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for SDR " + psiv_sdr + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in SDR_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for SDR " + psiv_sdr + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_sdr) {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // printToFile prints all SDR_Data data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("SDR_Data.printToFile psiv_sdr=" + psiv_sdr);
        // check if number of sdrs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_sdr < 1) || (psiv_sdr > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs)) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.printToFile: psiv_sdr = " + psiv_sdr + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if leg number 0 then return success and do not print
        if (msiv_sdr_leg_number == 0) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SDR_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_sdr));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("SDR_Data.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(3);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sdr_leg_number, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_LEG_NUMBER, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sdr_setback, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_SETBACK, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sdr_offset, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_OFFSET, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_sdr) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all SDR_Data fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("SDR_Data.readFromCards psiv_sdr=" + psiv_sdr);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of sdrs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_sdr < 1) || (psiv_sdr > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs)) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.readFromCards: psiv_sdr = " + psiv_sdr + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if sdr card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SDR_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SDR_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card + psiv_sdr;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_sdr));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("SDR_Data.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_sdr_leg_number = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_LEG_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        if (msiv_sdr_leg_number == 0) {
            // special code that must also be performed by the menu system
            msiv_sdr_setback = 0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_DATA_SETBACK] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else {
            msiv_sdr_setback = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_SETBACK, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_DATA_SETBACK] = Intersection.TX_FROM_FILE;
        }

        // read data from cards
        if (msiv_sdr_leg_number == 0) {
            // special code that must also be performed by the menu system
            msiv_sdr_offset = 0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_DATA_OFFSET] = Intersection.TX_SET_BY_SOFTWARE;
        }
        else {
            msiv_sdr_offset = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_OFFSET, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_DATA_OFFSET] = Intersection.TX_FROM_FILE;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all SDR_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("SDR_Data.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_sdr) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all SDR_Data fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("SDR_Data.writeToCards psiv_sdr=" + psiv_sdr);
        // check if number of sdrs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_sdr < 1) || (psiv_sdr > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs)) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.writeToCards: psiv_sdr = " + psiv_sdr + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if sdr card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SDR_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SDR_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_SDR_DATA];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card + psiv_sdr;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_sdr));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("SDR_Data.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_sdr_leg_number, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_LEG_NUMBER, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        if (msiv_sdr_leg_number == 0) {
            Intersection.writeIntToCard(0, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_SETBACK, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            Intersection.writeIntToCard(msiv_sdr_setback, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_SETBACK, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        if (msiv_sdr_leg_number == 0) {
            Intersection.writeIntToCard(0, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_OFFSET, Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten,
                    lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            Intersection.writeIntToCard(msiv_sdr_offset, lstv_name, Intersection.TX_FMT_GDV_SDR_DATA, Intersection.TX_FMT_GDV_SDR_DATA_OFFSET, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class SDR_Data

/******************************************************************************/
/* SDR_Data.java */
/******************************************************************************/
