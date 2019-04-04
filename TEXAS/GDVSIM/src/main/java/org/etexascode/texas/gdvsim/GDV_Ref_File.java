package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Ref_File.java                              */
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

class GDV_Ref_File {

    String mstv_gdv_ref_file; /* GDV reference file name for GDV data */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GDV_REF_FILE]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_gdv_ref_file.mstv_gdv_ref_file
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE]

    public GDV_Ref_File() {
        mstv_gdv_ref_file = new String();
        mclv_aux = new TX_Aux();
    } // end of method GDV_Ref_File

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Ref_File data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GDV_Ref_File.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_FILE];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GDV_Ref_File.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_File.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_File.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
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
        int lsiv_fileSeparator;
        String lstv_gdvdataFile;
        String lstv_name;

        // readFromCards reads all GDV_Ref_File fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_File.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_File.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if last SIM data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_File.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_FILE];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card + 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_File.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_gdv_ref_file = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_FILE, Intersection.TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;
        lsiv_fileSeparator = mstv_gdv_ref_file.lastIndexOf(File.separator);
        if ((lsiv_fileSeparator >= 0) && (lsiv_fileSeparator < mstv_gdv_ref_file.length() - 1)) {
            mstv_gdv_ref_file = mstv_gdv_ref_file.substring(lsiv_fileSeparator + 1);
        }

        // check against actual GDV data
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_File.readFromCards Intersection.mstv_gdvdataFile = \"" + Intersection.mstv_gdvdataFile + "\".");
        lstv_gdvdataFile = Intersection.mstv_gdvdataFile.toString();
        lsiv_fileSeparator = lstv_gdvdataFile.lastIndexOf(File.separator);
        if ((lsiv_fileSeparator >= 0) && (lsiv_fileSeparator < lstv_gdvdataFile.length() - 1)) {
            lstv_gdvdataFile = lstv_gdvdataFile.substring(lsiv_fileSeparator + 1);
        }
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_File.readFromCards lstv_gdvdataFile = \"" + lstv_gdvdataFile + "\".");
        if (File.separator.equals("\\")) {
            if (!mstv_gdv_ref_file.equalsIgnoreCase(lstv_gdvdataFile)) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_File.readFromCards: "
                        + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_FILE].msta_desc[Intersection.TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE] + " = \"" + mstv_gdv_ref_file
                        + "\" is not \"" + lstv_gdvdataFile + "\".";
                Intersection.warningMessage();
            }
        }
        else {
            if (!mstv_gdv_ref_file.equals(lstv_gdvdataFile)) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_File.readFromCards: "
                        + Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_FILE].msta_desc[Intersection.TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE] + " = \"" + mstv_gdv_ref_file
                        + "\" is not \"" + lstv_gdvdataFile + "\".";
                Intersection.warningMessage();
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Ref_File fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Ref_File.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_fileSeparator;
        String lstv_name;

        // writeToCards writes all GDV_Ref_File fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_File.writeToCards");
        // check if gdv ref file name card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_NAME_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_File.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_name_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_FILE];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_name_card;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_File.writeToCards writing " + lstv_name);

        // set gdv ref file name
        mstv_gdv_ref_file = Intersection.mstv_gdvdataFile.toString();
        lsiv_fileSeparator = mstv_gdv_ref_file.lastIndexOf(File.separator);
        if ((lsiv_fileSeparator >= 0) && (lsiv_fileSeparator < mstv_gdv_ref_file.length() - 1)) {
            mstv_gdv_ref_file = mstv_gdv_ref_file.substring(lsiv_fileSeparator + 1);
        }
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE] = Intersection.TX_SET_BY_SOFTWARE;

        // write data to cards
        Intersection.writeStringToCard(mstv_gdv_ref_file, lstv_name, Intersection.TX_FMT_SIM_GDV_REF_FILE, Intersection.TX_FMT_SIM_GDV_REF_FILE_GDV_REF_FILE, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Ref_File

/******************************************************************************/
/* GDV_Ref_File.java */
/******************************************************************************/
