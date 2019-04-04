package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                SIM_Ver.java                                */
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

class SIM_Ver {

    String mstv_sim_ver; /* version text (up to 5 characters) */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* the following variable is defined but is not read or written */
    double mdfv_sim_ver; /* version number */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_VERSION]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mstv_sim_ver
    // .mclv_aux.msia_stat[TX_FMT_SIM_VERSION_SIM_VER]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_version.mdfv_sim_ver

    public SIM_Ver() {
        mclv_aux = new TX_Aux();
    } // end of method SIM_Ver

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all SIM_Ver data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Ver.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VERSION];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("SIM_Ver.checkForErrors checking " + lstv_name);

        // if SIM_Ver defined then check
        if (Intersection.mbov_version_defined_sim) {
            // check all data for errors
            for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in SIM_Ver.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in SIM_Ver.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all SIM_Ver fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Ver.readFromCards");
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in SIM_Ver.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Ver.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if version card exists in the data file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card >= 3) {
            Intersection.mbov_version_defined_sim = true;
        }
        else {
            Intersection.mbov_version_defined_sim = false;
            mstv_sim_ver = "V6.00";
            mdfv_sim_ver = 6.00;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] = Intersection.TX_SET_BY_SOFTWARE;
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // set local data for reading data from cards
        // SIM_Ver card is card 2 if title card is 3
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VERSION];
        lsiv_card = 2;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("SIM_Ver.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_sim_ver = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_VERSION, Intersection.TX_FMT_SIM_VERSION_SIM_VER, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        try {
            mdfv_sim_ver = Double.parseDouble(mstv_sim_ver.substring(1).trim());
        }
        catch (Exception e) {
            Intersection.mstv_errorMessage = "Error in SIM_Ver.readFromCards: mstv_sim_ver does not have a valid number.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all SIM_Ver fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("SIM_Ver.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards write all SIM_Ver fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Ver.writeToCards");
        // if SIM Version not defined then return success
        if (!Intersection.mbov_version_defined_sim) {
            Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in SIM_Ver.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set version
        mstv_sim_ver = Intersection.IVERSN;
        mclv_aux.msia_stat[Intersection.TX_FMT_SIM_VERSION_SIM_VER] = Intersection.TX_SET_BY_SOFTWARE;
        Intersection.mbov_version_defined_sim = true;

        // set local data for writing data to cards
        // SIM_Ver card is card 2
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_VERSION];
        lsiv_card = 2;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("SIM_Ver.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_sim_ver, lstv_name, Intersection.TX_FMT_SIM_VERSION, Intersection.TX_FMT_SIM_VERSION_SIM_VER, Intersection.mcla_simdataCards,
                Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class SIM_Ver

/******************************************************************************/
/* SIM_Ver.java */
/******************************************************************************/
