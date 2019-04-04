package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               TX_Title.java                                */
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

class TX_Title {

    Card_Image mclv_text; /* title text (up to 80 characters) */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_TITLE]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_title.mclv_text.mstv_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_TITLE_TEXT]

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_TITLE]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_title.mclv_text.mstv_card
    // .mclv_aux.msia_stat[TX_FMT_SIM_TITLE_TEXT]

    public TX_Title() {
        mclv_text = new Card_Image();
        mclv_aux = new TX_Aux();
    } // end of method TX_Title

    public void checkForErrors(int psiv_tx_typ // TX_Title type (Intersection.TX_FMT_GDV_TITLE or
                                               // Intersection.TX_FMT_SIM_TITLE)
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all TX_Title data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("TX_Title.checkForErrors psiv_tx_typ=" + psiv_tx_typ);
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Title.checkForErrors psiv_tx_typ=" + psiv_tx_typ);
        // check parameters
        switch (psiv_tx_typ) {
            case Intersection.TX_FMT_GDV_TITLE:
            case Intersection.TX_FMT_SIM_TITLE:
                break;

            default:
                Intersection.mstv_errorMessage = "Error in TX_Title.checkForErrors: psiv_tx_typ = " + psiv_tx_typ + " is not " + Intersection.TX_FMT_GDV_TITLE + " or " + Intersection.TX_FMT_SIM_TITLE
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[psiv_tx_typ];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("TX_Title.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in TX_Title.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in TX_Title.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards(int psiv_tx_typ // TX_Title type (Intersection.TX_FMT_GDV_TITLE or
                                              // Intersection.TX_FMT_SIM_TITLE)
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all TX_Title fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("TX_Title.readFromCards psiv_tx_typ=" + psiv_tx_typ);
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("TX_Title.readFromCards psiv_tx_typ=" + psiv_tx_typ);
        // check parameters
        switch (psiv_tx_typ) {
            case Intersection.TX_FMT_GDV_TITLE:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("TX_Title.readFromCards for TX_FMT_GDV_TITLE");
                // check if GDV data cards have been read
                if (Intersection.msiv_gdvdataCardsRead == 0) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if title card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TITLE];
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card;
                lstv_name = lclv_tx_fmt.mstv_name.toString();
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("TX_Title.readFromCards reading " + lstv_name);

                // read data from cards
                mclv_text.mstv_card = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_TITLE, Intersection.TX_FMT_GDV_TITLE_TEXT, Intersection.mcla_gdvdataCards,
                        Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                break;

            case Intersection.TX_FMT_SIM_TITLE:
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("TX_Title.readFromCards for TX_FMT_SIM_TITLE");
                // check if SIM data cards have been read
                if (Intersection.msiv_simdataCardsRead == 0) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // check if title card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TITLE];
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card;
                lstv_name = lclv_tx_fmt.mstv_name.toString();
                /* debug */if (Intersection.mbov_debug_filesReadSIM)
                    System.out.println("TX_Title.readFromCards reading " + lstv_name);

                // read data from cards
                mclv_text.mstv_card = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_TITLE, Intersection.TX_FMT_SIM_TITLE_TEXT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                        Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                break;

            default:
                Intersection.mstv_errorMessage = "Error in TX_Title.readFromCards: psiv_tx_typ = " + psiv_tx_typ + " is not " + Intersection.TX_FMT_GDV_TITLE + " or " + Intersection.TX_FMT_SIM_TITLE
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all TX_Title fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("TX_Title.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_tx_typ // TX_Title type (Intersection.TX_FMT_GDV_TITLE or
                                             // Intersection.TX_FMT_SIM_TITLE)
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // writeToCards writes all TX_Title fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("TX_Title.writeToCards psiv_tx_typ=" + psiv_tx_typ);
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("TX_Title.writeToCards psiv_tx_typ=" + psiv_tx_typ);
        // check parameters
        switch (psiv_tx_typ) {
            case Intersection.TX_FMT_GDV_TITLE:
                /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                    System.out.println("TX_Title.writeToCards for TX_FMT_GDV_TITLE");
                // check if title card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TITLE];
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card;
                lstv_name = lclv_tx_fmt.mstv_name.toString();
                /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                    System.out.println("TX_Title.writeToCards writing " + lstv_name);

                // write data to cards
                Intersection.writeStringToCard(mclv_text.mstv_card, lstv_name, Intersection.TX_FMT_GDV_TITLE, Intersection.TX_FMT_GDV_TITLE_TEXT, Intersection.mcla_gdvdataCards,
                        Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                break;

            case Intersection.TX_FMT_SIM_TITLE:
                /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                    System.out.println("TX_Title.writeToCards for TX_FMT_SIM_TITLE");
                // check if title card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in TX_Title.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_TITLE];
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card;
                lstv_name = lclv_tx_fmt.mstv_name.toString();
                /* debug */if (Intersection.mbov_debug_filesWriteSIM)
                    System.out.println("TX_Title.writeToCards writing " + lstv_name);

                // write data to cards
                Intersection.writeStringToCard(mclv_text.mstv_card, lstv_name, Intersection.TX_FMT_SIM_TITLE, Intersection.TX_FMT_SIM_TITLE_TEXT, Intersection.mcla_simdataCards,
                        Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
                if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                    return;

                break;

            default:
                Intersection.mstv_errorMessage = "Error in TX_Title.writeToCards: psiv_tx_typ = " + psiv_tx_typ + " is not " + Intersection.TX_FMT_GDV_TITLE + " or " + Intersection.TX_FMT_SIM_TITLE
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class TX_Title

/******************************************************************************/
/* TX_Title.java */
/******************************************************************************/
