package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                GDV_Hdr.java                                */
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
import org.etexascode.texas.gdvsim.PARAMS;
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.io.*;
import java.lang.*;
import java.util.*;

class GDV_Hdr {

    String mstv_gdv_sig; /* signature */

    int msiv_gdv_title_card; /* card number for title */

    int msiv_curb_return_card; /* card number for curb return radii */

    int msia_leg_data_card[]; /*
                               * card number for leg data ([0]=- for diamond interchange)
                               */

    int msiv_other_card; /* card number for other data */

    int msiv_last_card; /* card number for last data */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* the following variables are defined but are not read or written */
    int msiv_arc_card; /* card number for the arc data */

    int msiv_line_card; /* card number for the line data */

    int msiv_sdr_card; /* card number for the sdr data */

    int msiv_plot_opts_card; /* card number for the plot opts data */

    int msiv_par_opt_2_card; /* card number for the par opt 2 data */

    int msiv_driver_mix_card; /* card number for the driver mix data */

    int msiv_veh_data_card; /* card number for the vehicle data */

    int msiv_drv_data_card; /* card number for the driver data */

    int msiv_special_veh_card; /* card number for the special vehicle data */

    int msiv_image_file_card; /* card number for the image file data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_HEADER]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mstv_gdv_sig
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_GDV_SIG ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_gdv_title_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_GDV_TITLE_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_CURB_RETURN_CARD]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [0]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_0 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_1 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_2 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_3 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_4 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_5 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LEG_DATA_CARD_6 ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_other_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_OTHER_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_last_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LAST_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_ARC_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_line_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_LINE_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_sdr_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_SDR_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_plot_opts_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_PLOT_OPTS_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_par_opt_2_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_PAR_OPT_2_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_driver_mix_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_DRIVER_MIX_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_veh_data_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_VEH_DATA_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_drv_data_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_DRV_DATA_CARD ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_special_veh_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_SPECIAL_VEH_CARD]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_image_file_card
    // .mclv_aux.msia_stat[TX_FMT_GDV_HEADER_IMAGE_FILE_CARD ]

    // order of GDV data records
    // TX_FMT_GDV_HEADER
    // TX_FMT_GDV_VERSION
    // TX_FMT_GDV_TITLE
    // TX_FMT_GDV_PAR_OPT
    // TX_FMT_GDV_CURB_RETURN
    // TX_FMT_GDV_FREE_UTURN
    // TX_FMT_GDV_DIAMOND_LEG
    // TX_FMT_GDV_DIAMOND_LANE
    // TX_FMT_GDV_LEG_GEO
    // TX_FMT_GDV_LANE_DATA
    // TX_FMT_GDV_TRAF_HDWAY & TX_FMT_GDV_HDWAY_PARAM
    // TX_FMT_GDV_TRAF_MIX
    // TX_FMT_GDV_TRAF_DEST
    // TX_FMT_GDV_VAR_TRAF_PER
    // TX_FMT_GDV_ARC_HEADER
    // TX_FMT_GDV_ARC_DATA
    // TX_FMT_GDV_LINE_HEADER
    // TX_FMT_GDV_LINE_DATA
    // TX_FMT_GDV_SDR_HEADER
    // TX_FMT_GDV_SDR_DATA
    // TX_FMT_GDV_PLOT_OPTS
    // TX_FMT_GDV_PAR_OPT_2
    // TX_FMT_GDV_DRIVER_MIX_01 ... TX_FMT_GDV_DRIVER_MIX_99
    // TX_FMT_GDV_VEH_LENGTH
    // TX_FMT_GDV_VEH_OPER_CHAR
    // TX_FMT_GDV_VEH_MAX_DECEL
    // TX_FMT_GDV_VEH_MAX_ACCEL
    // TX_FMT_GDV_VEH_MAX_VEL
    // TX_FMT_GDV_VEH_MIN_RAD
    // TX_FMT_GDV_VEH_CLASSIFY
    // TX_FMT_GDV_VEH_HEIGHT
    // TX_FMT_GDV_VEH_WIDTH
    // TX_FMT_GDV_VEH_UNITS_01 ... TX_FMT_GDV_VEH_UNITS_13 (99)
    // TX_FMT_GDV_DRV_OPER_CHAR
    // TX_FMT_GDV_DRV_PIJR_TIME
    // TX_FMT_GDV_SPECIAL_VEH
    // TX_FMT_GDV_IMAGE_FILE

    public GDV_Hdr() {
        msia_leg_data_card = new int[PARAMS.TEXAS_MODEL_NAL + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Hdr

    public void checkForErrors() {
        TX_Fmt lclv_tx_fmt = null;
        String lstv_name;

        // checkForErrors checks all GDV_Hdr data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Hdr.checkForErrors");
        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HEADER];
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_Hdr.checkForErrors checking " + lstv_name);

        // all fields are set by software therefore skip checkForErrors and return
        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards() {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        String lstv_name;

        // readFromCards reads all GDV_Hdr fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Hdr.readFromCards");
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // TX_FMT_GDV_HEADER card is always card 1
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HEADER];
        lsiv_card = 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Hdr.readFromCards reading " + lstv_name);

        // read data from cards
        mstv_gdv_sig = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_GDV_SIG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_DO_NOT_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_gdv_title_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_curb_return_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[0] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[1] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[2] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[3] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[4] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[5] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msia_leg_data_card[6] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_other_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_OTHER_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_last_card = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LAST_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // if last card is not number of cards then error
        if (msiv_last_card != Intersection.msiv_gdvdataCardsRead) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msiv_last_card = " + msiv_last_card + " is not " + Intersection.msiv_gdvdataCardsRead + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if card number > last card then error
        if (msiv_gdv_title_card > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msiv_gdv_title_card = " + msiv_gdv_title_card + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_curb_return_card > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msiv_curb_return_card = " + msiv_curb_return_card + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Math.abs(msia_leg_data_card[0]) > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[0] = " + Math.abs(msia_leg_data_card[0]) + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[1] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[1] = " + msia_leg_data_card[1] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[2] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[2] = " + msia_leg_data_card[2] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[3] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[3] = " + msia_leg_data_card[3] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[4] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[4] = " + msia_leg_data_card[4] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[5] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[5] = " + msia_leg_data_card[5] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msia_leg_data_card[6] > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msia_leg_data_card[6] = " + msia_leg_data_card[6] + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (msiv_other_card > msiv_last_card) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.readFromCards: msiv_other_card = " + msiv_other_card + " is > " + msiv_last_card + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // if msia_leg_data_card[0] < 0 then this is a diamond interchange
        if (msia_leg_data_card[0] < 0) {
            Intersection.mbov_is_diamond_interchange = true;
            Intersection.mbov_free_uturns_defined = ((Math.abs(msia_leg_data_card[0]) - msiv_curb_return_card) >= 3);
            Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT].msia_min[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] = 4;
        }
        else {
            Intersection.mbov_is_diamond_interchange = false;
            Intersection.mbov_free_uturns_defined = false;
            Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_PAR_OPT].msia_min[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] = 3;
        }
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Hdr.readFromCards Intersection.mbov_is_diamond_interchange=" + Intersection.mbov_is_diamond_interchange);
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Hdr.readFromCards Intersection.mbov_free_uturns_defined   =" + Intersection.mbov_free_uturns_defined);

        // set arc card to other card
        msiv_arc_card = msiv_other_card;
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_ARC_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_Hdr.readFromCards msiv_arc_card ="
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_arc_card + "  "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.status(Intersection.TX_FMT_GDV_HEADER_ARC_CARD));

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Hdr fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Hdr.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards() {
        boolean lbov_one_card;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_leg;
        int lsiv_traffic_mix_cards;
        int lsiv_veh;
        int lsiv_vtp;
        String lstv_name;

        // writeToCards write all GDV_Hdr fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards");
        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set the number of cards for traffic mix
        lsiv_traffic_mix_cards = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15);

        // set GDV version defined
        Intersection.mbov_version_defined_gdv = true;

        // set signature
        mstv_gdv_sig = " #%&$DATAGDVC2=";

        // set number of vehicle attributes
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_num_veh_attributes = 10;
        Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NUM_VEH_ATTRIB] = Intersection.TX_SET_BY_SOFTWARE;

        // set card numbers
        // TX_FMT_GDV_HEADER card is always card 1
        if (Intersection.mbov_version_defined_gdv) {
            msiv_gdv_title_card = 3; // TX_FMT_GDV_HEADER
                                     // TX_FMT_GDV_VERSION
        }
        else {
            msiv_gdv_title_card = 2; // TX_FMT_GDV_HEADER
        }

        msiv_curb_return_card = msiv_gdv_title_card + 2; // TX_FMT_GDV_TITLE
                                                         // TX_FMT_GDV_PAR_OPT

        for (lsiv_leg = 0; lsiv_leg <= PARAMS.TEXAS_MODEL_NAL; lsiv_leg++) {
            msia_leg_data_card[lsiv_leg] = 0;
        }

        if (Intersection.mbov_is_diamond_interchange) {
            // check if diamond number of inbound lanes to Right Intersection is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            // check if diamond number of inbound lanes to Left Intersection is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            if (Intersection.mbov_free_uturns_defined) {
                msia_leg_data_card[0] = msiv_curb_return_card + 3; // TX_FMT_GDV_CURB_RETURN
                                                                   // TX_FMT_GDV_FREE_UTURN
                                                                   // TX_FMT_GDV_FREE_UTURN

                msia_leg_data_card[1] = msia_leg_data_card[0] + 1 // TX_FMT_GDV_DIAMOND_LEG
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right
                        // TX_FMT_GDV_DIAMOND_LANE
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left; // TX_FMT_GDV_DIAMOND_LANE
            }
            else {
                msia_leg_data_card[0] = msiv_curb_return_card + 1; // TX_FMT_GDV_CURB_RETURN

                msia_leg_data_card[1] = msia_leg_data_card[0] + 1 // TX_FMT_GDV_DIAMOND_LEG
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right
                        // TX_FMT_GDV_DIAMOND_LANE
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left; // TX_FMT_GDV_DIAMOND_LANE
            }
        }
        else {
            msia_leg_data_card[0] = 0; // no diamond interchange

            msia_leg_data_card[1] = msiv_curb_return_card + 1; // TX_FMT_GDV_CURB_RETURN
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        msia_leg_data_card[2] = msia_leg_data_card[1] + 1 // TX_FMT_GDV_LEG_GEO
                + Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                // TX_FMT_GDV_LANE_DATA
                + Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA

        if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
            msia_leg_data_card[2] += (1 // TX_FMT_GDV_TRAF_HDWAY
                    + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                    + 1); // TX_FMT_GDV_TRAF_DEST
            for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                msia_leg_data_card[2] += 1; // TX_FMT_GDV_VAR_TRAF_PER
                // check if varying traffic period traffic mix is valid
                if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[1].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                    msia_leg_data_card[2] += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                }
            }
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        msia_leg_data_card[3] = msia_leg_data_card[2] + 1 // TX_FMT_GDV_LEG_GEO
                + Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                // TX_FMT_GDV_LANE_DATA
                + Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA
        if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
            msia_leg_data_card[3] += (1 // TX_FMT_GDV_TRAF_HDWAY
                    + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                    + 1); // TX_FMT_GDV_TRAF_DEST
            for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                msia_leg_data_card[3] += 1; // TX_FMT_GDV_VAR_TRAF_PER
                // check if varying traffic period traffic mix is valid
                if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[2].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                    msia_leg_data_card[3] += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                }
            }
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of varying traffic periods is valid
        if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        lsiv_card = msia_leg_data_card[3] + 1 // TX_FMT_GDV_LEG_GEO
                + Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                // TX_FMT_GDV_LANE_DATA
                + Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA

        if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
            lsiv_card += (1 // TX_FMT_GDV_TRAF_HDWAY
                    + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                    + 1); // TX_FMT_GDV_TRAF_DEST
            for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                lsiv_card += 1; // TX_FMT_GDV_VAR_TRAF_PER
                // check if varying traffic period traffic mix is valid
                if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[3].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                    lsiv_card += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                }
            }
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of outbound lanes is valid
            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of varying traffic periods is valid
            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            msia_leg_data_card[4] = lsiv_card;
            lsiv_card = msia_leg_data_card[4] + 1 // TX_FMT_GDV_LEG_GEO
                    + Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                    // TX_FMT_GDV_LANE_DATA
                    + Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA

            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                lsiv_card += (1 // TX_FMT_GDV_TRAF_HDWAY
                        + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                        + 1); // TX_FMT_GDV_TRAF_DEST
                for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                    lsiv_card += 1; // TX_FMT_GDV_VAR_TRAF_PER
                    // check if varying traffic period traffic mix is valid
                    if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[4].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                        lsiv_card += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                    }
                }
            }
        } // end if ( Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
          // >= 4 )

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of outbound lanes is valid
            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of varying traffic periods is valid
            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            msia_leg_data_card[5] = lsiv_card;
            lsiv_card = msia_leg_data_card[5] + 1 // TX_FMT_GDV_LEG_GEO
                    + Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                    // TX_FMT_GDV_LANE_DATA
                    + Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA

            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                lsiv_card += (1 // TX_FMT_GDV_TRAF_HDWAY
                        + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                        + 1); // TX_FMT_GDV_TRAF_DEST
                for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                    lsiv_card += 1; // TX_FMT_GDV_VAR_TRAF_PER
                    // check if varying traffic period traffic mix is valid
                    if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[5].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                        lsiv_card += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                    }
                }
            }
        } // end if ( Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
          // >= 5 )

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            // check if number of inbound lanes is valid
            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of outbound lanes is valid
            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // check if number of varying traffic periods is valid
            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            msia_leg_data_card[6] = lsiv_card;
            lsiv_card = msia_leg_data_card[6] + 1 // TX_FMT_GDV_LEG_GEO
                    + Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb
                    // TX_FMT_GDV_LANE_DATA
                    + Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out; // TX_FMT_GDV_LANE_DATA

            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb > 0) {
                lsiv_card += (1 // TX_FMT_GDV_TRAF_HDWAY
                        + lsiv_traffic_mix_cards // TX_FMT_GDV_TRAF_MIX
                        + 1); // TX_FMT_GDV_TRAF_DEST
                for (lsiv_vtp = 1; lsiv_vtp <= Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_num_var_traf_periods; lsiv_vtp++) {
                    lsiv_card += 1; // TX_FMT_GDV_VAR_TRAF_PER
                    // check if varying traffic period traffic mix is valid
                    if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_TRAFPER] == Intersection.TX_DATA_IS_INVALID) {
                        Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mcla_leg[6].mclv_TX_Leg_Data.mcla_var_traf_period[" + lsiv_vtp + "].mstv_tm is invalid.";
                        Intersection.errorMessage();
                        Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                        return;
                    }
                    if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mcla_var_traf_period[lsiv_vtp].mstv_tm.equals("YES")) {
                        lsiv_card += lsiv_traffic_mix_cards; // TX_FMT_GDV_TRAF_MIX
                    }
                }
            }
        } // end if ( Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs
          // >= 6 )

        msiv_other_card = lsiv_card;

        msiv_arc_card = msiv_other_card;

        // check if number of arcs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_ARC_HEADER_NUM_ARCS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        msiv_line_card = msiv_arc_card + 1 // TX_FMT_GDV_ARC_HEADER
                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_arcs.mclv_arc_header.msiv_num_arcs; // TX_FMT_GDV_ARC_DATA

        // check if number of lines is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LINE_HEADER_NUM_LINES] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        msiv_sdr_card = msiv_line_card + 1 // TX_FMT_GDV_LINE_HEADER
                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_lines.mclv_line_header.msiv_num_lines; // TX_FMT_GDV_LINE_DATA

        // check if number of sdrs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_SDR_HEADER_NUM_SDRS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        msiv_plot_opts_card = msiv_sdr_card + 1 // TX_FMT_GDV_SDR_HEADER
                + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_sdrs.mclv_sdr_header.msiv_num_sdrs; // TX_FMT_GDV_SDR_DATA

        msiv_par_opt_2_card = msiv_plot_opts_card + 1; // TX_FMT_GDV_PLOT_OPTS

        lsiv_card = msiv_par_opt_2_card + 1; // TX_FMT_GDV_PAR_OPT_2
        lbov_one_card = ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl <= 15)
                && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl <= 5));
        if (!lbov_one_card) {
            lsiv_card += (((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 25) / 26)
                    + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_drv_cl + 25) / 26));
        }

        // check if driver mix is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_MIX] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_mix.equals("YES")) {
            // check if number of vehicle classes is valid
            if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
            msiv_driver_mix_card = lsiv_card;
            lsiv_card = msiv_driver_mix_card + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl
            // TX_FMT_GDV_DRIVER_MIX_01
            // through
            ; // TX_FMT_GDV_DRIVER_MIX_99
        }
        else {
            msiv_driver_mix_card = 0;
        }

        // check if vehicle data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_VEHICLE_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_vehicle_data.equals("YES")) {
            msiv_veh_data_card = lsiv_card;
            lsiv_card = msiv_veh_data_card + 8 * ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 19) / 20)
                    + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 9) / 10);
            // 1 card for TX_FMT_GDV_VEH_LENGTH for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_OPER_CHAR for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_MAX_DECEL for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_MAX_ACCEL for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_MAX_VEL for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_MIN_RAD for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_CLASSIFY for every 10 vehicles
            // 1 card for TX_FMT_GDV_VEH_HEIGHT for every 20 vehicles
            // 1 card for TX_FMT_GDV_VEH_WIDTH for every 20 vehicles
            for (lsiv_veh = 1; lsiv_veh <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl; lsiv_veh++) {
                lsiv_card += ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_veh_units[lsiv_veh].msiv_veh_num_units + 3) / 4);
                // 1 card for TX_FMT_GDV_VEH_UNITS_01 for every 4 units per vehicle
            }
        }
        else {
            msiv_veh_data_card = 0;
        }

        // check if driver data is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_2_DRIVER_DATA] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Hdr.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt_2.mstv_driver_data.equals("YES")) {
            msiv_drv_data_card = lsiv_card;
            lsiv_card = msiv_drv_data_card + 2; // TX_FMT_GDV_DRV_OPER_CHAR
                                                // TX_FMT_GDV_DRV_PIJR_TIME
        }
        else {
            msiv_drv_data_card = 0;
        }

        if (Intersection.msiv_specialVehicles > 0) {
            msiv_special_veh_card = lsiv_card;
            lsiv_card = msiv_special_veh_card + Intersection.msiv_specialVehicles; // TX_FMT_GDV_SPECIAL_VEH
        }
        else {
            msiv_special_veh_card = 0;
        }

        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mstv_image_file_attached.equals("YES")) {
            msiv_image_file_card = lsiv_card;
            lsiv_card = msiv_image_file_card + 2; // TX_FMT_GDV_HEADER_IMAGE_FILE_CARD
        }
        else {
            msiv_image_file_card = 0;
        }

        msiv_last_card = lsiv_card - 1;

        // set diamond interchange
        if (Intersection.mbov_is_diamond_interchange) {
            msia_leg_data_card[0] = -Math.abs(msia_leg_data_card[0]);
        }

        // set number of GDV cards written to last card
        Intersection.msiv_gdvdataCardsWritten = msiv_last_card;

        // set all cards as set by software
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_SIG] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_OTHER_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_ARC_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LINE_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SDR_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PLOT_OPTS_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_PAR_OPT_2_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_DRIVER_MIX_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_VEH_DATA_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_DRV_DATA_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_SPECIAL_VEH_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_IMAGE_FILE_CARD] = Intersection.TX_SET_BY_SOFTWARE;
        mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LAST_CARD] = Intersection.TX_SET_BY_SOFTWARE;

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_gdv_title_card      = " + msiv_gdv_title_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_curb_return_card    = " + msiv_curb_return_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [0] = " + msia_leg_data_card[0]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [1] = " + msia_leg_data_card[1]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [2] = " + msia_leg_data_card[2]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [3] = " + msia_leg_data_card[3]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [4] = " + msia_leg_data_card[4]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [5] = " + msia_leg_data_card[5]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msia_leg_data_card   [6] = " + msia_leg_data_card[6]);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_other_card          = " + msiv_other_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_arc_card            = " + msiv_arc_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_line_card           = " + msiv_line_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_sdr_card            = " + msiv_sdr_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_plot_opts_card      = " + msiv_plot_opts_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_par_opt_2_card      = " + msiv_par_opt_2_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_driver_mix_card     = " + msiv_driver_mix_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_veh_data_card       = " + msiv_veh_data_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_drv_data_card       = " + msiv_drv_data_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_special_veh_card    = " + msiv_special_veh_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_image_file_card     = " + msiv_image_file_card);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards msiv_last_card           = " + msiv_last_card);

        // set local data for writing data to cards
        // TX_FMT_GDV_HEADER card is always card 1
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_HEADER];
        lsiv_card = 1;
        lstv_name = lclv_tx_fmt.mstv_name.toString();
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_Hdr.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeStringToCard(mstv_gdv_sig, lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_GDV_SIG, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_gdv_title_card, lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_GDV_TITLE_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_curb_return_card, lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[0], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[1], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[2], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[3], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[4], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[5], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msia_leg_data_card[6], lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_other_card, lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_OTHER_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_last_card, lstv_name, Intersection.TX_FMT_GDV_HEADER, Intersection.TX_FMT_GDV_HEADER_LAST_CARD, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Hdr

/******************************************************************************/
/* GDV_Hdr.java */
/******************************************************************************/
