package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                             GDV_Ref_Offs.java                              */
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

class GDV_Ref_Offs {

    int mcla_gdv_ref_offset[]; /* GDV reference offset for inbound lane */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_GDV_REF_OFFS ]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[1]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_1]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[2]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_2]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[3]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_3]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[4]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_4]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[5]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_5]
    // mclv_TX_Inter_Data.mclv_tx_sim_data.mcla_gdv_ref_offs[lsiv_leg].mcla_gdv_ref_offset[6]
    // .mclv_aux.msia_stat[TX_FMT_SIM_GDV_REF_OFFS_OFFSET_6]

    public GDV_Ref_Offs() {
        mcla_gdv_ref_offset = new int[PARAMS.TEXAS_MODEL_NAL + 1];
        mclv_aux = new TX_Aux();
    } // end of method GDV_Ref_Offs

    public void checkForErrors(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all GDV_Ref_Offs data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GDV_Ref_Offs.checkForErrors psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_OFFS];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("GDV_Ref_Offs.checkForErrors checking " + lstv_name);
        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_Ref_Offs.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void readFromCards(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_num_lanes;
        String lstv_name;

        // readFromCards reads all GDV_Ref_Offs fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Offs.readFromCards psiv_leg=" + psiv_leg);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width > 0)))) {
            lsiv_lane = 0;
            lsiv_num_lanes = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + 1;
        }
        else {
            lsiv_lane = 1;
            lsiv_num_lanes = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        // check if last SIM data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LAST_SIM_DAT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_OFFS];
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card + 5 + 2 * psiv_leg;
        }
        else {
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_last_SIM_dat_card + 4 + 2 * (psiv_leg - 1);
        }
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("GDV_Ref_Offs.readFromCards reading " + lstv_name);

        if (lsiv_num_lanes >= 1) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_1,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 2) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_2,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 3) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_3,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 4) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_4,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 5) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_5,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 6) {
            // read data from cards
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_6,
                    Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // increment lane number
            lsiv_lane++;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_Ref_Offs fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_Ref_Offs.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_num_lanes;
        String lstv_name;

        // writeToCards writes all GDV_Ref_Offs fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Offs.writeToCards psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if (Intersection.mbov_is_diamond_interchange) {
            // for diamond interchange leg 0 holds lane control and signal settings for internal
            // lanes center to right
            // for diamond interchange leg N+1 holds lane control and signal settings for internal
            // lanes center to left
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((Intersection.mbov_is_diamond_interchange)
                && (Intersection.mbov_free_uturns_defined)
                && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[1].msiv_lane_width > 0))
                        || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[2].msiv_lane_width > 0)))) {
            lsiv_lane = 0;
            lsiv_num_lanes = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + 1;
        }
        else {
            lsiv_lane = 1;
            lsiv_num_lanes = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
        }

        // check if ref lane data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_REF_FILE_LANE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_lane_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_GDV_REF_OFFS];
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_lane_card + 1 + 2 * psiv_leg;
        }
        else {
            lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_ref_file_lane_card + 1 + 2 * (psiv_leg - 1);
        }
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("GDV_Ref_Offs.writeToCards reading " + lstv_name);

        if (lsiv_num_lanes >= 1) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_1] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_1, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 2) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_2] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_2, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 3) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_3] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_3, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 4) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_4] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_4, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 5) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_5] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_5, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        if (lsiv_num_lanes >= 6) {
            // check if offset is valid
            if (Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_Ref_Offs.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mcla_inb_lane[" + lsiv_lane
                        + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off is invalid.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // set value from actual GDV data
            mcla_gdv_ref_offset[lsiv_lane] = Intersection.mcla_leg[psiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off;
            mclv_aux.msia_stat[Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_6] = Intersection.TX_SET_BY_SOFTWARE;

            // write data to cards
            Intersection.writeIntToCard(mcla_gdv_ref_offset[lsiv_lane], lstv_name, Intersection.TX_FMT_SIM_GDV_REF_OFFS, Intersection.TX_FMT_SIM_GDV_REF_OFFS_OFFSET_6, Intersection.mcla_simdataCards,
                    Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // increment lane number
            lsiv_lane++;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_Ref_Offs

/******************************************************************************/
/* GDV_Ref_Offs.java */
/******************************************************************************/
