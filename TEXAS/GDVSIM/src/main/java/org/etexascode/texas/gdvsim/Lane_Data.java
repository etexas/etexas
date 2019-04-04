package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Lane_Data.java                               */
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

class Lane_Data {

    int msiv_width; /* lane width */

    Mvmt_Code mclv_mvmt; /* movement code text */

    int msiv_inr_opn; /* length of open lane at intersection end */

    int msiv_otr_opn; /* length of open lane at outer end */

    int msiv_sl_off; /* stop line offset */

    int msiv_pct; /* percent of traffic to enter on this lane */

    String mstv_allowed_veh_type; /* vehicle types allowed to use this lane */

    /* the following variables are defined but are not read or written */
    int msiv_beg_1; /* beginning lane position 1 */

    int msiv_end_1; /* ending lane position 1 */

    int msiv_beg_2; /* beginning lane position 2 */

    int msiv_end_2; /* ending lane position 2 */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* msiv_inr_opn = 0 and msiv_otr_opn = 0 => all open */
    /* msiv_inr_opn > 0 and msiv_otr_opn = 0 => open stop line blocked entry */
    /* msiv_inr_opn = 0 and msiv_otr_opn > 0 => blocked stop line open entry */
    /* msiv_inr_opn > 0 and msiv_otr_opn > 0 => blocked in middle */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_LANE_DATA]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_WIDTH ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_MVMT ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_INR_OPN ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_OTR_OPN ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_SL_OFF ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_PCT ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_beg_1
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_BEG_1 ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_end_1
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_END_1 ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_beg_2
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_BEG_2 ]
    // mcla_leg[lsiv_leg].mcla_inb_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_end_2
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_END_2 ]

    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_WIDTH ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_MVMT ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_INR_OPN ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_OTR_OPN ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_SL_OFF ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_pct
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_PCT ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_beg_1
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_BEG_1 ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_end_1
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_END_1 ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_beg_2
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_BEG_2 ]
    // mcla_leg[lsiv_leg].mcla_out_lane[lsiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_end_2
    // .mclv_aux.msia_stat[TX_FMT_GDV_LANE_DATA_END_2 ]

    public Lane_Data() {
        mclv_mvmt = new Mvmt_Code();
        mclv_aux = new TX_Aux();
    } // end of method Lane_Data

    public void checkForErrors(int psiv_leg, // leg number
            int psiv_lane_inb_out, // LANE_INB or LANE_OUT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_inb_out;
        String lstv_name;

        // checkForErrors checks all Lane_Data data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Lane_Data.checkForErrors psiv_leg=" + psiv_leg + " psiv_lane_inb_out=" + psiv_lane_inb_out + " psiv_lane="
                    + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];

        // check parameters
        switch (psiv_lane_inb_out) {
            case Intersection.LANE_INB:
                // check if number of inbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // read leg 3 lane control for free u-turn lane and store in inbound lane 0
                    // read leg 6 lane control for free u-turn lane and store in inbound lane 0
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for checking data for errors
                lstv_inb_out = "Inbound";
                break;

            case Intersection.LANE_OUT:
                // check if number of outbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // read leg 3 lane control for free u-turn lane and store in inbound lane 0
                    // read leg 6 lane control for free u-turn lane and store in inbound lane 0
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for checking data for errors
                lstv_inb_out = "Outbound";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: psiv_lane_inb_out = " + psiv_lane_inb_out + " is not " + Intersection.LANE_INB + " or " + Intersection.LANE_OUT
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        lstv_name = lstv_name.replaceFirst("#", lstv_inb_out);
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Lane_Data.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if ((psiv_lane_inb_out == Intersection.LANE_OUT) && (lsiv_field == Intersection.TX_FMT_GDV_LANE_DATA_PCT)) {
                continue;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Lane_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for " + lstv_inb_out
                        + " lane " + psiv_lane + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Lane_Data.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for " + lstv_inb_out + " lane "
                        + psiv_lane + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg, // leg number
            int psiv_lane_inb_out, // LANE_INB or LANE_OUT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_approach;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_lines;
        String lstv_comment;
        String lstv_inb_out;
        String lstv_name;

        // printToFile prints all Lane_Data data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Lane_Data.printToFile psiv_leg=" + psiv_leg + " psiv_lane_inb_out=" + psiv_lane_inb_out + " psiv_lane=" + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        switch (psiv_lane_inb_out) {
            case Intersection.LANE_INB:
                // check if number of inbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // write leg 3 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    // write leg 6 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for printing data to a file
                lstv_inb_out = "Inbound";
                lsiv_lines = 9;
                if (Intersection.mbov_is_diamond_interchange) {
                    lsiv_approach = Intersection.msia_diamond_inb_appr_num[psiv_leg];
                }
                else {
                    lsiv_approach = psiv_leg;
                }
                break;

            case Intersection.LANE_OUT:
                // check if number of outbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // write leg 3 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    // write leg 6 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for printing data to a file
                lstv_inb_out = "Outbound";
                lsiv_lines = 8;
                if (Intersection.mbov_is_diamond_interchange) {
                    lsiv_approach = Intersection.msia_diamond_out_appr_num[psiv_leg];
                }
                else {
                    lsiv_approach = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + psiv_leg;
                }
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.printToFile: psiv_lane_inb_out = " + psiv_lane_inb_out + " is not " + Intersection.LANE_INB + " or " + Intersection.LANE_OUT + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        lstv_name = lstv_name.replaceFirst("#", lstv_inb_out);
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        if (psiv_lane == lsiv_lane_beg) {
            lstv_name = lstv_name.concat(" (median lane)");
        }
        if (psiv_lane == lsiv_lane_end) {
            lstv_name = lstv_name.concat(" (curb lane)");
        }
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Lane_Data.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(lsiv_lines);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(lsiv_approach, lstv_name, Intersection.TX_FMT_SIMPRO_APPR_NUM, Intersection.TX_FMT_SIMPRO_APPR_NUM_NUMBER, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_width, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_WIDTH, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mclv_mvmt.mstv_code, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_MVMT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_inr_opn, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_otr_opn, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_sl_off, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        if (psiv_lane_inb_out == Intersection.LANE_INB) {
            Intersection.filesPrintGDV_IntToFile(msiv_pct, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_PCT, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                    Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print comment to file
        if ((msiv_inr_opn == 0) && (msiv_otr_opn == 0)) {
            lstv_comment = "Lane open at all locations";
        }
        else if ((msiv_inr_opn > 0) && (msiv_otr_opn == 0)) {
            lstv_comment = "Lane open near intersection center and blocked on outer end";
        }
        else if ((msiv_inr_opn == 0) && (msiv_otr_opn > 0)) {
            lstv_comment = "Lane open on outer end and blocked near intersection center";
        }
        else {
            lstv_comment = "Lane open near intersection center, blocked in middle, and open on outer end";
        }
        Intersection.filesPrintGDV_CommentToFile(lstv_comment, Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg, // leg number
            int psiv_lane_inb_out, // LANE_INB or LANE_OUT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_inb_out;
        String lstv_name;

        // readFromCards reads all Lane_Data fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Lane_Data.readFromCards psiv_leg=" + psiv_leg + " psiv_lane_inb_out=" + psiv_lane_inb_out + " psiv_lane=" + psiv_lane);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set card number
        lsiv_card = -1;
        switch (psiv_leg) {
            case 1:
                // check if leg 1 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1];
                break;

            case 2:
                // check if leg 2 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2];
                break;

            case 3:
                // check if leg 3 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3];
                break;

            case 4:
                // check if leg 4 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4];
                break;

            case 5:
                // check if leg 5 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5];
                break;

            case 6:
                // check if leg 6 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }

        // check parameters and set card number
        switch (psiv_lane_inb_out) {
            case Intersection.LANE_INB:
                // check if number of inbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // read leg 3 lane control for free u-turn lane and store in inbound lane 0
                    // special code that must also be performed by the menu system
                    // read leg 6 lane control for free u-turn lane and store in inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lsiv_card += psiv_lane;
                lstv_inb_out = "Inbound";
                break;

            case Intersection.LANE_OUT:
                // check if number of outbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // read leg 3 lane control for free u-turn lane and store in inbound lane 0
                    // special code that must also be performed by the menu system
                    // read leg 6 lane control for free u-turn lane and store in inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + psiv_lane);
                lstv_inb_out = "Outbound";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.readFromCards: psiv_lane_inb_out = " + psiv_lane_inb_out + " is not " + Intersection.LANE_INB + " or " + Intersection.LANE_OUT
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        lstv_name = lstv_name.replaceFirst("#", lstv_inb_out);
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Lane_Data.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_width = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mclv_mvmt.mstv_code = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_MVMT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_inr_opn = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_otr_opn = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_sl_off = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_pct = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_PCT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set value to zero if outbound lane and input is non-zero
        if ((psiv_lane_inb_out == Intersection.LANE_OUT) && (msiv_pct != 0)) {
            msiv_pct = 0;
            mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_PCT] = Intersection.TX_SET_BY_SOFTWARE;
        }

        // read data from cards
        mstv_allowed_veh_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Lane_Data fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Lane_Data.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg, // leg number
            int psiv_lane_inb_out, // LANE_INB or LANE_OUT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_inb_out;
        String lstv_name;

        // writeToCards writes all Lane_Data fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Lane_Data.writeToCards psiv_leg=" + psiv_leg + " psiv_lane_inb_out=" + psiv_lane_inb_out + " psiv_lane=" + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set card number
        lsiv_card = -1;
        switch (psiv_leg) {
            case 1:
                // check if leg 1 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_1] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1];
                break;

            case 2:
                // check if leg 2 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_2] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2];
                break;

            case 3:
                // check if leg 3 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_3] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3];
                break;

            case 4:
                // check if leg 4 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_4] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4];
                break;

            case 5:
                // check if leg 5 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_5] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5];
                break;

            case 6:
                // check if leg 6 data card number is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_6] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }

        // check parameters and set card number
        switch (psiv_lane_inb_out) {
            case Intersection.LANE_INB:
                // check if number of inbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // write leg 3 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    // write leg 6 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lsiv_card += psiv_lane;
                lstv_inb_out = "Inbound";
                break;

            case Intersection.LANE_OUT:
                // check if number of outbound lanes is valid
                if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out;
                if ((Intersection.mbov_is_diamond_interchange)
                        && (Intersection.mbov_free_uturns_defined)
                        && (((psiv_leg == 3) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_1_LEG3_TO_LEG4].msiv_lane_width > 0))
                                || ((psiv_leg == 6) && (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[Intersection.DIAMOND_FREE_UTURN_2_LEG6_TO_LEG1].msiv_lane_width > 0)))) {
                    // write leg 3 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    // write leg 6 lane control for free u-turn lane from inbound lane 0
                    // special code that must also be performed by the menu system
                    lsiv_lane_beg = 0;
                }

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + psiv_lane);
                lstv_inb_out = "Outbound";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Lane_Data.writeToCards: psiv_lane_inb_out = " + psiv_lane_inb_out + " is not " + Intersection.LANE_INB + " or " + Intersection.LANE_OUT
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        lstv_name = lstv_name.replaceFirst("#", lstv_inb_out);
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Lane_Data.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_width, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mclv_mvmt.mstv_code, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_MVMT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_inr_opn, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_otr_opn, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_sl_off, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        if (psiv_lane_inb_out == Intersection.LANE_INB) {
            Intersection.writeIntToCard(msiv_pct, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_PCT, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }

        // write data to cards
        Intersection.writeStringToCard(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_LANE_DATA, Intersection.TX_FMT_GDV_LANE_DATA_ALLOW_VEH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Lane_Data

/******************************************************************************/
/* Lane_Data.java */
/******************************************************************************/
