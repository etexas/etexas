package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                               Lane_Cont.java                               */
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

class Lane_Cont {

    String mstv_cont; /* type of control for the inbound lane: */

    /* "BL" - blocked lane. lane ends before the intersection. */
    /* "UN" - uncontrolled. only inter cont = UNCONTRL, YIELD, or STOP */
    /* "YI" - yield sign. only intersection control = YIELD or signalized */
    /* "ST" - stop sign. only intersection control = STOP or ALL-STOP */
    /* "SI" - signal w/out left or right turn on red. signalized inter. */
    /* "LT" - signal with left turn on red. signalized intersection */
    /* "RT" - signal with right turn on red. signalized intersection */
    /* the following variables are defined but are not read or written */
    int msiv_x; /* lane control x coordinate */

    int msiv_y; /* lane control y coordinate */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_SIM_LANE_CONT]
    // mcla_leg[lsiv_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_0]
    // mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_1]
    // mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_2]
    // mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_3]
    // mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_4]
    // mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_5]
    // mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.mstv_cont
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_CONT_6]
    // mcla_leg[lsiv_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_0]
    // mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_1]
    // mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_2]
    // mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_3]
    // mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_4]
    // mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_5]
    // mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_x
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCX_6]
    // mcla_leg[lsiv_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_0]
    // mcla_leg[lsiv_leg].mcla_inb_lane[1].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_1]
    // mcla_leg[lsiv_leg].mcla_inb_lane[2].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_2]
    // mcla_leg[lsiv_leg].mcla_inb_lane[3].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_3]
    // mcla_leg[lsiv_leg].mcla_inb_lane[4].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_4]
    // mcla_leg[lsiv_leg].mcla_inb_lane[5].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_5]
    // mcla_leg[lsiv_leg].mcla_inb_lane[6].mclv_TX_Lane_Data.mclv_tx_l_c.msiv_y
    // .mclv_aux.msia_stat[TX_FMT_SIM_LANE_CONT_LOCY_6]

    public Lane_Cont() {
        mclv_aux = new TX_Aux();
    } // end of method Lane_Cont

    public void checkForErrors(int psiv_leg, // leg number
            int psiv_lane // inbound lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        String lstv_name;

        // checkForErrors checks all Lane_Cont data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Lane_Cont.checkForErrors psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.checkForErrors: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.checkForErrors: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
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
            Intersection.mstv_errorMessage = "Error in Lane_Cont.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_LANE_CONT];
        lsiv_field = Intersection.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + psiv_lane;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));

        /* debug */if (Intersection.mbov_debug_checkForErrorsSIM)
            System.out.println("Lane_Cont.checkForErrors checking " + lstv_name);

        // check all data for errors
        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
            Intersection.mstv_warningMessage = "Warning in Lane_Cont.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error.";
            Intersection.warningMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
            return;
        }

        if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg, // leg number
            int psiv_lane // inbound lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_name;

        // printToFile reads all Lane_Cont data to a file

        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Lane_Cont.printToFile psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        // special code that must also be performed by the menu system
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.printToFile: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
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
            Intersection.mstv_errorMessage = "Error in Lane_Cont.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for printing data to file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_LANE_CONT];
        lsiv_member = Intersection.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + psiv_lane;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printSIM)
            System.out.println("Lane_Cont.printToFile printing " + lstv_name);

        // print data to a file
        Intersection.filesPrintSIM_StringToFile(mstv_cont, lstv_name, Intersection.TX_FMT_SIM_LANE_CONT, lsiv_member, mclv_aux, Intersection.GDVSIM_PRINT_LEFT_MARGIN,
                Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN, Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg, // leg number
            int psiv_lane, // inbound lane number
            int psiv_col // column on card for start of lane control
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_name;

        // readFromCards reads all Lane_Cont fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Lane_Cont.readFromCards psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane + " psiv_col=" + psiv_col);
        // check if SIM data cards have been read
        if (Intersection.msiv_simdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: Intersection.msiv_simdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        // special code that must also be performed by the menu system
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
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
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((psiv_col < 1) || (psiv_col > (Intersection.TX_FMT_NC - 1))) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: psiv_col = " + psiv_col + " is < 1 or > " + (Intersection.TX_FMT_NC - 1) + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if title card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_SIM_TITLE_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for reading data from cards
        // Lane_Cont card is always after the SIM_Par_Opt_2 card after the SIM_Par_Opt card after
        // the SIM_Title card
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_LANE_CONT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_sim_title_card + 3;
        lsiv_member = Intersection.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + psiv_lane;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadSIM)
            System.out.println("Lane_Cont.readFromCards reading " + lstv_name);

        // save and overide column on card because all Lane_Cont values are on one card for all legs
        // for all inbound lanes
        lsiv_col = lclv_tx_fmt.msia_fo[lsiv_member];
        lclv_tx_fmt.msia_fo[lsiv_member] = psiv_col;

        // read data from cards
        mstv_cont = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_SIM_LANE_CONT, lsiv_member, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsRead, lsiv_card, mclv_aux,
                Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        // restore column on card
        lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // set lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
        if (psiv_lane == 0) {
            mstv_cont = "UN";
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Lane_Cont fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Lane_Cont.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg, // leg number
            int psiv_lane, // inbound lane number
            int psiv_col // column on card for start of lane control
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_col;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_beg;
        int lsiv_leg_end;
        int lsiv_member;
        String lstv_name;

        // writeToCards writes all Lane_Cont fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Lane_Cont.writeToCards psiv_leg=" + psiv_leg + " psiv_lane=" + psiv_lane + " psiv_col=" + psiv_col);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set beginning and ending leg number
        // special code that must also be performed by the menu system
        if (Intersection.mbov_is_diamond_interchange) {
            lsiv_leg_beg = 0;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        }
        else {
            lsiv_leg_beg = 1;
            lsiv_leg_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs;
        }

        // check parameters
        if ((psiv_leg < lsiv_leg_beg) || (psiv_leg > lsiv_leg_end)) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: psiv_leg = " + psiv_leg + " is < " + lsiv_leg_beg + " or > " + lsiv_leg_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
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
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        if ((psiv_col < 1) || (psiv_col > (Intersection.TX_FMT_NC - 1))) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: psiv_col = " + psiv_col + " is < 1 or > " + (Intersection.TX_FMT_NC - 1) + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if lane control card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.mclv_aux.msia_stat[Intersection.TX_FMT_SIM_HEADER_LANE_CONT_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Lane_Cont.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_lane_cont_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_SIM_LANE_CONT];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_sim_data.mclv_sim_header.msiv_lane_cont_card;
        lsiv_member = Intersection.TX_FMT_SIM_LANE_CONT_CONT_1 - 1 + psiv_lane;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteSIM)
            System.out.println("Lane_Cont.writeToCards writing " + lstv_name);

        // save and overide column on card because all Lane_Cont values are on one card for all legs
        // for all inbound lanes
        lsiv_col = lclv_tx_fmt.msia_fo[lsiv_member];
        lclv_tx_fmt.msia_fo[lsiv_member] = psiv_col;

        // set lane control "UN" for free u-turn in leg 3/4 or 6/1 lane 0
        if (psiv_lane == 0) {
            mstv_cont = "UN";
        }

        // write data to cards
        Intersection
                .writeStringToCard(mstv_cont, lstv_name, Intersection.TX_FMT_SIM_LANE_CONT, lsiv_member, Intersection.mcla_simdataCards, Intersection.msiv_simdataCardsWritten, lsiv_card, mclv_aux);
        // restore column on card
        lclv_tx_fmt.msia_fo[lsiv_member] = lsiv_col;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Lane_Cont

/******************************************************************************/
/* Lane_Cont.java */
/******************************************************************************/
