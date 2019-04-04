package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_DiamondLane.java                            */
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
import org.etexascode.texas.gdvsim.Mvmt_Code;
import org.etexascode.texas.gdvsim.TX_Aux;
import org.etexascode.texas.gdvsim.TX_Fmt;
import java.io.*;
import java.lang.*;
import java.util.*;

class GDV_DiamondLane {

    int msiv_int_lane_width; /* Internal lane width */

    Mvmt_Code mclv_mvmt_near_center_right; /*
                                            * Movement code at end near Right Intersection (L, LS,
                                            * or S)
                                            */

    Mvmt_Code mclv_mvmt_near_center_left; /*
                                           * Movement code at end near Left Intersection (L, LS, or
                                           * S)
                                           */

    int msiv_usable_from_center_right; /*
                                        * Length of usable lane from Right Intersection (0 for open
                                        * lane)
                                        */

    int msiv_usable_from_center_left; /*
                                       * Length of usable lane from Left Intersection (0 for open
                                       * lane)
                                       */

    int msiv_offset_near_center_right; /*
                                        * Offset of lane terminal near Right Intersection (+ is
                                        * toward Right Intersection)
                                        */

    int msiv_offset_near_center_left; /*
                                       * Offset of lane terminal near Left Intersection (+ is toward
                                       * Left Intersection )
                                       */

    String mstv_allowed_veh_type; /* Vehicle types allowed to use this lane */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_DIAMOND_LANE]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_int_lane_width
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_right.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mclv_mvmt_near_center_left.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_usable_from_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].msiv_offset_near_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_right[lsiv_lane].mstv_allowed_veh_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]

    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].msiv_int_lane_width .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].mclv_mvmt_near_center_right.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].mclv_mvmt_near_center_left.mstv_code
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].msiv_usable_from_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].msiv_usable_from_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].msiv_offset_near_center_right
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].msiv_offset_near_center_left
    // .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_diamond_lane_inb_left
    // [lsiv_lane].mstv_allowed_veh_type .mclv_aux.msia_stat[TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE]

    public GDV_DiamondLane() {
        mclv_mvmt_near_center_right = new Mvmt_Code();
        mclv_mvmt_near_center_left = new Mvmt_Code();
        mclv_aux = new TX_Aux();
    } // end of method GDV_DiamondLane

    public void checkForErrors(int psiv_lane_right_left, // LANE_CENTER_RIGHT or LANE_CENTER_LEFT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_name;
        String lstv_right_left;

        // checkForErrors checks all GDV_DiamondLane data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_DiamondLane.checkForErrors psiv_lane_right_left=" + psiv_lane_right_left + "="
                    + (psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT ? "LANE_CENTER_RIGHT" : "LANE_CENTER_LEFT") + " psiv_lane=" + psiv_lane);
        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                // check if diamond number of inbound lanes to Right Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for checking data for errors
                lstv_right_left = "Right";
                break;

            case Intersection.LANE_CENTER_LEFT:
                // check if diamond number of inbound lanes to Left Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for checking data for errors
                lstv_right_left = "Left";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: psiv_lane_right_left = " + psiv_lane_right_left + " is not " + Intersection.LANE_CENTER_RIGHT + " or "
                        + Intersection.LANE_CENTER_LEFT + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LANE];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        lstv_name = lstv_name.replaceFirst("#", lstv_right_left);
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_DiamondLane.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in GDV_DiamondLane.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for lane to center "
                        + lstv_right_left + " " + psiv_lane + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for lane to center "
                        + lstv_right_left + " " + psiv_lane + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_lane_right_left, // LANE_CENTER_RIGHT or LANE_CENTER_LEFT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_approach;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_right_left;
        String lstv_name;
        String lstv_val;

        // printToFile prints all GDV_DiamondLane data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_DiamondLane.printToFile psiv_lane_right_left=" + psiv_lane_right_left + "="
                    + (psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT ? "LANE_CENTER_RIGHT" : "LANE_CENTER_LEFT") + " psiv_lane=" + psiv_lane);
        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                // check if diamond number of inbound lanes to Right Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for printing data to a file
                lsiv_approach = Intersection.msia_diamond_inb_appr_num[0];
                lstv_right_left = "Right";
                break;

            case Intersection.LANE_CENTER_LEFT:
                // check if diamond number of inbound lanes to Left Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for printing data to a file
                lsiv_approach = Intersection.msia_diamond_inb_appr_num[7];
                lstv_right_left = "Left";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.printToFile: psiv_lane_right_left = " + psiv_lane_right_left + " is not " + Intersection.LANE_CENTER_RIGHT + " or "
                        + Intersection.LANE_CENTER_LEFT + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LANE];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        lstv_name = lstv_name.replaceFirst("#", lstv_right_left);
        if (psiv_lane == lsiv_lane_beg) {
            lstv_name = lstv_name.concat(" (median)");
        }
        if (psiv_lane == lsiv_lane_end) {
            lstv_name = lstv_name.concat(" (curb)");
        }
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_DiamondLane.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(11);
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
        Intersection.filesPrintGDV_IntToFile(msiv_int_lane_width, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        lstv_val = lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT];
        if ((psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT) && (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
            // there are no outbound lanes for diamond interchange leg 3 thus right turns are not
            // allowed
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = "|XX|L|LS|S|";
        }
        else {
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = "|XX|L|LS|LR|S|SR|R|";
        }
        Intersection.filesPrintGDV_StringToFile(mclv_mvmt_near_center_right.mstv_code, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = lstv_val;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        lstv_val = lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT];
        if ((psiv_lane_right_left == Intersection.LANE_CENTER_LEFT) && (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
            // there are no outbound lanes for diamond interchange leg 6 thus right turns are not
            // allowed
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = "|XX|L|LS|S|";
        }
        else {
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = "|XX|L|LS|LR|S|SR|R|";
        }
        Intersection.filesPrintGDV_StringToFile(mclv_mvmt_near_center_left.mstv_code, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = lstv_val;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_usable_from_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_usable_from_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_offset_near_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_IntToFile(msiv_offset_near_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        Intersection.filesPrintGDV_StringToFile(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE, mclv_aux,
                Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                Intersection.GDVSIM_ADD_TRAILING_DASHES);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_lane_right_left, // LANE_CENTER_RIGHT or LANE_CENTER_LEFT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        int lsiv_leg_cl;
        int lsiv_leg_cr;
        String lstv_right_left;
        String lstv_name;

        // readFromCards reads all GDV_DiamondLane fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLane.readFromCards psiv_lane_right_left=" + psiv_lane_right_left + "="
                    + (psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT ? "LANE_CENTER_RIGHT" : "LANE_CENTER_LEFT") + " psiv_lane=" + psiv_lane);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if leg 0 data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0] is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                // check if diamond number of inbound lanes to Right Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0]) + psiv_lane;
                lstv_right_left = "Right";
                break;

            case Intersection.LANE_CENTER_LEFT:
                // check if diamond number of inbound lanes to Left Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for reading data from cards
                lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0])
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right + psiv_lane;
                lstv_right_left = "Left";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.readFromCards: psiv_lane_right_left = " + psiv_lane_right_left + " is not " + Intersection.LANE_CENTER_RIGHT + " or "
                        + Intersection.LANE_CENTER_LEFT + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for reading data from cards
        lsiv_leg_cl = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + 1;
        lsiv_leg_cr = 0;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LANE];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        lstv_name = lstv_name.replaceFirst("#", lstv_right_left);
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_DiamondLane.readFromCards reading " + lstv_name);

        // read data from cards
        msiv_int_lane_width = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_int_lane_width);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_int_lane_width;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_int_lane_width);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_int_lane_width;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_int_lane_width);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_int_lane_width;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_int_lane_width);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_int_lane_width;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        mclv_mvmt_near_center_right.mstv_code = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"" + mclv_mvmt_near_center_right.mstv_code + "\"");
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = mclv_mvmt_near_center_right.mstv_code.toString();
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"" + mclv_mvmt_near_center_right.mstv_code + "\"");
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = mclv_mvmt_near_center_right.mstv_code.toString();
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        mclv_mvmt_near_center_left.mstv_code = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_DO_NOT_ALLOW_EMPTY,
                Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"" + mclv_mvmt_near_center_left.mstv_code + "\"");
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = mclv_mvmt_near_center_left.mstv_code.toString();
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"" + mclv_mvmt_near_center_left.mstv_code + "\"");
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = mclv_mvmt_near_center_left.mstv_code.toString();
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        msiv_usable_from_center_right = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_usable_from_center_right);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_usable_from_center_right;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=" + msiv_usable_from_center_right);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = msiv_usable_from_center_right;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=" + msiv_usable_from_center_right);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = msiv_usable_from_center_right;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_usable_from_center_right);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_usable_from_center_right;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        msiv_usable_from_center_left = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=" + msiv_usable_from_center_left);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = msiv_usable_from_center_left;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_usable_from_center_left);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_usable_from_center_left;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_usable_from_center_left);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_usable_from_center_left;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=" + msiv_usable_from_center_left);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = msiv_usable_from_center_left;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        msiv_offset_near_center_right = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=" + msiv_offset_near_center_right);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = msiv_offset_near_center_right;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=" + msiv_offset_near_center_right);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = msiv_offset_near_center_right;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        msiv_offset_near_center_left = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=" + msiv_offset_near_center_left);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = msiv_offset_near_center_left;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=" + msiv_offset_near_center_left);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = msiv_offset_near_center_left;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        // read data from cards
        mstv_allowed_veh_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for internal lane in leg to Right Intersection and leg to Left Intersection
        // special code that must also be performed by the menu system
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type=" + mstv_allowed_veh_type);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = mstv_allowed_veh_type;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type=" + mstv_allowed_veh_type);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = mstv_allowed_veh_type;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                break;

            case Intersection.LANE_CENTER_LEFT:
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cl Intersection.mcla_leg[" + lsiv_leg_cl + "].mcla_inb_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type=" + mstv_allowed_veh_type);
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = mstv_allowed_veh_type;
                Intersection.mcla_leg[lsiv_leg_cl].mcla_inb_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("GDV_DiamondLane.readFromCards setting leg cr Intersection.mcla_leg[" + lsiv_leg_cr + "].mcla_out_lane["
                            + psiv_lane + "].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type=" + mstv_allowed_veh_type);
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mstv_allowed_veh_type = mstv_allowed_veh_type;
                Intersection.mcla_leg[lsiv_leg_cr].mcla_out_lane[psiv_lane].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
                break;
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_DiamondLane fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_DiamondLane.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_lane_right_left, // LANE_CENTER_RIGHT or LANE_CENTER_LEFT
            int psiv_lane // lane number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_lane_beg;
        int lsiv_lane_end;
        String lstv_right_left;
        String lstv_name;
        String lstv_val;

        // writeToCards writes all GDV_DiamondLane fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_DiamondLane.writeToCards psiv_lane_right_left=" + psiv_lane_right_left + "="
                    + (psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT ? "LANE_CENTER_RIGHT" : "LANE_CENTER_LEFT") + " psiv_lane=" + psiv_lane);
        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if leg 0 data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0] is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        switch (psiv_lane_right_left) {
            case Intersection.LANE_CENTER_RIGHT:
                // check if diamond number of inbound lanes to Right Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_R] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0]) + psiv_lane;
                lstv_right_left = "Right";
                break;

            case Intersection.LANE_CENTER_LEFT:
                // check if diamond number of inbound lanes to Left Intersection is valid
                if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_DIAMOND_LEG_LANES_INB_L] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set beginning and ending lane number
                lsiv_lane_beg = 1;
                lsiv_lane_end = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_left;

                // check parameters
                if ((psiv_lane < lsiv_lane_beg) || (psiv_lane > lsiv_lane_end)) {
                    Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: psiv_lane = " + psiv_lane + " is < " + lsiv_lane_beg + " or > " + lsiv_lane_end + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }

                // set local data for writing data to cards
                lsiv_card = Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0])
                        + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_diamond_leg.msiv_lanes_inb_to_center_right + psiv_lane;
                lstv_right_left = "Left";
                break;

            default:
                Intersection.mstv_errorMessage = "Error in GDV_DiamondLane.writeToCards: psiv_lane_right_left = " + psiv_lane_right_left + " is not " + Intersection.LANE_CENTER_RIGHT + " or "
                        + Intersection.LANE_CENTER_LEFT + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_DIAMOND_LANE];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_lane));
        lstv_name = lstv_name.replaceFirst("#", lstv_right_left);
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_DiamondLane.writeToCards writing " + lstv_name);

        // write data to cards
        Intersection.writeIntToCard(msiv_int_lane_width, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_LANE_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        lstv_val = lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT];
        if ((psiv_lane_right_left == Intersection.LANE_CENTER_RIGHT) && (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
            // there are no outbound lanes for diamond interchange leg 3 thus right turns are not
            // allowed
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = "|XX|L|LS|S|";
        }
        else {
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = "|XX|L|LS|LR|S|SR|R|";
        }
        Intersection.writeStringToCard(mclv_mvmt_near_center_right.mstv_code, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_RIGHT] = lstv_val;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        lstv_val = lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT];
        if ((psiv_lane_right_left == Intersection.LANE_CENTER_LEFT) && (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out == 0)) {
            // there are no outbound lanes for diamond interchange leg 6 thus right turns are not
            // allowed
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = "|XX|L|LS|S|";
        }
        else {
            lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = "|XX|L|LS|LR|S|SR|R|";
        }
        Intersection.writeStringToCard(mclv_mvmt_near_center_left.mstv_code, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT,
                Intersection.mcla_gdvdataCards, Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        lclv_tx_fmt.msta_val[Intersection.TX_FMT_GDV_DIAMOND_LANE_MVMT_LEFT] = lstv_val;
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_usable_from_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_RIGHT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_usable_from_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_USABLE_LEFT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_offset_near_center_right, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_RIGHT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_offset_near_center_left, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_OFFSET_LEFT, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_DIAMOND_LANE, Intersection.TX_FMT_GDV_DIAMOND_LANE_ALLOWVEHTYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_DiamondLane

/******************************************************************************/
/* GDV_DiamondLane.java */
/******************************************************************************/
