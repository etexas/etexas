package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                            GDV_FreeUTurns.java                             */
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

class GDV_FreeUTurns {

    int msiv_lane_width; /* Lane width (0=no free u-turn lane) */

    int msiv_space_between; /*
                             * Space between outer internal lane and free u-turn lane
                             */

    int msiv_entr_length; /* Entrance lane length */

    int msiv_entr_radius; /* Entrance lane radius */

    int msiv_exit_length; /* Exit lane length */

    int msiv_exit_radius; /* Exit lane radius */

    int msiv_percent_using; /*
                             * Percent of u-turning traffic to use the free u-turn
                             */

    String mstv_allowed_veh_type; /* Vehicle types allowed to use this lane */

    TX_Aux mclv_aux; /* Auxiliary data */

    /* free u-turn 1 connects leg 3 inbound and leg 4 outbound */
    /* free u-turn 2 connects leg 6 inbound and leg 1 outbound */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_FREE_UTURN]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_lane_width
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_LANE_WIDTH ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_space_between
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_entr_length
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_entr_radius
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_exit_length
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_exit_radius
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].msiv_percent_using
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_PERCENT_USING ]
    // mclv_TX_Inter_Data.mclv_tx_gdv_data.mcla_gdv_free_uturn[lsiv_fut].mstv_allowed_veh_type
    // .mclv_aux.msia_stat[TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE]

    public GDV_FreeUTurns() {
        mclv_aux = new TX_Aux();
    } // end of method GDV_FreeUTurns

    public void checkForErrors(int psiv_fut // free u-turn card number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        int lsiv_inb_leg;
        int lsiv_out_leg;
        String lstv_name;

        // checkForErrors checks all GDV_FreeUTurns data for invalid data and data with errors

        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("GDV_FreeUTurns.checkForErrors psiv_fut=" + psiv_fut);
        // check parameters
        if ((psiv_fut < 1) || (psiv_fut > PARAMS.TEXAS_MODEL_NFU)) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.checkForErrors: psiv_fut = " + psiv_fut + " is < 1 or > " + PARAMS.TEXAS_MODEL_NFU + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.checkForErrors: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set inbound and outbound leg number for free u-turn
        // special code that must also be performed by the menu system
        if (psiv_fut == 1) {
            // free u-turn 1 connects leg 3 inbound and leg 4 outbound
            lsiv_inb_leg = 3;
            lsiv_out_leg = 4;
        }
        else {
            // free u-turn 2 connects leg 6 inbound and leg 1 outbound
            lsiv_inb_leg = 6;
            lsiv_out_leg = 1;
        }

        // check if a free u-turns defined
        if (Intersection.mbov_free_uturns_defined) {
            // set local data for checking data for errors
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_FREE_UTURN];
            lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_fut));
            lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_inb_leg));
            lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_out_leg));
            /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
                System.out.println("GDV_FreeUTurns.checkForErrors checking " + lstv_name);

            // check all data for errors
            for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                    Intersection.mstv_warningMessage = "Warning in GDV_FreeUTurns.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for free u-turn "
                            + psiv_fut + ".";
                    Intersection.warningMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                    return;
                }

                if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                    Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for free u-turn "
                            + psiv_fut + ".";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_fut // free u-turn card number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_inb_leg;
        int lsiv_out_leg;
        String lstv_name;

        // printToFile prints all GDV_FreeUTurns data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_FreeUTurns.printToFile psiv_fut=" + psiv_fut);
        // check parameters
        if ((psiv_fut < 1) || (psiv_fut > PARAMS.TEXAS_MODEL_NFU)) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.printToFile: psiv_fut = " + psiv_fut + " is < 1 or > " + PARAMS.TEXAS_MODEL_NFU + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.printToFile: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set inbound and outbound leg number for free u-turn
        // special code that must also be performed by the menu system
        if (psiv_fut == 1) {
            // free u-turn 1 connects leg 3 inbound and leg 4 outbound
            lsiv_inb_leg = 3;
            lsiv_out_leg = 4;
        }
        else {
            // free u-turn 2 connects leg 6 inbound and leg 1 outbound
            lsiv_inb_leg = 6;
            lsiv_out_leg = 1;
        }

        // set local data for printing data to a file
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_FREE_UTURN];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_fut));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_inb_leg));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_out_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("GDV_FreeUTurns.printToFile printing " + lstv_name);

        if (msiv_lane_width == 0) {
            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintGDV_check_newpage(1);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            Intersection.filesPrintGDV_CommentToFile("There is no Diamond Interchange Free U-Turn " + psiv_fut + " Data",
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN + Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        }
        else {
            // go to a new page if there is not enough room on the current page
            Intersection.filesPrintGDV_check_newpage(8);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print heading to a file
            Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // check lane width range for a normal lane
            // special code that must also be performed by the menu system
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
            if ((msiv_lane_width < lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]) || (msiv_lane_width > lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH])) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] = Intersection.TX_DATA_IS_INVALID;
                Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.printToFile: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] + " = " + msiv_lane_width
                        + " is < " + lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] + " or > " + lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_lane_width, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_space_between, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_entr_length, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_entr_radius, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_exit_length, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_exit_radius, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_IntToFile(msiv_percent_using, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_PERCENT_USING, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;

            // print data to a file
            Intersection.filesPrintGDV_StringToFile(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
        } // end if ( msiv_lane_width == 0 )

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_fut // free u-turn card number
    ) {
        boolean lbov_forceDefault;
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_inb_leg;
        int lsiv_out_leg;
        String lstv_name;

        // readFromCards reads all GDV_FreeUTurns fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards psiv_fut=" + psiv_fut);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_fut < 1) || (psiv_fut > PARAMS.TEXAS_MODEL_NFU)) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: psiv_fut = " + psiv_fut + " is < 1 or > " + PARAMS.TEXAS_MODEL_NFU + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if curb return card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if leg 0 data card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_LEG_DATA_CARD_0] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0] is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set inbound and outbound leg number for free u-turn
        // special code that must also be performed by the menu system
        if (psiv_fut == 1) {
            // free u-turn 1 connects leg 3 inbound and leg 4 outbound
            lsiv_inb_leg = 3;
            lsiv_out_leg = 4;
        }
        else {
            // free u-turn 2 connects leg 6 inbound and leg 1 outbound
            lsiv_inb_leg = 6;
            lsiv_out_leg = 1;
        }

        // set local data for reading data from cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_FREE_UTURN];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card + psiv_fut;
        lbov_forceDefault = ((Math.abs(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[0])
                - Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card) < 3);
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_fut));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_inb_leg));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_out_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards reading " + lstv_name);

        // store movement code "U" for free u-turn in leg 3/4 or 6/1 lane 0
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                    + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"U\"");
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                    + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code=\"U\"");
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_mvmt.mstv_code = "U";
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_MVMT] = Intersection.TX_SET_BY_SOFTWARE;

        // store stop line offset 0 for free u-turn in leg 3/4 or 6/1 lane 0
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                    + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=0");
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                    + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off=0");
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_sl_off = 0;
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_SL_OFF] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_lane_width = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for free u-turn in leg 3/4 or 6/1 lane 0
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                    + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_lane_width);
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_lane_width;
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                    + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width=" + msiv_lane_width);
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_width = msiv_lane_width;
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_space_between = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_entr_length = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for free u-turn in leg 3/4 or 6/1 lane 0
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                    + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_entr_length);
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_entr_length;
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting inb leg Intersection.mcla_leg[" + lsiv_inb_leg
                    + "].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=0");
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        Intersection.mcla_leg[lsiv_inb_leg].mcla_inb_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_entr_radius = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_exit_length = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // store value for free u-turn in leg 3/4 or 6/1 lane 0
        // special code that must also be performed by the menu system
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                    + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn=" + msiv_exit_length);
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_inr_opn = msiv_exit_length;
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_INR_OPN] = Intersection.TX_SET_BY_SOFTWARE;
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("GDV_FreeUTurns.readFromCards setting out leg Intersection.mcla_leg[" + lsiv_out_leg
                    + "].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn=0");
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.msiv_otr_opn = 0;
        Intersection.mcla_leg[lsiv_out_leg].mcla_out_lane[0].mclv_TX_Lane_Data.mclv_tx_l_g.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LANE_DATA_OTR_OPN] = Intersection.TX_SET_BY_SOFTWARE;

        // read data from cards
        msiv_exit_radius = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        msiv_percent_using = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_PERCENT_USING, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // read data from cards
        mstv_allowed_veh_type = Intersection.readStringFromCard(lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, lbov_forceDefault, Intersection.GDVSIM_ALLOW_EMPTY, Intersection.GDVSIM_TRIM_VALUE);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // if lane width > 0 then check lane width range for a normal lane
        // special code that must also be performed by the menu system
        if (msiv_lane_width > 0) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
            if ((msiv_lane_width < lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]) || (msiv_lane_width > lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH])) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] = Intersection.TX_DATA_IS_INVALID;
                Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.readFromCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]
                        + " = " + msiv_lane_width + " is < " + lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] + " or > " + lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all GDV_FreeUTurns fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("GDV_FreeUTurns.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_fut // free u-turn card number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_inb_leg;
        int lsiv_out_leg;
        String lstv_name;

        // writeToCards writes all GDV_FreeUTurns fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_FreeUTurns.writeToCards psiv_fut=" + psiv_fut);
        // check parameters
        if ((psiv_fut < 1) || (psiv_fut > PARAMS.TEXAS_MODEL_NFU)) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.writeToCards: psiv_fut = " + psiv_fut + " is < 1 or > " + PARAMS.TEXAS_MODEL_NFU + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if curb return card number is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_HEADER_CURB_RETURN_CARD] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if a diamond interchange
        if (!Intersection.mbov_is_diamond_interchange) {
            Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.writeToCards: Intersection is not a diamond interchange.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set inbound and outbound leg number for free u-turn
        // special code that must also be performed by the menu system
        if (psiv_fut == 1) {
            // free u-turn 1 connects leg 3 inbound and leg 4 outbound
            lsiv_inb_leg = 3;
            lsiv_out_leg = 4;
        }
        else {
            // free u-turn 2 connects leg 6 inbound and leg 1 outbound
            lsiv_inb_leg = 6;
            lsiv_out_leg = 1;
        }

        // set local data for writing data to cards
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_FREE_UTURN];
        lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msiv_curb_return_card + psiv_fut;
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_fut));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_inb_leg));
        lstv_name = lstv_name.replaceFirst("#", Integer.toString(lsiv_out_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("GDV_FreeUTurns.writeToCards writing " + lstv_name);

        // if lane width > 0 then check lane width range for a normal lane
        // special code that must also be performed by the menu system
        if (msiv_lane_width > 0) {
            lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_LANE_DATA];
            if ((msiv_lane_width < lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]) || (msiv_lane_width > lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH])) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH] = Intersection.TX_DATA_IS_INVALID;
                Intersection.mstv_errorMessage = "Error in GDV_FreeUTurns.writeToCards: Card " + lsiv_card + " " + lstv_name + " - " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]
                        + " = " + msiv_lane_width + " is < " + lclv_tx_fmt.msia_min[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH] + " or > " + lclv_tx_fmt.msia_max[Intersection.TX_FMT_GDV_LANE_DATA_WIDTH]
                        + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        // write data to cards
        Intersection.writeIntToCard(msiv_lane_width, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_LANE_WIDTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_space_between, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_SPACE_BETWEEN, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_entr_length, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_LENGTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_entr_radius, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ENTR_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_exit_length, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_LENGTH, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_exit_radius, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_EXIT_RADIUS, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeIntToCard(msiv_percent_using, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_PERCENT_USING, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // write data to cards
        Intersection.writeStringToCard(mstv_allowed_veh_type, lstv_name, Intersection.TX_FMT_GDV_FREE_UTURN, Intersection.TX_FMT_GDV_FREE_UTURN_ALLOW_VEH_TYPE, Intersection.mcla_gdvdataCards,
                Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class GDV_FreeUTurns

/******************************************************************************/
/* GDV_FreeUTurns.java */
/******************************************************************************/
