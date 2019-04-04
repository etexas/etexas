package org.etexascode.texas.gdvsim;

/******************************************************************************/
/*                                gdvsim.java                                 */
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

class Traf_Dest /* traffic destination data */
{

    int msia_dest_per[]; /*
                          * outbound traffic destination percentages by leg [1] is for leg 1
                          */

    int msia_TMC[]; /* Turn Movement Count */

    TX_Aux mclv_aux; /* Auxiliary data */

    // typical access examples
    // mclv_tx_mdl_formats.mcla_tx_fmt[TX_FMT_GDV_TRAF_DEST]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_1]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_2]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_3]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_4]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_5]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_PER_6]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [1]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_1]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [2]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_2]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [3]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_3]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [4]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_4]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [5]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_5]
    // mcla_leg[lsiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_TMC [6]
    // .mclv_aux.msia_stat[TX_FMT_GDV_TRAF_DEST_DEST_TMC_6]

    public Traf_Dest() {
        msia_dest_per = new int[PARAMS.TEXAS_MODEL_NLGP1];
        msia_TMC = new int[PARAMS.TEXAS_MODEL_NLGP1];
        mclv_aux = new TX_Aux();
    } // end of method Traf_Dest

    public void checkForErrors(int psiv_leg // leg number
    ) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_field;
        String lstv_name;

        // checkForErrors checks all Traf_Dest data for invalid data and data with errors
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Dest.checkForErrors psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.checkForErrors: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.checkForErrors: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for checking data for errors
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_checkForErrorsGDV)
            System.out.println("Traf_Dest.checkForErrors checking " + lstv_name);

        // check all data for errors
        for (lsiv_field = 1; lsiv_field <= lclv_tx_fmt.msiv_nf; lsiv_field++) {
            // do not check legs beyond msiv_no_legs
            if (lsiv_field > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)
                break;

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_ERROR) {
                Intersection.mstv_warningMessage = "Warning in Traf_Dest.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data has an error for leg " + psiv_leg + ".";
                Intersection.warningMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_NON_FATAL_ERROR;
                return;
            }

            if (mclv_aux.msia_stat[lsiv_field] == Intersection.TX_DATA_IS_INVALID) {
                Intersection.mstv_errorMessage = "Error in Traf_Dest.checkForErrors: " + lstv_name + " - " + lclv_tx_fmt.msta_desc[lsiv_field] + " - data is invalid for leg " + psiv_leg + ".";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                return;
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method checkForErrors

    public void printToFile(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_leg;
        int lsiv_sum_dest_per;
        String lstv_name;

        // printToFile prints all Traf_Dest data to a file

        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Dest.printToFile psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.printToFile: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.printToFile: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.printToFile: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // set local data for print data to a file
        lsiv_sum_dest_per = 0;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_printGDV)
            System.out.println("Traf_Dest.printToFile printing " + lstv_name);

        // go to a new page if there is not enough room on the current page
        Intersection.filesPrintGDV_check_newpage(Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print heading to a file
        Intersection.filesPrintGDV_HeadingToFile(lstv_name, Intersection.GDVSIM_PRINT_HEADING_MARGIN, Intersection.GDVSIM_PRINT_RIGHT_MARGIN);
        if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
            return;

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 1) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[1], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[1];
            }
        }

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 2) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[2], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_2, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[2];
            }
        }

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 3) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[3], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_3, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[3];
            }
        }

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[4], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_4, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[4];
            }
        }

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[5], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_5, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[5];
            }
        }

        // print data to a file
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            Intersection.filesPrintGDV_IntToFile(msia_dest_per[6], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_6, mclv_aux,
                    Intersection.GDVSIM_PRINT_LEFT_MARGIN, Intersection.GDVSIM_PRINT_DATA_INDENT, Intersection.GDVSIM_PRINT_DATA_SIZE, Intersection.GDVSIM_PRINT_RIGHT_MARGIN,
                    Intersection.GDVSIM_ADD_TRAILING_DASHES);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[6];
            }
        }

        // if sum of percent of destination traffic from this leg is not 100 then warning
        // special code that must also be performed by the menu system
        if (lsiv_sum_dest_per != 100) {
            Intersection.mstv_warningMessage = "Warning in Traf_Dest.printToFile: Sum of percent of destination traffic from Leg " + psiv_leg + " = " + lsiv_sum_dest_per + " is not 100.";
            Intersection.warningMessage();
            /* debug */lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
            for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_printGDV)
                    System.out.println("Traf_Dest.printToFile " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] + "["
                            + psiv_leg + "]" + "=" + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method printToFile

    public void readFromCards(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_leg;
        int lsiv_sum_dest_per;
        String lstv_name;

        // readFromCards reads all Traf_Dest fields from cards

        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Dest.readFromCards psiv_leg=" + psiv_leg);
        // check if GDV data cards have been read
        if (Intersection.msiv_gdvdataCardsRead == 0) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.msiv_gdvdataCardsRead = 0.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for reading data from cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Dest.readFromCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15) + 1);

        // set local data for reading data from cards
        lsiv_sum_dest_per = 0;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesReadGDV)
            System.out.println("Traf_Dest.readFromCards reading " + lstv_name);

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 1) {
            msia_dest_per[1] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[1];
        }

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 2) {
            msia_dest_per[2] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_2, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[2];
        }

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 3) {
            msia_dest_per[3] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_3, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[3];
        }

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            msia_dest_per[4] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_4, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[4];
        }

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            msia_dest_per[5] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_5, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[5];
        }

        // read data from cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            msia_dest_per[6] = Intersection.readIntFromCard(lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_6, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsRead, lsiv_card, mclv_aux, Intersection.GDVSIM_DO_NOT_FORCE_DEFAULT);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            lsiv_sum_dest_per += msia_dest_per[6];
        }

        // if sum of percent of destination traffic from this leg is not 100 then warning
        // special code that must also be performed by the menu system
        if (lsiv_sum_dest_per != 100) {
            Intersection.mstv_warningMessage = "Warning in Traf_Dest.readFromCards: Sum of percent of destination traffic from this Leg " + psiv_leg + " = " + lsiv_sum_dest_per + " is not 100.";
            Intersection.warningMessage();
            /* debug */lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
            for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_filesReadGDV)
                    System.out.println("Traf_Dest.readFromCards " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]
                            + "[" + psiv_leg + "]" + "=" + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method readFromCards

    public void setAllInvalid() {
        // setAllInvalid sets all Traf_Dest fields invalid

        /* debug */if (Intersection.mbov_debug_setAllInvalid)
            System.out.println("Traf_Dest.setAllInvalid");
        mclv_aux.setAllInvalid();
        return;
    } // end of method setAllInvalid

    public void writeToCards(int psiv_leg) {
        TX_Fmt lclv_tx_fmt = null;
        int lsiv_card;
        int lsiv_leg;
        int lsiv_sum_dest_per;
        String lstv_name;

        // writeToCards writes all Traf_Dest fields to cards

        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Dest.writeToCards psiv_leg=" + psiv_leg);
        // check if number of legs is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_LEGS] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check parameters
        if ((psiv_leg < 1) || (psiv_leg > Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs)) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > "
                    + Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs + ".";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of vehicle classes is valid
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_PAR_OPT_NO_VEH_CL] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of inbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_INB] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb is invalid.";
            Intersection.errorMessage();
            Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
            return;
        }

        // check if number of outbound lanes is valid
        if (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.mclv_aux.msia_stat[Intersection.TX_FMT_GDV_LEG_GEO_NO_OUT] == Intersection.TX_DATA_IS_INVALID) {
            Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mcla_leg[" + psiv_leg + "].mclv_TX_Leg_Data.mclv_geo.msiv_no_out is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[1] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[2] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[3] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[4] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[5] is invalid.";
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
                    Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6] is invalid.";
                    Intersection.errorMessage();
                    Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                    return;
                }
                // set local data for writing data to cards
                lsiv_card = Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_header.msia_leg_data_card[6];
                break;

            default:
                Intersection.mstv_errorMessage = "Error in Traf_Dest.writeToCards: psiv_leg = " + psiv_leg + " is < 1 or > 6.";
                Intersection.errorMessage();
                Intersection.msiv_returnCode = Intersection.RETURN_FATAL_ERROR;
                break;
        }
        lsiv_card += (Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_inb + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_geo.msiv_no_out + 1
                + ((Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_veh_cl + 14) / 15) + 1);

        // set local data for writing data to cards
        lsiv_sum_dest_per = 0;
        lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
        lstv_name = lclv_tx_fmt.mstv_name.replaceFirst("#", Integer.toString(psiv_leg));
        /* debug */if (Intersection.mbov_debug_filesWriteGDV)
            System.out.println("Traf_Dest.writeToCards writing " + lstv_name);

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 1) {
            Intersection.writeIntToCard(msia_dest_per[1], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[1].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[1];
            }
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 2) {
            Intersection.writeIntToCard(msia_dest_per[2], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_2, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[2].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[2];
            }
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 3) {
            Intersection.writeIntToCard(msia_dest_per[3], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_3, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[3].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[3];
            }
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 4) {
            Intersection.writeIntToCard(msia_dest_per[4], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_4, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[4].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[4];
            }
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 5) {
            Intersection.writeIntToCard(msia_dest_per[5], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_5, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[5].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[5];
            }
        }

        // write data to cards
        if (Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs >= 6) {
            Intersection.writeIntToCard(msia_dest_per[6], lstv_name, Intersection.TX_FMT_GDV_TRAF_DEST, Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_6, Intersection.mcla_gdvdataCards,
                    Intersection.msiv_gdvdataCardsWritten, lsiv_card, mclv_aux);
            if (Intersection.msiv_returnCode != Intersection.RETURN_SUCCESS)
                return;
            if (Intersection.mcla_leg[6].mclv_TX_Leg_Data.mclv_geo.msiv_no_out > 0) {
                lsiv_sum_dest_per += msia_dest_per[6];
            }
        }

        // if sum of percent of destination traffic from this leg is not 100 then warning
        // special code that must also be performed by the menu system
        if (lsiv_sum_dest_per != 100) {
            Intersection.mstv_warningMessage = "Warning in Traf_Dest.writeToCards: Sum of percent of destination traffic from this Leg " + psiv_leg + " = " + lsiv_sum_dest_per + " is not 100.";
            Intersection.warningMessage();
            /* debug */lclv_tx_fmt = Intersection.mclv_tx_mdl_formats.mcla_tx_fmt[Intersection.TX_FMT_GDV_TRAF_DEST];
            for (lsiv_leg = 1; lsiv_leg <= Intersection.mclv_TX_Inter_Data.mclv_tx_gdv_data.mclv_gdv_par_opt.msiv_no_legs; lsiv_leg++) {
                mclv_aux.msia_stat[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg] = Intersection.TX_DATA_ERROR;
                /* debug */if (Intersection.mbov_debug_filesWriteGDV)
                    System.out.println("Traf_Dest.writeToCards " + lclv_tx_fmt.msta_desc[Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg]
                            + "[" + psiv_leg + "]" + "=" + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.msia_dest_per[lsiv_leg] + "  "
                            + Intersection.mcla_leg[psiv_leg].mclv_TX_Leg_Data.mclv_dest.mclv_aux.status(Intersection.TX_FMT_GDV_TRAF_DEST_DEST_PER_1 - 1 + lsiv_leg) + " TX_DATA_ERROR");
            }
        }

        Intersection.msiv_returnCode = Intersection.RETURN_SUCCESS;
        return;
    } // end of method writeToCards
} // end of class Traf_Dest

/******************************************************************************/
/* gdvsim.java */
/******************************************************************************/
